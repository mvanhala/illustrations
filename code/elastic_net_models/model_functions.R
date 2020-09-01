
h2o_frame_to_df <- function(x, as_tibble = FALSE) {
  # based on h2o::as.data.frame.H2OFrame
  # fix for 1-column numeric data frame
  checkmate::assert_class(x, "H2OFrame")
  
  colClasses <- attr(x, "types")
  colClasses <- gsub("numeric", "numeric", colClasses)
  colClasses <- gsub("int", "integer", colClasses)
  colClasses <- gsub("real", "numeric", colClasses)
  colClasses <- gsub("enum", "factor", colClasses)
  colClasses <- gsub("uuid", "character", colClasses)
  colClasses <- gsub("string", "character", colClasses)
  colClasses <- gsub("time", NA, colClasses)
  
  dates <- attr(x, "types") %in% "time"
  nCol <- attr(x, "ncol")
  nRow <- attr(x, "nrow")
  if (nCol > .Machine$integer.max || nRow > .Machine$integer.max) {
    stop(glue::glue(
      "It is not possible convert H2OFrame to data.frame/data.table. ",
      "The H2OFrame is bigger than vector size limit for R."
    ))
  }
  
  useHexString <- getRversion() >= "3.1"
  urlSuffix <- paste0(
    "DownloadDataset", 
    "?frame_id=",
    URLencode(h2o::h2o.getId(x)), 
    "&hex_string=", 
    as.numeric(useHexString)
  )
  
  payload <- h2o:::.h2o.doSafeGET(urlSuffix = urlSuffix, binary = TRUE)
  
  maxPayloadSize <- getOption(
    "h2o.as.data.frame.max.in-memory.payload.size", 
    .Machine$integer.max
  )
  
  if (length(payload) < maxPayloadSize) {
    ttt <- rawToChar(payload)
  } else {
    ttt <- h2o:::.writeBinToTmpFile(payload)
    on.exit(try(fs::file_delete(ttt)), add = TRUE)
  }
  
  df <- data.table::fread(
    ttt, 
    na.strings = "", 
    colClasses = colClasses, 
    showProgress = FALSE
  )
  
  for (i in which(dates)) data.table::setattr(df[[i]], "class", "POSIXct")
  
  if (as_tibble) df <- tibble::as_tibble(df)
  df
}


h2o_import_fast <- function(data) {
  tmp <- fs::file_temp(ext = ".csv")
  on.exit(try(fs::file_delete(tmp)), add = TRUE)
  types_h2o <- purrr::map_chr(
    data,
    ~if (rlang::is_bare_numeric(.)) {
      "numeric"
    } else if (rlang::is_bare_logical(.)) {
      "enum"
    } else if (inherits(., "factor")) {
      "enum"
    } else {
      "string"
    }
  )
  data.table::fwrite(mutate_if(data, is.factor, as.character), tmp)
  h2o_raw <- h2o::h2o.importFile(tmp, parse = FALSE)
  h2o::h2o.parseRaw(h2o_raw, sep = ",", col.types = types_h2o, blocking = TRUE)
}



get_model_output <- function(h2o_model) {
  metrics <- h2o::h2o.getFrame(
    h2o_model@model[["cross_validation_holdout_predictions_frame_id"]][["name"]]
  ) %>%
    h2o_frame_to_df(as_tibble = TRUE) %>%
    mutate(
      wgt = data_model[[.params$wgt_var]], 
      tgt = data_model[[.params$tgt_var]], 
      loss = data_model[[".loss"]]
    ) %>%
    calc_metrics("predict", "loss", "wgt")
  
  coef <- as_tibble(h2o_model@model$coefficients_table)
  
  list(metrics = metrics, coef = coef)
}


fit_model_grid_search <- function(alpha, 
                                  log_lambda, 
                                  data_h2o,
                                  predictors,
                                  tgt_var,
                                  wgt_var,
                                  fit_args,
                                  fold_col = ".fold") {
  message(
    glue::glue(
      "Run alpha {alpha}, ",
      "lambda {round(exp(log_lambda), 4)}, ",
      'time {strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")}'
    )
  )
  
  args <- rutils::list_edit(
    list(
      x = predictors,
      y = tgt_var,
      training_frame = data_h2o,
      weights_column = wgt_var,
      alpha = alpha,
      lambda = exp(log_lambda),
      fold_column = fold_col,
      keep_cross_validation_predictions = TRUE
    ),
    !!!fit_args
  ) 
  
  purrr::lift_dl(h2o::h2o.glm)(args)
}

compute_grid_search_model <- function(alpha, 
                                      log_lambda, 
                                      data_h2o,
                                      predictors,
                                      tgt_var,
                                      wgt_var,
                                      fit_args,
                                      fold_col = ".fold") {
  model <- fit_model_grid_search(
    alpha = alpha,
    log_lambda = log_lambda,
    data_h2o = data_h2o,
    predictors = predictors,
    tgt_var = tgt_var,
    wgt_var = wgt_var,
    fit_args = fit_args,
    fold_col = fold_col
  )
  
  res <- get_model_output(model)
  h2o::h2o.rm(model)
  res
}






calc_lorenz_curve <- function(data, pred_col, tgt_x_wgt_col, wgt_col) {
  pred_col <- sym(pred_col)
  tgt_x_wgt_col <- sym(tgt_x_wgt_col)
  wgt_col <- sym(wgt_col)
  
  by_pred <- data %>%
    dtplyr::lazy_dt() %>%
    group_by(!!pred_col) %>%
    summarise(.wgt = sum(!!wgt_col), .tgt_x_wgt = sum(!!tgt_x_wgt_col)) %>%
    arrange(desc(!!pred_col)) %>%
    mutate(
      cum_pct_wgt = cumsum(.wgt) / sum(.wgt),
      cum_pct_tgt = cumsum(.tgt_x_wgt) / sum(.tgt_x_wgt)
    ) %>%
    select(cum_pct_wgt, cum_pct_tgt) %>%
    data.table::as.data.table()
  
  initial_row <- data.table::data.table(cum_pct_wgt = 0, cum_pct_tgt = 0)
  
  dplyr::bind_rows(initial_row, by_pred)
}


calc_lorenz_auc <- function(curve, tgt_col = "cum_pct_tgt", wgt_col = "cum_pct_wgt") {
  tgt_col <- sym(tgt_col)
  wgt_col <- sym(wgt_col)
  curve %>%
    dtplyr::lazy_dt() %>%
    dplyr::mutate(
      xdiff = !!wgt_col - dplyr::lag(!!wgt_col, default = 0),
      area = xdiff * (!!tgt_col + dplyr::lag(!!tgt_col, default = 0))/2
    ) %>% 
    summarise(auc = sum(area)) %>%
    as_tibble() %>%
    pull(auc)
}

calc_prem_dist_curve <- function(data, pred_col, tgt_col, wgt_col) {
  initial_row <- data.table::data.table(cum_pct_pred = 0, cum_pct_tgt = 0)
  
  curve <- data %>%
    as_tibble() %>%
    select(tgt = !!sym(tgt_col), pred = !!sym(pred_col), wgt = !!sym(wgt_col)) %>%
    arrange(pred) %>%
    transmute(
      cum_pct_pred = cumsum(wgt * pred) / sum(wgt * pred), 
      cum_pct_tgt = cumsum(tgt) / sum(tgt)
    )
  bind_rows(initial_row, curve)
}

calc_prem_cvm <- function(curve, pred_col = "cum_pct_pred", tgt_col = "cum_pct_tgt") {
  pred_col <- sym(pred_col)
  tgt_col <- sym(tgt_col)
  curve %>%
    as_tibble() %>%
    mutate(
      xdiff = !!pred_col - dplyr::lag(!!pred_col, default = 0),
      value = (!!tgt_col + dplyr::lag(!!tgt_col, default = 0)) / 2,
      sq_diff = (!!pred_col - value) ^ 2
    ) %>%
    summarise(distance = sum(xdiff * sq_diff)) %>%
    pull(distance)
}


plot_dist_curve <- function(curve, 
                            tgt_col,
                            wgt_col,
                            grp_col = NULL, 
                            title = NULL) {
  curve <- curve %>%
    as_tibble() %>%
    group_by_at(grp_col) %>%
    filter(
      !!sym(tgt_col) != lag(!!sym(tgt_col), default = -Inf) |
        !!sym(tgt_col) != lead(!!sym(tgt_col), default = Inf)
    ) %>%
    ungroup()
  
  pars <- list(
    x = rlang::quo(!!rlang::sym(wgt_col)), 
    y = rlang::quo(!!rlang::sym(tgt_col))
  )
  
  if (!is.null(grp_col)) {
    pars <- rutils::list_edit(pars, color = rlang::quo(!!rlang::sym(grp_col)))
  }
  
  ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), 
      color = "red",
      size = 1.2
    ) + 
    ggplot2::geom_line(
      data = curve, 
      (purrr::lift_dl(ggplot2::aes))(pars), size = 1.2
    ) + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    ) + 
    ggplot2::ggtitle(title)
}


weighted_var <- function(x, w) {
  wgt_mean <- sum(x * w) / sum(w)
  sum(w * (x - wgt_mean) ^ 2) / sum(w)
}


calc_metrics <- function(data, pred_var, loss_var, wgt_var) {
  list(
    lorenz_auc = calc_lorenz_curve(data, pred_var, loss_var, wgt_var) %>%
      calc_lorenz_auc(),
    prem_dist = calc_prem_dist_curve(data, pred_var, loss_var, wgt_var) %>%
      calc_prem_cvm(),
    var = weighted_var(data[[pred_var]], data[[wgt_var]])
  )
}


plot_regularization_path <- function(coef_df, var_grp, log_lambda_lines = NULL) {
  plt <- ggplot(
    data = coef_df
  ) +
    theme_minimal()
  
  if (length(unique(coef_df$level)) > 1) {
    plt <- plt + 
      geom_line(
        aes(
          x = log_lambda, 
          y = standardized_coefficients, 
          color = stringr::str_wrap(level, width = 20)
        )
      ) +
      scale_color_manual(values = pals::glasbey(length(unique(coef_df$level))))
  } else {
    plt <- plt +
      geom_line(aes(x = log_lambda, y = standardized_coefficients))
  }
  
  plt <- plt +
    facet_wrap(
      ~alpha,
      scales = "free_y",
      labeller = label_bquote(alpha == .(alpha)),
      ncol = 3
    ) +
    xlab(expression(log(lambda))) +
    theme(legend.title = element_blank(), axis.title.y = element_blank())
  
  subtitle <- NULL
  
  if (!is.null(log_lambda_lines)) {
    plt <- plt +
      geom_vline(data = log_lambda_lines, aes(xintercept = log_lambda))
    subtitle <- expression(
      paste(
        "Vertical lines are ", 
        log(lambda), 
        " values with optimal metric ranks"
      )
    )
  }
  
  plt + 
    ggtitle(
      glue::glue("Regularization paths for {var_grp}"),
      subtitle = subtitle
    ) 
}



