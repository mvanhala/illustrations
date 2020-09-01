sim_data <- function(data, n, idcol) {
  idcol <- sym(idcol)
  km_fit <- survfit(Surv(loss, uncensored) ~ 1, data = data)
  n_clmnt <- count(data, !!idcol)[["n"]]
  
  losses <- tibble(claim_id = 1:n) %>%
    mutate(n_claim = sample(n_clmnt, n, replace = TRUE)) %>%
    slice(rep(1:n, times = n_claim)) %>%
    mutate(unif_loss = runif(nrow(.))) %>%
    mutate(loss = unname(quantile(km_fit, unif_loss)$quantile))
  
  large_loss_cens <- data %>%
    filter(loss >= 100000) %>% 
    transmute(
      left = loss,
      right = if_else(uncensored == 1, loss, NA_real_)
    )
  
  max_loss <- data %>%
    filter(uncensored == 1) %>%
    pull(loss) %>%
    max()
  
  fit_pareto <- fitdistcens(
    as.data.frame(large_loss_cens),
    "pareto1", 
    fix.arg = list(min = 100000)
  )
  
  min_surv <- km_fit$surv[length(km_fit$surv) - 1]
  
  losses %>%
    mutate(
      loss = case_when(
        is.na(loss) & unif_loss < 0.01 ~ min(km_fit$time),
        is.na(loss) | loss >= 100000 ~ rpareto1(nrow(.), shape = fit_pareto[["estimate"]][["shape"]], min = 100000),
        TRUE ~ loss
      )
    )
}

calc_lev <- function(data, limit_1, limit_2) {
  data_limited <- data %>%
    mutate(loss_limited = pmin(limit_1, loss)) %>%
    as.data.table()
  
  claim_data <- data_limited[, .(loss_limited = sum(loss_limited)), .(claim_id)]
  claim_data[, loss_limited := pmin(limit_2, loss_limited)]
  mean(claim_data$loss_limited)
}

calc_ilfs <- function(data, n_sim_claim, idcol, limit_combos) {
  data <- sim_data(data, n_sim_claim, idcol)
  limit_combos %>%
    mutate(lev = purrr::pmap_dbl(list(limit_1, limit_2), calc_lev, data = data))
}

