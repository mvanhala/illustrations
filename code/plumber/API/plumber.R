
library(dplyr)

coef_table <- readRDS("coefficients.rds")

#* @post /AutoPremium
#* @serializer unboxedJSON
AutoPremium <- function(res, req) {
  input <- jsonlite::fromJSON(req$postBody)
  
  vehicle_by_driver <- full_join(
    input$Drivers %>%
      rename(DrivAge = Age),
    input$Vehicles %>%
      rename(
        VehAge = Age,
        VehBody = Body,
        VehClass = Class,
        VehEnergy = Energy,
        VehEngine = Engine,
        VehMaxSpeed = MaxSpeed,
        VehUsage = Usage
      ),
    by = character()
  )
  
  predictors <- input[c("PolNumber", "PolEffDate", "BonusMalus", "RiskVar")] %>%
    as_tibble() %>%
    full_join(vehicle_by_driver, by = character()) %>%
    as_tibble() %>%
    group_by(VehicleNum) %>%
    mutate(Exposure = 1 / n()) %>%
    ungroup() %>%
    mutate(
      VehAge_log = log(1 + VehAge),
      BonusMalus_log = log(BonusMalus),
      BonusMalus_sqrt = sqrt(BonusMalus),
      VehMaxSpeed_log = log(VehMaxSpeed),
      RiskVar_log = log(RiskVar),
      Intercept = 1
    )
  
  coefficient_values <- predictors %>%
    tidyr::pivot_longer(
      -c(PolNumber, PolEffDate, DriverNum, VehicleNum, Exposure), 
      names_to = "var",
      values_to = "var_value",
      values_transform = list(var_value = as.list)
    ) %>%
    mutate(
      level = purrr::map2_chr(var_value, var, ~if (is.character(.x)) .x else .y),
      value = purrr::map_dbl(var_value, ~if (is.numeric(.)) . else 1)
    ) %>%
    left_join(coef_table, by = c("var", "level"))
  
  coefficient_values %>%
    group_by(PolNumber, PolEffDate, DriverNum, VehicleNum, Exposure) %>%
    summarise(predicted = exp(sum(value * coefficient))) %>%
    ungroup() %>%
    mutate(
      Premium = round(Exposure * predicted, 2)
    ) %>%
    select(-Exposure, -predicted) %>%
    tidyr::nest(Drivers = c(DriverNum, Premium)) %>%
    mutate(VehiclePremium = purrr::map_dbl(Drivers, ~sum(.$Premium))) %>%
    tidyr::nest(Vehicles = c(VehicleNum, VehiclePremium, Drivers)) %>%
    mutate(PolicyPremium = purrr::map_dbl(Vehicles, ~sum(.$VehiclePremium))) %>%
    select(PolNumber, PolEffDate, PolicyPremium, Vehicles) %>%
    purrr::transpose() %>%
    .[[1]]
}

