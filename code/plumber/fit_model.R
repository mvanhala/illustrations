
library(CASdatasets)
library(dplyr)
library(h2o)

source("code/elastic_net_models/model_functions.R")

data(freMPL1)

data <- freMPL1 %>%
  as_tibble() %>% 
  transmute(
    PolNumber = 1:n(),
    Exposure, 
    VehAge = readr::parse_number(as.character(VehAge)),
    VehAge_log = log(1 + VehAge),
    Gender,
    MariStat,
    SocioCateg = forcats::fct_lump_prop(SocioCateg, 0.01, w = Exposure),
    VehUsage,
    DrivAge,
    DrivAge_log = log(DrivAge),
    DrivAge_sqrt = sqrt(DrivAge),
    DrivAge_cubert = DrivAge ^ (1 / 3),
    HasKmLimit = factor(HasKmLimit, levels = c(0, 1), labels = c("No", "Yes")),
    BonusMalus,
    BonusMalus_log = log(BonusMalus),
    BonusMalus_sqrt = sqrt(BonusMalus),
    VehBody,
    VehEngine,
    VehEnergy,
    VehMaxSpeed = pmax(100, readr::parse_number(as.character(VehMaxSpeed))),
    VehMaxSpeed_log = log(VehMaxSpeed),
    VehClass, 
    RiskVar,
    RiskVar_log = log(RiskVar),
    Garage, 
    ClaimAmount,
    target = ClaimAmount / Exposure
  )


predictors <- c(
  "VehAge", "VehAge_log", "Gender", 
  "MariStat", "SocioCateg", "VehUsage", "DrivAge", "DrivAge_log", 
  "DrivAge_sqrt", "DrivAge_cubert", "HasKmLimit", "BonusMalus", 
  "BonusMalus_log", "BonusMalus_sqrt", "VehBody", "VehEngine", 
  "VehEnergy", "VehMaxSpeed", "VehMaxSpeed_log", "VehClass", "RiskVar", 
  "RiskVar_log", "Garage"
)


data_model <- data %>%
  select(c(all_of(predictors), target, Exposure)) %>%
  mutate(
    across(where(is.character), factor),
    across(target, ~pmax(., 0))
  )

h2o.init(max_mem_size = "32G", nthreads = 4)


data_model_h2o <- h2o_import_fast(data_model)

model <- h2o::h2o.glm(
  x = predictors,
  y = "target",
  training_frame = data_model_h2o,
  weights_column = "Exposure",
  alpha = 0.5,
  lambda = 0.3,
  family = "tweedie", 
  tweedie_variance_power = 1.5,
  tweedie_link_power = 0
)

coef_table <- model@model$coefficients_table %>% 
  as_tibble() %>%
  tidyr::separate(names, c("var", "level"), sep = "\\.", remove = FALSE) %>%
  transmute(
    var, 
    level = coalesce(level, var),
    coefficient = coefficients
  )

readr::write_rds(coef_table, "code/plumber/API/coefficients.rds")

h2o.shutdown(FALSE)



#-------
## sample json input for Plumber API

drivers <- data_model %>% 
  slice(1, 2, 7, 8) %>%
  transmute(
    PolNumber = 12345678L,
    PolEffDate = as.Date("2021-01-01"),
    DriverNum = 1:n(),
    Gender,
    MariStat,
    Age = DrivAge,
    SocioCateg
  )

vehicles <- data_model %>%
  slice(1, 4, 12) %>%
  transmute(
    PolNumber = 12345678L,
    PolEffDate = as.Date("2021-01-01"),
    VehicleNum = 1:n(),
    Age = VehAge,
    Body = VehBody,
    Class = VehClass,
    Energy = VehEnergy,
    Engine = VehEngine,
    Garage,
    HasKmLimit,
    MaxSpeed = VehMaxSpeed,
    Usage = VehUsage
  )

policies <- data_model %>%
  slice(1) %>%
  transmute(
    PolNumber = 12345678L,
    PolEffDate = as.Date("2021-01-01"),
    BonusMalus,
    RiskVar
  )

input <- policies %>%
  left_join(
    drivers %>%
      tidyr::nest(Drivers = c(-PolNumber, -PolEffDate)),
    by = c("PolNumber", "PolEffDate")
  ) %>%
  left_join(
    vehicles %>%
      tidyr::nest(Vehicles = c(-PolNumber, -PolEffDate)),
    by = c("PolNumber", "PolEffDate")
  )

input %>%
  mutate(across(where(lubridate::is.Date), as.character)) %>%
  purrr::transpose() %>%
  .[[1]] %>%
  jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
  write("code/plumber/input_sample.json")


