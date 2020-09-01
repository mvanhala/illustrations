
library(dplyr)

spec_json_body <- function(description, req_schema = list()) {
  list(
    description = description,
    required = TRUE,
    content = list(
      "application/json" = list(
        schema = req_schema
      )
    )
  )
}

spec_json_responses <- function(success_desc, res_schema = list()) {
  list(
    "200" = list(
      description = success_desc,
      content = list(
        "application/json" = list(
          schema = res_schema
        )
      )
    ),
    "400" = list(
      description = "Bad request"
    )
  )
}

spec_path_post_json <- function(endpoint, summary, tag_name, body, responses) {
  l <- list(
    post = list(
      summary = summary,
      tags = list(
        list(name = tag_name)
      ),
      requestBody = body,
      responses = responses
    )
  )
  rlang::set_names(list(l), endpoint)
}

spec_parse_schema <- function(schema_df) {
  schema_df <- schema_df %>%
    mutate(
      other = purrr::modify2(
        other, type,
        ~{if (.y == "date" && is.null(.x$format)) .x$format = "date"; .x}
      ),
      type = recode(
        type, 
        "date" = "string", 
        "character" = "string", 
        "numeric" = "number",
        "logical" = "boolean",
        "list" = "object"
      )
    )
  
  properties <- schema_df %>%
    select(-var) %>%
    purrr::transpose() %>%
    rlang::set_names(schema_df$var) %>%
    purrr::map(
      function(l) {
        l %>%
          rutils::list_edit(other = NULL) %>%
          rutils::list_edit(!!!l$other)
      }
    )
  
  list(
    type = "object",
    properties = properties
  )
}

spec_schema_ref <- function(ref) {
  list("$ref" = glue::glue("#/components/schemas/{ref}"))
}

spec_list_to_schema <- function(x) {
  if (!is.list(x)) return(
    list(
      type = case_when(
        is.integer(x) ~ "integer",
        is.numeric(x) ~ "number",
        is.logical(x) ~ "boolean",
        TRUE ~ "string"
      ),
      example = x
    )
  )
  list(type = "object", properties = purrr::map(x, spec_list_to_schema))
}


schemas <- list(
  input_auto = tribble(
    ~var,         ~type,        ~description,                  ~other,                      
    "PolNumber",   "character", "Policy number",               list(example = 12345678L),          
    "PolEffDate",  "date",      "Policy effective date",       list(example = "2021-01-01"),
    "BonusMalus",  "integer",   "Policy-level bonus/malus",    list(example = 72L),
    "RiskVar",     "integer",   "Policy-level risk variable",  list(example = 15L, enum = 1:20),
    "Drivers",     "list",      "Array of drivers",            list(type = "array", items = spec_schema_ref("DriverVars")),
    "Vehicles",    "list",      "Array of vehicles",           list(type = "array", items = spec_schema_ref("VehicleVars"))
  ), 
  input_driver = tribble(
    ~var,         ~type,        ~description,                  ~other,                                                                                                 
    "DriverNum",  "integer",    "Driver number",               list(example = 1L),
    "Gender",     "character",  "Gender",                      list(example = "Male", enum = c("Male", "Female")),                                                                                     
    "MariStat",   "character",  "Marital status",              list(example = "Alone", enum = c("Alone", "Other")),                                                                                     
    "Age",        "integer",    "Driver age",                  list(example = 30L),                               
    "SocioCateg", "character",  "Socio-professional category", list(example = "CSP50", enum = c("CSP1", "CSP26", "CSP37", "CSP42", "CSP46", "CSP48", "CSP50", "CSP55", "CSP60", "CSP66", "Other"))                                                                                
  ),
  input_vehicle = tribble(
    ~var,         ~type,        ~description,            ~other,                                                                                                 
    "VehicleNum", "integer",    "Vehicle number",        list(example = 1L),
    "Age",        "integer",    "Vehicle age in years",  list(example = 2L),                                                                                     
    "Body",       "character",  "Vehicle body",          list(example = "sedan", enum = c("bus", "cabriolet", "coupe", "microvan", "other microvan", "sedan", "sport utility vehicle", "station wagon", "van")),                                                                                     
    "Class",      "character",  "Vehicle class",         list(example = "B", enum = c("0", "A", "B", "H", "M1", "M2")),                               
    "Energy",     "character",  "Vehicle energy",        list(example = "regular", enum = c("diesel", "eletric", "GPL", "regular")),
    "Engine",     "character",  "Vehicle engine",        list(example = "injection", enum = c("carburation", "direct injection overpowered", "electric", "GPL", "injection", "injection overpowered")),
    "Garage",     "character",  "Garage location type",  list(example = "None", enum = c("Collective garage", "None", "Private garage")),
    "HasKmLimit", "character",  "Mileage limit?",        list(example = "No", enum = c("Yes", "No")),
    "MaxSpeed",   "integer",    "Maximum vehicle speed", list(example = 160L),
    "Usage",      "character",  "Vehicle speed",         list(example = "Professional", enum = c("Private", "Private+trip to office", "Professional", "Professional run")),
  ),
  response_policy = tribble(
    ~var,            ~type,        ~description,            ~other,                      
    "PolNumber",     "character", "Policy number",          list(example = 12345678L),          
    "PolEffDate",    "date",      "Policy effective date",  list(example = "2021-01-01"),
    "PolicyPremium", "number",    "Total policy premium",   list(example = 630.12),
    "Vehicles",      "list",      "Vehicle-level premiums", list(type = "array", items = spec_schema_ref("ResponseAutoVehicle"))
  ),
  response_vehicle = tribble(
    ~var,              ~type,     ~description,                     ~other,
    "VehicleNum",     "integer", "Vehichle number",                 list(example = 1L),
    "VehiclePremium", "number",  "Vehicle premium",                 list(example = 257.51),
    "Drivers",        "list",    "Driver-level premium allocation", list(type = "array", items = spec_schema_ref("ResponseAutoDriver"))
  ),
  response_driver = tribble(
    ~var,        ~type,     ~description,                           ~other,                      
    "DriverNum", "integer", "Driver number",                        list(example = 1L), 
    "Premium",   "number",  "Driver-allocated premium for vehicle", list(example = 154.24)
  )
)

swagger_list <- list(
  openapi = "3.0.0",
  info = list(
    title = "Auto models",
    description = "Calculating auto policy premium",
    version = 1.0
  ),
  paths = c(
    spec_path_post_json(
      endpoint = "/AutoPremium",
      summary = "Calculate auto policy premium",
      tag_name = "French Auto Premium",
      body = spec_json_body(
        "Parameters for calculating premium", 
        spec_schema_ref("AutoVars")
      ),
      responses = spec_json_responses(
        "Calculated premiums",
        spec_schema_ref("ResponseAutoPolicy")
      )
    )
  ),
  components = list(
    schemas = list(
      "AutoVars" = spec_parse_schema(schemas$input_auto),
      "DriverVars" = spec_parse_schema(schemas$input_driver),
      "VehicleVars" = spec_parse_schema(schemas$input_vehicle),
      "ResponseAutoPolicy" = spec_parse_schema(schemas$response_policy),
      "ResponseAutoVehicle" = spec_parse_schema(schemas$response_vehicle),
      "ResponseAutoDriver" = spec_parse_schema(schemas$response_driver)
    )
  )
)

yaml::write_yaml(swagger_list, "code/plumber/API/spec.yaml")

