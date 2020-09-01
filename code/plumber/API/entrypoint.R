
pr <- plumber::plumb("plumber.R")

pr$run(
  host = "0.0.0.0",
  port = 8000,
  swagger = function(pr, spec, ...) {
    spec_yaml <- yaml::read_yaml("spec.yaml")
    spec <- pr$swaggerFile()

    spec$info <- spec_yaml$info
    spec$paths <- spec_yaml$paths
    spec$components <- spec_yaml$components

    spec
  }
)


