library(dplyr)

compute_block_pop_weighted_centroid <- function(state) {
  aea_proj <- glue::glue(
    "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 ",
    "+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km"
  )
  
  zcta_state <- sfcensus::zcta_sf %>%
    filter(state_abb == state) %>%
    st_transform(aea_proj)
  
  block_state <- read_sf(fs::path("data/blocks", state))
  
  block_state_pop_area <- block_state %>%
    select(BLOCKID10, POP10) %>%
    filter(POP10 > 0) %>%
    st_transform(aea_proj) %>%
    mutate(area_total = st_area(.))
  
  zcta_center_pop <- st_intersection(zcta_state, block_state_pop_area) %>%
    mutate(area_int = st_area(.)) %>%
    st_centroid() %>%
    bind_cols(as_tibble(st_coordinates(.))) %>%
    st_set_geometry(NULL) %>%
    as_tibble() %>%
    filter(area_int > units::as_units(0, "km^2")) %>%
    mutate(pop_est = units::set_units(POP10 * area_int / area_total, NULL)) %>%
    group_by(zcta) %>%
    summarise(across(c(X, Y), ~sum(. * pop_est) / sum(pop_est))) %>%
    ungroup() %>%
    st_as_sf(coords = c("X", "Y"), crs = aea_proj) %>%
    st_transform(4326) %>%
    transmute(state_abb = state, zcta)
  
  zcta_missing <- zcta_state %>%
    anti_join(st_set_geometry(zcta_center_pop, NULL), by = "zcta") %>%
    st_transform(aea_proj) %>%
    st_centroid() %>%
    st_transform(4326)
  
  bind_rows(
    zcta_center_pop,
    zcta_missing
  )
}

states <- sfcensus::states$state_abb

r_proc <- purrr::map(
  split(states, seq_along(states) %% 4),
  ~callr::r_bg(
    function(states, fn) {
      library(sf)
      library(dplyr)
      purrr::map_dfr(states, fn)
    },
    args = list(states = ., fn = compute_block_pop_weighted_centroid)
  )
)

purrr::walk(r_proc, ~.$wait())

zcta_centers_of_population <- purrr::map_dfr(r_proc, ~.$get_result())

readr::write_rds(
  zcta_centers_of_population %>%
    arrange(zcta), 
  "data/derived/zcta_centers_of_population.rds"
)

