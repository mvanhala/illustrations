
library(dplyr)
library(fasterize)
library(leaflet)
library(purrr)
library(raster)
library(sf)
library(sfcensus)
library(viridis)

#### data from
# https://www.mtbs.gov/direct-download
# https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip

aea_proj <- glue::glue(
  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 ",
  "+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km"
)

mtbs_perimeter <- read_sf("data/mtbs_perims_DD")

states_western <- c("WA", "OR", "CA", "NV", "AZ", "UT", "ID", "MT", "WY", "CO", "NM")

region <- state_sf %>% 
  filter(state_abb %in% states_western) %>% 
  st_transform(aea_proj) %>% 
  st_union() %>% 
  st_buffer(50) %>%
  st_transform(4326)

mtbs_region <- mtbs_perimeter %>%
  st_transform(4326) %>%
  st_join(st_sf(region), left = FALSE)

abs_ceiling <- function(x) ifelse(x > 0, ceiling(x), floor(x))
mround <- function(x, base, fun = round) fun(x / base) * base

bounds <- mround(
  state_sf %>% 
    filter(state_abb %in% states_western) %>%
    st_transform(aea_proj) %>% 
    st_bbox(), 
  1, 
  abs_ceiling
)

raster_km <- raster(
  xmn = bounds[["xmin"]], 
  xmx = bounds[["xmax"]], 
  ymn = bounds[["ymin"]],
  ymx = bounds[["ymax"]], 
  resolution = c(1, 1),
  crs = aea_proj
)

years <- 1984:2018

mtbs_by_year <- map(
  years,
  function(year) {
    fasterize(
      mtbs_region %>%
        filter(Year == year) %>% 
        st_transform(as.character(crs(raster_km))),
      raster_km, 
      fun = "any"
    )
  }
)

mtbs_by_year_filled <- mtbs_by_year %>%
  map(
    function(raster) {
      raster[is.na(raster)] <- 0
      raster
    }
  )

region_raster_zero <- raster_km
values(region_raster_zero) <- 0

fire_sum <- reduce(mtbs_by_year_filled, `+`, .init = region_raster_zero)

n_max <- max(getValues(fire_sum), na.rm = TRUE)

fire_sum[fire_sum == 0] <- NA

values(fire_sum) <- factor(getValues(fire_sum), levels = 1:n_max)

grp_pal <- colorFactor(
  viridis::plasma(2 * n_max, direction = -1)[2 * (1:n_max)], 
  getValues(fire_sum),
  na.color = "transparent"
)
grp_pal_rev <- colorFactor(
  viridis::plasma(2 * n_max)[2 * (1:n_max)], 
  getValues(fire_sum),
  na.color = "transparent"
)

wt_mat <- matrix(0, ncol = 41, nrow = 41)
for (i in 1:41) {
  for (j in 1:41) {
    dist <- sqrt((i - 21) ^ 2 + (j - 21) ^ 2)
    wt_mat[i, j] <- 1 - sqrt(min(dist, 20) / 20)
  }
}

fire_smoothed <- focal(
  fire_sum, 
  wt_mat, 
  na.rm = TRUE, 
  pad = TRUE, 
  padValue = 0,
  fun = sum
)
fire_smoothed[fire_smoothed == 0] <- NA

values(fire_smoothed) <- getValues(fire_smoothed) / max(getValues(fire_smoothed), na.rm = TRUE)

num_pal <- colorNumeric(
  viridis::plasma(100, direction = -1), 
  c(-0.01, 1.01), 
  na.color = "transparent"
)
num_pal_rev <- colorNumeric(
  viridis::plasma(100), 
  c(-0.01, 1.01), 
  na.color = "transparent"
)

historical_wildfire_map <- leaflet() %>%
  addProviderTiles("CartoDB.Voyager") %>%
  addRasterImage(fire_sum, colors = grp_pal, opacity = 0.75, group = "Fire frequency") %>%
  addRasterImage(fire_smoothed, colors = num_pal, opacity = 0.75, group = "Smoothed") %>%
  addLegend(
    "bottomright", grp_pal_rev, values = getValues(fire_sum), opacity = 1,
    title = "# years with fire<br>(1984 - 2018)",
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) %>%
  addLegend(
    "bottomright", 
    num_pal_rev,
    values = c(-0.01, 1.01), opacity = 1,
    title = "Smoothed<br>fire index",
    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) %>%
  addLayersControl(
    baseGroups = c("Fire frequency", "Smoothed"),
    options = layersControlOptions(collapsed = FALSE)
  )

rutils::save_widget(
  historical_wildfire_map, 
  "docs/mtbs_historical_wildfire.html", 
  title = "MTBS historical wildfre"
)


