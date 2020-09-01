
library(leaflet)
library(raster)
library(terra)

# https://nassgeodata.gmu.edu/CropScape/
# https://nassgeodata.gmu.edu/CropScape/devhelp/getexamples.html

purrr::walk(
  2016:2019,
  ~download.file(
    glue::glue("https://nassgeodata.gmu.edu/webservice/nass_data_cache/byfips/CDL_{.}_46.tif"),
    glue::glue("data/south_dakota_cdl_{.}.tif"),
    method = "wget",
    quiet = TRUE
  )
)

sd_crops <- purrr::map(
  rev(fs::dir_ls("data", regex = "south_dakota_cdl")),
  rast
)

sd_crops_agg <- purrr::map(
  sd_crops,
  aggregate,
  fact = 10,
  fun = "modal"
)

rasterize_factorize <- function(rast) {
  r <- raster(rast)
  r[!(r %in% c(1, 5, 36, 37, 61))] <- NA
  r[r %in% c(36, 37)] <- 36
  r <- ratify(r)
  lev <- levels(r)[[1]]
  lev$crop <- c("Corn", "Soybeans", "Alfalfa/Hay", "Fallow")
  lev$code <- c(1, 5, 36, 61)
  levels(r) <- lev
  r
}

sd_southeast <- sfcensus::county_sf %>%
  dplyr::filter(state_abb == "SD", county_name %in% c("Union", "Clay", "Yankton")) %>%
  sf::st_transform(crs(sd_crops[[1]]))

sd_southeast_crop <- sd_crops %>%
  purrr::map(~crop(., sd_southeast)) %>%
  purrr::map(rasterize_factorize) %>%
  purrr::set_names(~paste(readr::parse_number(.), "- Southeast SD - 30m resolution"))

sd_statewide_crop <- sd_crops_agg %>%
  purrr::map(rasterize_factorize) %>%
  purrr::set_names(~paste(readr::parse_number(.), "- Statewide SD - 300m resolution"))

sd_crop_pal <- colorFactor(
  pals::glasbey(4),
  domain =  c(1, 5, 36, 61), 
  na.color = "transparent"
)

sd_crop_all <- c(sd_statewide_crop, sd_southeast_crop)

sd_crop_map <- purrr::reduce2(
  sd_crop_all,
  names(sd_crop_all),
  function(map, layer, name) {
    leaflet::addRasterImage(
      map,
      layer, 
      colors = sd_crop_pal, 
      opacity = 0.7, 
      group = name
    )
  },
  .init = leaflet() %>%
    addProviderTiles("CartoDB.Voyager")
) %>%
  addLegend(
    "bottomright", 
    opacity = 0.8, 
    colors = pals::glasbey(4),
    labels =  c("Corn", "Soybeans", "Alfalfa/Hay", "Fallow"),
    title = "Crop"
  ) %>%
  addLayersControl(
    baseGroups = names(sd_crop_all),
    options = layersControlOptions(collapsed = FALSE)
  )

rutils::save_widget(
  sd_crop_map, 
  "docs/south_dakota_cropland.html", 
  title = "South Dakota Cropland 2016 - 2019"
)


