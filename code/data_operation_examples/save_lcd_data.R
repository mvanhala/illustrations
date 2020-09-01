# 
# https://www1.ncdc.noaa.gov/pub/data/cdo/samples/LCD_sample_pdf.pdf
# 
# http://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# 
# www1.ncdc.noaa.gov/pub/data/ish/ish-format-document.pdf

# https://openflights.org/data.html


library(rnoaa)
library(dplyr)

find_lcd_avail <- function(year) {
  cat(year, strftime(Sys.time(), "%H:%M:%S"), "\n")
  page <- xml2::read_html(
    glue::glue("https://www.ncei.noaa.gov/data/local-climatological-data/access/{year}")
  )
  links <- xml2::xml_find_all(page, ".//a")
  tibble(
    link = xml2::xml_attr(links, "href"),
    text = xml2::xml_text(links)
  ) %>% 
    filter(link == text) %>%
    transmute(link, year = year)
}

station_years <- purrr::map_dfr(2000:2019, find_lcd_avail)

airports <- readr::read_csv(
  "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", 
  col_names = c("airport_id", "name", "city", "country", "iata", "icao", "lat", "lon",
                "elev", "tz_hours", "dst", "tz_name", "type", "source"),
  col_types = c(lat = "d", lon = "d", elev = "d", tz_hours = "d", .default = "c"),
  na = "\\N"
)

airports_selected <- tribble(
  ~icao,  ~city,                 ~state,
  "KABQ", "Albuquerque",         "NM",  
  "PANC", "Anchorage",           "AK",  
  "KATL", "Atlanta",             "GA",  
  "KBIL", "Billings",            "MT",  
  "KBOI", "Boise",               "ID",  
  "KBOS", "Boston",              "MA",  
  "KCLT", "Charlotte",           "NC",  
  "KCYS", "Cheyenne",            "WY",  
  "KORD", "Chicago",             "IL",  
  "KCLE", "Cleveland",           "OH",  
  "KDFW", "Dallas",              "TX",  
  "PAFA", "Fairbanks",           "AK",  
  "PHNL", "Honolulu",            "HI",  
  "KIAH", "Houston",             "TX",  
  "KIND", "Indianapolis",        "IN",  
  "KINL", "International Falls", "MN",  
  "KLAS", "Las Vegas",           "NV",  
  "KMEM", "Memphis",             "TN",  
  "KMIA", "Miami",               "FL",  
  "KMSP", "Minneapolis",         "MN",  
  "KMOT", "Minot",               "ND",  
  "KMSY", "New Orleans",         "LA",  
  "KLGA", "New York",            "NY",  
  "KOMA", "Omaha",               "NE",  
  "KMCO", "Orlando",             "FL",  
  "KPHX", "Phoenix",             "AZ",  
  "KPDX", "Portland",            "OR",  
  "KRAP", "Rapid City",          "SD",  
  "KSLC", "Salt Lake City",      "UT",  
  "KSAN", "San Diego",           "CA",  
  "KSFO", "San Francisco",       "CA",  
  "KSEA", "Seattle",             "WA",  
  "KGEG", "Spokane",             "WA",  
  "KSTL", "St. Louis",           "MO",  
  "KDCA", "Washington",          "DC",  
  "KICT", "Wichita",             "KS"
) 

isd_stations <- isd_stations()

stations_to_download <- isd_stations %>%
  mutate(
    id = paste0(usaf, wban),
    link = paste0(id, ".csv")
  ) %>%
  semi_join(
    station_years %>% 
      count(link) %>%
      filter(n == 20),
    by = "link"
  ) %>%
  semi_join(airports_selected, by = "icao") %>%
  mutate(year = list(2000:2019)) %>%
  tidyr::unnest(year) %>%
  select(id, year) %>%
  group_split(1:n() %% 10, .keep = FALSE)

r_proc <- purrr::map(
  station_years_to_download,
  ~callr::r_bg(
    function(to_download) {
      download_lcd_year <- function(id, year) {
        save_dir <- glue::glue("data/noaa/lcd/{year}")
        message(Sys.time(), " - ", year, " - ", id)
        fs::dir_create(save_dir)
        httr::GET(
          glue::glue("https://www.ncei.noaa.gov/data/local-climatological-data/access/{year}/{id}.csv"),
          httr::write_disk(glue::glue("{save_dir}/{id}.csv"), overwrite = TRUE)
        )
      }
      purrr::pwalk(to_download, download_lcd_year)
    },
    args = list(to_download = .)
  )
)

purrr::walk(r_proc, ~.$wait())

lcd_files <- fs::dir_ls("data/noaa/lcd", recurse = TRUE, type = "file")

tictoc::tic("Reading")
lcd_data <- purrr::map(
  lcd_files, 
  readr::read_csv, 
  col_types = readr::cols(.default = "c"),
  progress = FALSE
)
tictoc::toc()

lcd_daily <- lcd_data %>%
  purrr::map_dfr(filter, REPORT_TYPE == "SOD") %>%
  mutate_at(vars(DATE), as.Date) %>%
  select(
    STATION, DATE, NAME, starts_with("Daily")
  ) %>%
  rename_all(stringr::str_replace_all, c("^Daily" = "")) %>%
  rename_all(snakecase::to_snake_case) %>%
  select(
    station, date, name, 
    temp_max = maximum_dry_bulb_temperature,
    temp_min = minimum_dry_bulb_temperature,
    temp_avg = average_dry_bulb_temperature,
    temp_dep = departure_from_normal_average_temperature,
    humidity_avg = average_relative_humidity,
    pressure_station = average_station_pressure,
    pressure_sea_level = average_sea_level_pressure,
    hdd = heating_degree_days,
    cdd = cooling_degree_days,
    precipitation,
    snowfall,
    snow_depth,
    wind_speed_avg = average_wind_speed,
    wind_speed_peak = peak_wind_speed,
    wind_dir_peak = peak_wind_direction,
    wind_speed_sust = sustained_wind_speed,
    wind_dir_sust = sustained_wind_direction,
    weather
  ) %>%
  mutate_at(vars(precipitation, snowfall), ~recode(., "T" = "0")) %>%
  mutate_at(vars(temp_max:wind_dir_sust), as.numeric)

lcd_daily_selected <- isd_stations %>%
  transmute(
    station = paste0(usaf, wban),
    lat, 
    lon,
    icao
  ) %>%
  inner_join(airports_selected, by = "icao") %>%
  inner_join(lcd_daily, by = "station")

readr::write_rds(lcd_daily_selected, "data/noaa/lcd/lcd_daily_selected.rds")

