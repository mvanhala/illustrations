
library(dplyr)

state_groups <- sfcensus::states %>% 
  select(state_abb, state_fips) %>%
  group_split(1:n() %% 4, .keep = FALSE) %>%
  purrr::map(tibble::deframe)

proc <- purrr::map(
  state_groups,
  ~callr::r_bg(
    function(states) {
      fs::dir_create("log")
      download_state_blocks <- function(code, state) {
        write(paste(Sys.time(), state), "log/block_download.txt", append = TRUE)
        tmp <- fs::file_temp()
        on.exit(try(fs::file_delete(tmp)), add = TRUE)
        download.file(
          glue::glue("https://www2.census.gov/geo/tiger/TIGER2010BLKPOPHU/tabblock2010_{code}_pophu.zip"),
          tmp
        )
        unzip(tmp, exdir = fs::path("data/blocks", state))
      }
      purrr::iwalk(states, download_state_blocks)
    },
    args = list(states = .)
  )
)

purrr::walk(proc, ~.$wait())

