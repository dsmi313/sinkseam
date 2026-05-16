# 01_data_pull.R
# Pull SI and FT pitches from Statcast via baseballr for 2020-2023.
# Saves raw data to data/raw/statcast_si_ft.rds.
#
# Year range: 2020-2023 (COVID-shortened 2020 included; 2024 excluded pending
# full-season data availability).  Adjust YEARS below to extend the panel.

library(baseballr)
library(dplyr)
library(purrr)

YEARS <- 2020:2023

# baseballr::statcast_search returns one week at a time, so we pull
# month-by-month and bind.  The regular season runs roughly April–October.
season_dates <- function(year) {
  starts <- seq(
    as.Date(paste0(year, "-04-01")),
    as.Date(paste0(year, "-10-01")),
    by = "month"
  )
  ends <- pmin(starts + 30, as.Date(paste0(year, "-10-31")))
  data.frame(start = starts, end = ends)
}

pull_year <- function(year) {
  dates <- season_dates(year)
  message(sprintf("Pulling %d (%d chunks)...", year, nrow(dates)))
  map2_dfr(dates$start, dates$end, function(s, e) {
    Sys.sleep(0.5)  # be polite to the Baseball Savant endpoint
    tryCatch(
      statcast_search(
        start_date = as.character(s),
        end_date   = as.character(e),
        player_type = "pitcher"
      ),
      error = function(err) {
        warning(sprintf("Failed %s – %s: %s", s, e, conditionMessage(err)))
        NULL
      }
    )
  })
}

raw <- map_dfr(YEARS, pull_year)

# Keep only sinkers and two-seamers; drop rows missing the key movement fields.
si_ft <- raw |>
  filter(pitch_type %in% c("SI", "FT")) |>
  filter(!is.na(pfx_x), !is.na(pfx_z), !is.na(release_speed)) |>
  select(
    game_date, pitcher, batter, pitch_type, stand, p_throws,
    release_speed, pfx_x, pfx_z,
    # outcome columns used in downstream modelling
    description, events, launch_angle, launch_speed,
    # plate-discipline helpers
    zone, type
  )

saveRDS(si_ft, "data/raw/statcast_si_ft.rds")
message(sprintf("Saved %d rows to data/raw/statcast_si_ft.rds", nrow(si_ft)))
