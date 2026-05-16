# 01_data_pull.R
# Pull SL (slider) and ST (sweeper) pitches from Statcast via baseballr.
# Saves raw data to data/raw/statcast_sl_st.rds.
#
# Year range: 2022-2024.  The ST label did not exist before 2022, so earlier
# seasons are excluded.  Adjust YEARS below if extending the panel.

library(baseballr)
library(dplyr)
library(purrr)

YEARS <- 2022:2024

# baseballr::statcast_search returns one week at a time, so we pull
# month-by-month and bind.  The regular season runs roughly April-October.
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
        start_date  = as.character(s),
        end_date    = as.character(e),
        player_type = "pitcher"
      ),
      error = function(err) {
        warning(sprintf("Failed %s - %s: %s", s, e, conditionMessage(err)))
        NULL
      }
    )
  })
}

raw <- map_dfr(YEARS, pull_year)

# Keep only sliders and sweepers; drop rows missing the key movement fields.
sl_st <- raw |>
  filter(pitch_type %in% c("SL", "ST")) |>
  filter(!is.na(pfx_x), !is.na(pfx_z), !is.na(release_speed)) |>
  select(
    game_date, pitcher, batter, pitch_type, stand, p_throws,
    release_speed, pfx_x, pfx_z,
    release_spin_rate, release_spin_axis,
    # outcome columns used in downstream modelling
    description, events, launch_angle, launch_speed,
    # plate-discipline helpers (zone needed for chase rate)
    zone, type
  )

saveRDS(sl_st, "data/raw/statcast_sl_st.rds")
message(sprintf("Saved %d rows to data/raw/statcast_sl_st.rds", nrow(sl_st)))
