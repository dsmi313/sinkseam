# 02_clean.R
# Cleans raw Statcast SI/FT data and engineers the three outcome variables:
#   ground_ball  – binary, 1 if batted ball type is ground ball
#   whiff        – binary, 1 if swing-and-miss
#   woba_contact – numeric wOBA weights for balls in play (NA for non-contact)
#
# Key transformation: flip pfx_x sign for LHP so arm-side movement is always
# positive, matching the convention used by most movement-profile research.
#
# Saves: data/clean/si_ft_clean.rds

library(dplyr)

raw <- readRDS("data/raw/statcast_si_ft.rds")

# wOBA linear weights (2020-2023 average, source: FanGraphs guts page).
# Update these weights if the year range changes.
WOBA_WEIGHTS <- c(
  single      = 0.888,
  double      = 1.271,
  triple      = 1.616,
  home_run    = 2.101,
  walk        = 0.690,
  hit_by_pitch = 0.720,
  out         = 0.000
)

event_to_woba <- function(events) {
  case_when(
    events == "single"        ~ WOBA_WEIGHTS["single"],
    events == "double"        ~ WOBA_WEIGHTS["double"],
    events == "triple"        ~ WOBA_WEIGHTS["triple"],
    events == "home_run"      ~ WOBA_WEIGHTS["home_run"],
    events %in% c("walk", "intent_walk") ~ WOBA_WEIGHTS["walk"],
    events == "hit_by_pitch"  ~ WOBA_WEIGHTS["hit_by_pitch"],
    events %in% c(
      "field_out", "grounded_into_double_play", "double_play",
      "fielders_choice_out", "fielders_choice", "force_out",
      "sac_fly", "sac_bunt", "strikeout", "strikeout_double_play"
    ) ~ WOBA_WEIGHTS["out"],
    TRUE ~ NA_real_
  )
}

clean <- raw |>
  mutate(
    # Arm-side movement convention: positive = arm side for both hands.
    pfx_x_adj = if_else(p_throws == "L", -pfx_x, pfx_x),

    # Outcome 1: ground ball (batted-ball type via description/events proxy).
    # Statcast does not return bb_type directly from statcast_search; we
    # reconstruct from 'events'. Treat grounded_into_double_play as GB.
    ground_ball = as.integer(
      events %in% c("field_out", "grounded_into_double_play",
                    "double_play", "fielders_choice_out", "fielders_choice",
                    "force_out") &
      # A rough proxy: exclude fly-ball-ish events
      !(events %in% c("sac_fly"))
    ),
    # Note: This is a rough proxy.  A cleaner approach uses the bb_type column
    # from a direct Baseball Savant CSV download.

    # Outcome 2: whiff (swing and miss on this pitch).
    whiff = as.integer(description %in% c("swinging_strike",
                                          "swinging_strike_blocked",
                                          "foul_tip")),

    # Outcome 3: wOBA against (defined only on plate-appearance-ending pitches).
    woba = event_to_woba(events),

    # Label as integer for JAGS (SI = 1, FT = 0).
    label_si = as.integer(pitch_type == "SI"),

    # Pitcher ID as a consecutive integer index (required by JAGS).
    pitcher_id = as.integer(factor(pitcher))
  ) |>
  # Standardize movement and speed for GMM and JAGS.
  mutate(
    pfx_x_z   = scale(pfx_x_adj)[, 1],
    pfx_z_z   = scale(pfx_z)[, 1],
    speed_z   = scale(release_speed)[, 1]
  ) |>
  select(
    game_date, pitcher, pitcher_id, batter, pitch_type, label_si,
    p_throws, stand,
    release_speed, pfx_x, pfx_z, pfx_x_adj,
    pfx_x_z, pfx_z_z, speed_z,
    description, events,
    ground_ball, whiff, woba
  )

saveRDS(clean, "data/clean/si_ft_clean.rds")
message(sprintf(
  "Saved %d pitches (%d pitchers) to data/clean/si_ft_clean.rds",
  nrow(clean), n_distinct(clean$pitcher)
))
