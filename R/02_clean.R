# 02_clean.R
# Cleans raw Statcast SL/ST data and engineers the three outcome variables:
#   whiff  – binary, 1 if swing-and-miss or foul-tip
#   chase  – binary, 1 if out-of-zone pitch with a swing (NA for in-zone)
#   woba   – numeric wOBA weight for plate-appearance-ending pitches
#
# Key transformation: flip pfx_x sign for RHP so glove-side break is always
# positive regardless of handedness.  For sliders/sweepers the relevant
# movement direction is toward the glove, not the arm — the opposite
# convention from sinkers/two-seamers.
#
# Saves: data/clean/sl_st_clean.rds

library(dplyr)

raw <- readRDS("data/raw/statcast_sl_st.rds")

# wOBA linear weights (2022-2024 average, source: FanGraphs guts page).
# Update these weights if the year range changes.
WOBA_WEIGHTS <- c(
  single       = 0.883,
  double       = 1.263,
  triple       = 1.601,
  home_run     = 2.063,
  walk         = 0.693,
  hit_by_pitch = 0.722,
  out          = 0.000
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

# Descriptions that constitute a swing.
SWING_DESCRIPTIONS <- c(
  "swinging_strike", "swinging_strike_blocked", "foul_tip",
  "foul", "foul_bunt", "missed_bunt",
  "hit_into_play", "hit_into_play_no_out", "hit_into_play_score"
)

clean <- raw |>
  mutate(
    # Glove-side movement convention: positive = glove side for both hands.
    # For sliders/sweepers the horizontal break is toward the glove, so we
    # flip RHP (whose sliders have negative pfx_x in Statcast) to positive.
    pfx_x_adj = if_else(p_throws == "R", -pfx_x, pfx_x),

    # Outcome 1: whiff (swing and miss on this pitch).
    whiff = as.integer(description %in% c("swinging_strike",
                                          "swinging_strike_blocked",
                                          "foul_tip")),

    # Outcome 2: chase (swing at out-of-zone pitch).
    # Statcast zones 11-14 are outside the strike zone.
    # NA for in-zone pitches — the JAGS model filters to non-NA rows.
    chase = case_when(
      zone %in% 11:14 & description %in% SWING_DESCRIPTIONS ~ 1L,
      zone %in% 11:14 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Outcome 3: wOBA against (defined only for plate-appearance-ending pitches).
    woba = event_to_woba(events),

    # Label as integer for JAGS (ST = 1, SL = 0).
    label_st = as.integer(pitch_type == "ST"),

    # Pitcher ID as a consecutive integer index (required by JAGS).
    pitcher_id = as.integer(factor(pitcher))
  ) |>
  # Standardize movement, speed, and spin axis for GMM and JAGS.
  mutate(
    pfx_x_z      = scale(pfx_x_adj)[, 1],
    pfx_z_z      = scale(pfx_z)[, 1],
    speed_z      = scale(release_speed)[, 1],
    spin_axis_z  = scale(release_spin_axis)[, 1]
  ) |>
  select(
    game_date, pitcher, pitcher_id, batter, pitch_type, label_st,
    p_throws, stand,
    release_speed, release_spin_rate, release_spin_axis,
    pfx_x, pfx_z, pfx_x_adj,
    pfx_x_z, pfx_z_z, speed_z, spin_axis_z,
    description, events, zone,
    whiff, chase, woba
  )

saveRDS(clean, "data/clean/sl_st_clean.rds")
message(sprintf(
  "Saved %d pitches (%d pitchers) to data/clean/sl_st_clean.rds",
  nrow(clean), n_distinct(clean$pitcher)
))
