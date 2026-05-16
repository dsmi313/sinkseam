# 02_clean.R
# Cleans raw Statcast CU/SL/ST data and engineers outcome variables:
#   whiff  – binary, 1 if swing-and-miss or foul-tip
#   chase  – binary, 1 if out-of-zone pitch with a swing (NA for in-zone)
#   woba   – numeric wOBA for plate-appearance-ending pitches
#
# pfx_x sign convention: flip for LHP so that glove-side break is in the same
# direction for both handednesses.  After adjustment, positive values indicate
# arm-side movement; breaking balls (CU/SL/ST) have negative pfx_x_adj for
# both RHP and LHP.
#
# Label encoding: CU is the reference category.
#   label_sl = 1 if SL, 0 otherwise (CU or ST)
#   label_st = 1 if ST, 0 otherwise (CU or SL)
#   Both = 0 → CU (reference)
#
# Saves: data/clean/cu_sl_st_clean.rds

library(dplyr)

raw <- readRDS("data/raw/statcast_cu_sl_st.rds")

# wOBA linear weights (2022-2024 average, source: FanGraphs guts page).
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

SWING_DESCRIPTIONS <- c(
  "swinging_strike", "swinging_strike_blocked", "foul_tip",
  "foul", "foul_bunt", "missed_bunt",
  "hit_into_play", "hit_into_play_no_out", "hit_into_play_score"
)

clean <- raw |>
  mutate(
    # Flip pfx_x for LHP so glove-side break is consistently directed.
    # Result: arm-side = positive, glove-side (breaking balls) = negative.
    pfx_x_adj = if_else(p_throws == "L", -pfx_x, pfx_x),

    # Outcome 1: whiff.
    whiff = as.integer(description %in% c("swinging_strike",
                                          "swinging_strike_blocked",
                                          "foul_tip")),

    # Outcome 2: chase (swing on out-of-zone pitch; NA for in-zone pitches).
    chase = case_when(
      zone %in% 11:14 & description %in% SWING_DESCRIPTIONS ~ 1L,
      zone %in% 11:14 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Outcome 3: wOBA against.
    woba = event_to_woba(events),

    # Dummy labels — CU is the reference category (both = 0).
    label_sl = as.integer(pitch_type == "SL"),
    label_st = as.integer(pitch_type == "ST"),

    # Same-handedness matchup: 1 when pitcher and batter share throwing/batting hand.
    # Sweepers and sliders behave very differently against same- vs. opposite-hand batters.
    same_hand = as.integer(
      (p_throws == "R" & stand == "R") | (p_throws == "L" & stand == "L")
    ),

    # Two-strike indicator for count context.
    two_strike = as.integer(strikes == 2),

    pitcher_id = as.integer(factor(pitcher))
  ) |>
  mutate(
    pfx_x_z     = scale(pfx_x_adj)[, 1],
    pfx_z_z     = scale(pfx_z)[, 1],
    speed_z     = scale(release_speed)[, 1],
    spin_axis_z = scale(release_spin_axis)[, 1]
  ) |>
  select(
    game_date, game_year, pitcher, pitcher_id, batter, pitch_type,
    label_sl, label_st,
    p_throws, stand, same_hand,
    balls, strikes, two_strike, outs_when_up, inning,
    release_speed, release_spin_rate, release_spin_axis,
    pfx_x, pfx_z, pfx_x_adj, plate_x, plate_z,
    pfx_x_z, pfx_z_z, speed_z, spin_axis_z,
    description, events, type, zone,
    whiff, chase, woba
  )

saveRDS(clean, "data/clean/cu_sl_st_clean.rds")
message(sprintf(
  "Saved %d pitches (%d pitchers) to data/clean/cu_sl_st_clean.rds",
  nrow(clean), n_distinct(clean$pitcher)
))
