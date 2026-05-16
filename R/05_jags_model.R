# 05_jags_model.R
# Fits three Bayesian hierarchical models via JAGS (using jagsUI):
#   Model A: whiff ~ pfx_x_z + pfx_z_z + speed_z + batter_right + two_strike + label_sl + label_st + (1|pitcher)
#   Model B: chase ~ pfx_x_z + pfx_z_z + speed_z + batter_right + two_strike + label_sl + label_st + (1|pitcher)
#   Model C: woba  ~ pfx_x_z + pfx_z_z + speed_z + batter_right + two_strike + label_sl + label_st + (1|pitcher)
#
# CU (curveball) is the reference category.  The two key estimands are:
#   beta_sl: SL vs CU effect (controlling for movement, speed, count, and handedness)
#   beta_st: ST vs CU effect (controlling for movement, speed, count, and handedness)
# A derived contrast beta_st_vs_sl = beta_st - beta_sl is computed inside JAGS.
#
# NOTE: conditioning on pfx_x_z, pfx_z_z, and speed_z makes this a conservative
# test — movement and velocity partly mediate the label-to-outcome path.  A
# positive result (label CIs exclude zero) is therefore strong evidence that the
# taxonomy captures something beyond the physics.  A null result is weaker because
# we may have conditioned away signal, not because labels are uninformative.
#
# If all three credible intervals include zero, the three-label taxonomy adds no
# predictive value beyond movement — the classification is unsupported.

library(dplyr)
library(jagsUI)

clean <- readRDS("data/clean/cu_sl_st_clean.rds")

make_jags_data_binary <- function(df, outcome_col) {
  df_cc <- df |>
    filter(!is.na(.data[[outcome_col]]),
           !is.na(pfx_x_z), !is.na(pfx_z_z), !is.na(speed_z),
           !is.na(batter_right), !is.na(two_strike),
           !is.na(label_sl), !is.na(label_st), !is.na(pitcher_id))

  df_cc <- df_cc |>
    mutate(pitcher_idx = as.integer(factor(pitcher_id)))

  list(
    Y            = df_cc[[outcome_col]],
    pfx_x_z      = df_cc$pfx_x_z,
    pfx_z_z      = df_cc$pfx_z_z,
    speed_z      = df_cc$speed_z,
    batter_right = df_cc$batter_right,
    two_strike   = df_cc$two_strike,
    label_sl     = df_cc$label_sl,
    label_st     = df_cc$label_st,
    pitcher_id   = df_cc$pitcher_idx,
    N            = nrow(df_cc),
    J            = max(df_cc$pitcher_idx)
  )
}

make_jags_data_continuous <- function(df) {
  df_cc <- df |>
    filter(!is.na(woba),
           !is.na(pfx_x_z), !is.na(pfx_z_z), !is.na(speed_z),
           !is.na(batter_right), !is.na(two_strike),
           !is.na(label_sl), !is.na(label_st), !is.na(pitcher_id))

  df_cc <- df_cc |>
    mutate(pitcher_idx = as.integer(factor(pitcher_id)))

  list(
    Y            = df_cc$woba,
    pfx_x_z      = df_cc$pfx_x_z,
    pfx_z_z      = df_cc$pfx_z_z,
    speed_z      = df_cc$speed_z,
    batter_right = df_cc$batter_right,
    two_strike   = df_cc$two_strike,
    label_sl     = df_cc$label_sl,
    label_st     = df_cc$label_st,
    pitcher_id   = df_cc$pitcher_idx,
    N            = nrow(df_cc),
    J            = max(df_cc$pitcher_idx)
  )
}

params_monitor <- c(
  "beta_pfx_x", "beta_pfx_z", "beta_speed",
  "beta_R", "beta_2strike",
  "beta_sl", "beta_st", "beta_st_vs_sl",
  "mu_alpha", "tau_alpha"
)

inits_fn <- function() list(
  beta_pfx_x  = rnorm(1, 0, 0.1),
  beta_pfx_z  = rnorm(1, 0, 0.1),
  beta_speed  = rnorm(1, 0, 0.1),
  beta_R      = rnorm(1, 0, 0.1),
  beta_2strike = rnorm(1, 0, 0.1),
  beta_sl     = rnorm(1, 0, 0.1),
  beta_st     = rnorm(1, 0, 0.1),
  mu_alpha    = rnorm(1, 0, 0.1),
  tau_alpha   = rgamma(1, 1, 1)
)

run_model <- function(jags_data, model_file, params, n_chains = 3,
                      n_adapt = 1000, n_burnin = 5000, n_iter = 10000,
                      n_thin = 5) {
  jags(
    data               = jags_data,
    inits              = replicate(n_chains, inits_fn(), simplify = FALSE),
    parameters.to.save = params,
    model.file         = model_file,
    n.chains           = n_chains,
    n.adapt            = n_adapt,
    n.burnin           = n_burnin,
    n.iter             = n_iter,
    n.thin             = n_thin,
    parallel           = TRUE,
    verbose            = FALSE
  )
}

report_labels <- function(fit, name) {
  for (beta in c("beta_sl", "beta_st", "beta_st_vs_sl")) {
    s    <- fit$summary[beta, ]
    rhat <- fit$Rhat[[beta]]
    message(sprintf(
      "[%s] %s: mean=%.3f, 95%% CI=[%.3f, %.3f], Rhat=%.3f",
      name, beta, s["mean"], s["2.5%"], s["97.5%"], rhat
    ))
    if (rhat > 1.1) warning(sprintf("[%s] %s Rhat > 1.1 — not converged!", name, beta))
  }
}

check_convergence <- function(fit, name) {
  rhats <- unlist(fit$Rhat)
  bad   <- rhats[rhats > 1.1]
  if (length(bad) > 0) {
    warning(sprintf("[%s] %d node(s) with Rhat > 1.1: %s",
                    name, length(bad),
                    paste(names(bad), round(bad, 3), sep = "=", collapse = ", ")))
  } else {
    message(sprintf("[%s] All Rhat < 1.1 — convergence OK.", name))
  }
}

binary_jags <- file.path("jags", "hierarchical_model.jags")
linear_jags <- file.path("jags", "hierarchical_model_linear.jags")

message("Fitting Model A: whiff rate...")
data_wh <- make_jags_data_binary(clean, "whiff")
fit_wh  <- run_model(data_wh, binary_jags, params_monitor)
check_convergence(fit_wh, "whiff")
report_labels(fit_wh, "whiff")

message("Fitting Model B: chase rate...")
data_ch <- make_jags_data_binary(clean, "chase")
fit_ch  <- run_model(data_ch, binary_jags, params_monitor)
check_convergence(fit_ch, "chase")
report_labels(fit_ch, "chase")

message("Fitting Model C: wOBA against...")
data_woba <- make_jags_data_continuous(clean)
fit_woba  <- run_model(data_woba, linear_jags, c(params_monitor, "tau"))
check_convergence(fit_woba, "wOBA")
report_labels(fit_woba, "wOBA")

jags_results <- list(
  fit_whiff = fit_wh,
  fit_chase = fit_ch,
  fit_woba  = fit_woba
)

saveRDS(jags_results, "data/clean/jags_results.rds")
message("JAGS results saved to data/clean/jags_results.rds")
