# 05_figures.R
# Produces publication-ready figures for the JQAS manuscript.
# All plots are saved to reports/figures/ as 300-dpi PNGs.
#
# Figure list:
#   F1  – Movement profile scatter (pfx_x_adj vs pfx_z), coloured by SI/FT
#   F2  – GMM cluster assignments (G=2) vs. pitch label — side-by-side scatter
#   F3  – BIC curve across G = 2..8
#   F4  – Posterior density of beta_label for each of the three outcomes
#   F5  – Caterpillar plot of pitcher random intercepts (ground ball model)

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)

clean        <- readRDS("data/clean/si_ft_clean.rds")
gmm_results  <- readRDS("data/clean/gmm_results.rds")
jags_results <- readRDS("data/clean/jags_results.rds")

theme_jqas <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    strip.background  = element_blank(),
    legend.position   = "bottom"
  )

save_fig <- function(p, name, width = 7, height = 5) {
  path <- file.path("reports", "figures", paste0(name, ".png"))
  ggsave(path, p, width = width, height = height, dpi = 300)
  message(sprintf("Saved %s", path))
}

# A random subsample keeps the scatter plots from overplotting.
set.seed(1)
sub <- clean |> slice_sample(n = min(30000, nrow(clean)))

# Figure 1: Raw movement profiles coloured by label.
f1 <- ggplot(sub, aes(pfx_x_adj, pfx_z, colour = pitch_type)) +
  geom_point(alpha = 0.15, size = 0.6) +
  scale_colour_manual(
    values = c(FT = "#E69F00", SI = "#0072B2"),
    labels = c(FT = "Two-seam (FT)", SI = "Sinker (SI)")
  ) +
  labs(
    x       = "Arm-side break (pfx_x, adj., inches)",
    y       = "Vertical break (pfx_z, inches)",
    colour  = NULL,
    title   = "Movement profiles: SI vs. FT (2020–2023)",
    caption = "pfx_x sign flipped for LHP so arm-side is always positive"
  ) +
  theme_jqas

save_fig(f1, "F1_movement_scatter")

# Figure 2: GMM G=2 clusters vs. label — faceted scatter.
sub2 <- sub |>
  mutate(cluster_g2 = factor(cluster_g2,
                             labels = c("Cluster 1", "Cluster 2")))

f2 <- ggplot(sub2, aes(pfx_x_adj, pfx_z, colour = pitch_type)) +
  geom_point(alpha = 0.15, size = 0.6) +
  facet_wrap(~ cluster_g2) +
  scale_colour_manual(
    values = c(FT = "#E69F00", SI = "#0072B2"),
    labels = c(FT = "FT", SI = "SI")
  ) +
  labs(
    x      = "Arm-side break (inches)",
    y      = "Vertical break (inches)",
    colour = "Statcast label",
    title  = "GMM clusters (G=2) vs. pitch label"
  ) +
  theme_jqas

save_fig(f2, "F2_gmm_vs_label")

# Figure 3: BIC curve.
bic_df <- data.frame(
  G   = 2:8,
  BIC = gmm_results$fit_bic$BIC[as.character(2:8),
                                  gmm_results$fit_bic$modelName]
)

f3 <- ggplot(bic_df, aes(G, BIC)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = gmm_results$fit_bic$G,
             linetype = "dashed", colour = "red") +
  scale_x_continuous(breaks = 2:8) +
  labs(
    x     = "Number of GMM components (G)",
    y     = "BIC",
    title = sprintf("GMM BIC curve (optimal G = %d, model = %s)",
                    gmm_results$fit_bic$G,
                    gmm_results$fit_bic$modelName)
  ) +
  theme_jqas

save_fig(f3, "F3_bic_curve", width = 6, height = 4)

# Figure 4: Posterior densities of beta_label across three outcomes.
extract_samples <- function(fit, outcome_label) {
  # jagsUI stores MCMC chains as an mcmc.list under fit$samples.
  samps <- do.call(rbind, lapply(fit$samples, as.data.frame))
  data.frame(value = samps[["beta_label"]], outcome = outcome_label)
}

post_df <- bind_rows(
  extract_samples(jags_results$fit_ground_ball, "Ground ball rate"),
  extract_samples(jags_results$fit_whiff,       "Whiff rate"),
  extract_samples(jags_results$fit_woba,        "wOBA against")
)

f4 <- ggplot(post_df, aes(value, fill = outcome, colour = outcome)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    x     = expression(beta["label"] ~ "(SI vs. FT effect, log-odds or raw)"),
    y     = "Posterior density",
    title = "Posterior distribution of SI/FT label effect",
    fill  = NULL, colour = NULL
  ) +
  theme_jqas +
  theme(legend.position = "none")

save_fig(f4, "F4_beta_label_posterior", width = 9, height = 4)

# Figure 5: Pitcher random intercepts — ground ball model, top/bottom 20.
alpha_cols <- grep("^alpha\\[", colnames(
  as.data.frame(do.call(rbind, lapply(jags_results$fit_ground_ball$samples,
                                      as.data.frame)))
), value = TRUE)

alpha_df <- jags_results$fit_ground_ball$summary[alpha_cols, ] |>
  as.data.frame() |>
  tibble::rownames_to_column("node") |>
  mutate(pitcher_idx = as.integer(sub("alpha\\[(\\d+)\\]", "\\1", node))) |>
  arrange(mean) |>
  slice(c(1:20, (n() - 19):n())) |>
  mutate(node = factor(node, levels = node))

f5 <- ggplot(alpha_df, aes(mean, node)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x     = "Pitcher random intercept (log-odds)",
    y     = "Pitcher (index)",
    title = "Top/bottom 20 pitcher random intercepts — ground ball model"
  ) +
  theme_jqas

save_fig(f5, "F5_pitcher_intercepts", width = 6, height = 7)

message("All figures written to reports/figures/")
