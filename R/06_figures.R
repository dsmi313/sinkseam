# 06_figures.R
# Produces publication-ready figures for the JQAS manuscript.
# All plots are saved to reports/figures/ as 300-dpi PNGs.
#
# Figure list:
#   F1  – Movement scatter (pfx_x_adj vs pfx_z) coloured by CU/SL/ST
#   F2  – PCA scores (PC1 vs PC2) coloured by pitch type, with loading arrows
#   F3  – GMM cluster assignments (G=3 and BIC-optimal) vs. label
#   F4  – BIC curve across G = 2..8
#   F5  – Posterior densities of beta_sl and beta_st for each outcome
#   F6  – Caterpillar plot of pitcher random intercepts (whiff model)

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)

clean        <- readRDS("data/clean/cu_sl_st_clean.rds")
pca_results  <- readRDS("data/clean/pca_results.rds")
gmm_results  <- readRDS("data/clean/gmm_results.rds")
jags_results <- readRDS("data/clean/jags_results.rds")

LABEL_COLOURS <- c(CU = "#0072B2", SL = "#D55E00", ST = "#009E73")
LABEL_NAMES   <- c(CU = "Curveball (CU)", SL = "Slider (SL)", ST = "Sweeper (ST)")

theme_jqas <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position  = "bottom"
  )

save_fig <- function(p, name, width = 7, height = 5) {
  path <- file.path("reports", "figures", paste0(name, ".png"))
  ggsave(path, p, width = width, height = height, dpi = 300)
  message(sprintf("Saved %s", path))
}

set.seed(1)
sub <- clean |> slice_sample(n = min(30000, nrow(clean)))

# Figure 1: Raw movement profiles coloured by label.
f1 <- ggplot(sub, aes(pfx_x_adj, pfx_z, colour = pitch_type)) +
  geom_point(alpha = 0.12, size = 0.5) +
  scale_colour_manual(values = LABEL_COLOURS, labels = LABEL_NAMES) +
  labs(
    x       = "Arm-side / glove-side break (pfx_x adj., inches)",
    y       = "Induced vertical break (pfx_z, inches)",
    colour  = NULL,
    title   = "Breaking ball movement profiles: CU, SL, ST (2022-2024)",
    caption = "pfx_x sign flipped for LHP; negative = glove-side for both handednesses"
  ) +
  theme_jqas

save_fig(f1, "F1_movement_scatter", width = 8, height = 5)

# Figure 2: PCA scores PC1 vs PC2 with loading arrows (biplot-style).
scores_sub <- pca_results$scores_df |>
  slice_sample(n = min(20000, nrow(pca_results$scores_df)))

# Scale arrows to be visible relative to score cloud.
arrow_scale <- max(abs(scores_sub[, c("PC1", "PC2")])) * 0.4
loadings_df <- as.data.frame(pca_results$pca$rotation[, 1:2]) |>
  tibble::rownames_to_column("feature") |>
  mutate(
    xend = PC1 * arrow_scale,
    yend = PC2 * arrow_scale,
    label = recode(feature,
                   pfx_x_z     = "Horiz. break",
                   pfx_z_z     = "Vert. break",
                   speed_z     = "Velocity",
                   spin_axis_z = "Spin axis")
  )

var1 <- round(pca_results$var_exp[1] * 100, 1)
var2 <- round(pca_results$var_exp[2] * 100, 1)

f2 <- ggplot(scores_sub, aes(PC1, PC2, colour = pitch_type)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_segment(
    data = loadings_df,
    aes(x = 0, y = 0, xend = xend, yend = yend),
    colour = "black", linewidth = 0.7,
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(
    data = loadings_df,
    aes(x = xend * 1.15, y = yend * 1.15, label = label),
    colour = "black", size = 3, inherit.aes = FALSE
  ) +
  scale_colour_manual(values = LABEL_COLOURS, labels = LABEL_NAMES) +
  labs(
    x      = sprintf("PC1 (%.1f%% variance)", var1),
    y      = sprintf("PC2 (%.1f%% variance)", var2),
    colour = NULL,
    title  = "PCA of breaking ball movement space"
  ) +
  theme_jqas

save_fig(f2, "F2_pca_biplot", width = 8, height = 5)

# Figure 3: GMM G=3 clusters vs. label — faceted scatter.
n_clust <- gmm_results$fit_g3$G
sub3 <- sub |>
  mutate(cluster_g3 = factor(cluster_g3,
                             labels = paste("Cluster", seq_len(n_clust))))

f3 <- ggplot(sub3, aes(pfx_x_adj, pfx_z, colour = pitch_type)) +
  geom_point(alpha = 0.12, size = 0.5) +
  facet_wrap(~ cluster_g3, nrow = 1) +
  scale_colour_manual(values = LABEL_COLOURS, labels = LABEL_NAMES) +
  labs(
    x      = "Arm-side / glove-side break (inches)",
    y      = "Vertical break (inches)",
    colour = "Statcast label",
    title  = "GMM clusters (forced G=3) vs. pitch label"
  ) +
  theme_jqas

save_fig(f3, "F3_gmm_g3_vs_label", width = 10, height = 4)

# Figure 4: BIC curve.
bic_df <- data.frame(
  G   = 2:8,
  BIC = gmm_results$fit_bic$BIC[as.character(2:8),
                                  gmm_results$fit_bic$modelName]
)

f4 <- ggplot(bic_df, aes(G, BIC)) +
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

save_fig(f4, "F4_bic_curve", width = 6, height = 4)

# Figure 5: Posterior densities — beta_sl and beta_st for all three outcomes.
extract_two <- function(fit, outcome_label) {
  samps <- do.call(rbind, lapply(fit$samples, as.data.frame))
  bind_rows(
    data.frame(value = samps[["beta_sl"]], contrast = "SL vs. CU",
               outcome = outcome_label),
    data.frame(value = samps[["beta_st"]], contrast = "ST vs. CU",
               outcome = outcome_label),
    data.frame(value = samps[["beta_st_vs_sl"]], contrast = "ST vs. SL",
               outcome = outcome_label)
  )
}

post_df <- bind_rows(
  extract_two(jags_results$fit_whiff, "Whiff rate"),
  extract_two(jags_results$fit_chase, "Chase rate"),
  extract_two(jags_results$fit_woba,  "wOBA against")
) |>
  mutate(
    outcome  = factor(outcome,  levels = c("Whiff rate", "Chase rate", "wOBA against")),
    contrast = factor(contrast, levels = c("SL vs. CU", "ST vs. CU", "ST vs. SL"))
  )

CONTRAST_COLOURS <- c("SL vs. CU" = "#D55E00",
                       "ST vs. CU" = "#009E73",
                       "ST vs. SL" = "#CC79A7")

f5 <- ggplot(post_df, aes(value, colour = contrast, fill = contrast)) +
  geom_density(alpha = 0.25, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(outcome ~ contrast, scales = "free") +
  scale_colour_manual(values = CONTRAST_COLOURS) +
  scale_fill_manual(values = CONTRAST_COLOURS) +
  labs(
    x      = expression(beta ~ "(log-odds or raw)"),
    y      = "Posterior density",
    title  = "Posterior distributions of pitch-type label effects",
    colour = NULL, fill = NULL
  ) +
  theme_jqas +
  theme(legend.position = "none")

save_fig(f5, "F5_beta_posteriors", width = 9, height = 7)

# Figure 6: Pitcher random intercepts — whiff model, top/bottom 20.
alpha_cols <- grep("^alpha\\[", colnames(
  as.data.frame(do.call(rbind, lapply(jags_results$fit_whiff$samples,
                                      as.data.frame)))
), value = TRUE)

alpha_df <- jags_results$fit_whiff$summary[alpha_cols, ] |>
  as.data.frame() |>
  tibble::rownames_to_column("node") |>
  mutate(pitcher_idx = as.integer(sub("alpha\\[(\\d+)\\]", "\\1", node))) |>
  arrange(mean) |>
  slice(c(1:20, (n() - 19):n())) |>
  mutate(node = factor(node, levels = node))

f6 <- ggplot(alpha_df, aes(mean, node)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x     = "Pitcher random intercept (log-odds of whiff)",
    y     = "Pitcher (index)",
    title = "Top/bottom 20 pitcher random intercepts — whiff model"
  ) +
  theme_jqas

save_fig(f6, "F6_pitcher_intercepts", width = 6, height = 7)

message("All figures written to reports/figures/")
