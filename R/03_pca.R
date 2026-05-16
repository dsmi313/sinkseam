# 03_pca.R
# Principal component analysis on standardized movement and speed features.
# Purpose: visualize whether CU/SL/ST occupy distinct regions of the
# movement-speed space or form a single elongated continuum.
#
# The CU -> SL -> ST continuum hypothesis predicts that PC1 captures the
# trade-off between vertical drop (CU end) and horizontal break (ST end),
# producing one elongated cloud in PC space rather than three separable clusters.
#
# Set INCLUDE_SPIN_AXIS = TRUE to add spin axis as a 4th feature (see concept
# paper open question on whether spin axis is the defining ST/SL boundary).
#
# Saves: data/clean/pca_results.rds

library(dplyr)

INCLUDE_SPIN_AXIS <- FALSE

clean <- readRDS("data/clean/cu_sl_st_clean.rds")

feature_cols <- c("pfx_x_z", "pfx_z_z", "speed_z")
if (INCLUDE_SPIN_AXIS) feature_cols <- c(feature_cols, "spin_axis_z")

# Keep only complete cases for the chosen features.
complete_mask <- complete.cases(clean[, feature_cols])
features      <- as.matrix(clean[complete_mask, feature_cols])

message(sprintf(
  "PCA on %d pitches, features: %s",
  nrow(features), paste(feature_cols, collapse = ", ")
))

# Features are already standardized (mean=0, SD=1), so no rescaling needed.
pca <- prcomp(features, center = FALSE, scale. = FALSE)

var_exp <- summary(pca)$importance["Proportion of Variance", ]
cum_var <- summary(pca)$importance["Cumulative Proportion",  ]

message(sprintf("PC1 explains %.1f%% of variance", var_exp[1] * 100))
message(sprintf("PC1+PC2 explain %.1f%% of variance", cum_var[2] * 100))
message("Loadings (rotation):")
print(round(pca$rotation, 3))

# Attach scores and labels for plotting.
scores_df <- as.data.frame(pca$x) |>
  mutate(
    pitch_type = clean$pitch_type[complete_mask],
    p_throws   = clean$p_throws[complete_mask]
  )

pca_results <- list(
  pca               = pca,
  scores_df         = scores_df,
  var_exp           = var_exp,
  cum_var           = cum_var,
  feature_cols      = feature_cols,
  include_spin_axis = INCLUDE_SPIN_AXIS
)

saveRDS(pca_results, "data/clean/pca_results.rds")
message("PCA results saved to data/clean/pca_results.rds")
