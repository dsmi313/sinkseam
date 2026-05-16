# 04_gmm.R
# Gaussian mixture model on standardized movement and speed features to test
# whether data-driven clusters align with CU/SL/ST labels.
#
# Three forced solutions are compared alongside the BIC-optimal fit:
#   fit_g2  – forced G=2: is there even a binary split?
#   fit_g3  – forced G=3: do three clusters match the three-label taxonomy?
#   fit_bic – BIC-optimal across G = 2..8
#
# Alignment is measured with the adjusted Rand index (ARI).  An ARI near 0
# means the label boundaries do not correspond to natural movement clusters.
#
# Set INCLUDE_SPIN_AXIS = TRUE to add spin axis as a 4th clustering feature.

library(dplyr)
library(mclust)

INCLUDE_SPIN_AXIS <- FALSE

clean <- readRDS("data/clean/cu_sl_st_clean.rds")

feature_cols <- c("pfx_x_z", "pfx_z_z", "speed_z")
if (INCLUDE_SPIN_AXIS) feature_cols <- c(feature_cols, "spin_axis_z")

features <- clean |>
  select(all_of(feature_cols)) |>
  as.matrix()

message(sprintf("GMM features: %s", paste(feature_cols, collapse = ", ")))

set.seed(42)

fit_bic <- Mclust(features, G = 2:8, verbose = FALSE)
message(sprintf(
  "BIC-optimal: G = %d, model = %s (BIC = %.1f)",
  fit_bic$G, fit_bic$modelName, fit_bic$bic
))

fit_g2 <- Mclust(features, G = 2, modelNames = fit_bic$modelName, verbose = FALSE)
fit_g3 <- Mclust(features, G = 3, modelNames = fit_bic$modelName, verbose = FALSE)

ari_bic <- adjustedRandIndex(fit_bic$classification, clean$pitch_type)
ari_g2  <- adjustedRandIndex(fit_g2$classification,  clean$pitch_type)
ari_g3  <- adjustedRandIndex(fit_g3$classification,  clean$pitch_type)

message(sprintf("ARI (BIC-optimal, G=%d): %.3f", fit_bic$G, ari_bic))
message(sprintf("ARI (forced G=2):        %.3f", ari_g2))
message(sprintf("ARI (forced G=3):        %.3f", ari_g3))

clean <- clean |>
  mutate(
    cluster_bic     = fit_bic$classification,
    cluster_g2      = fit_g2$classification,
    cluster_g3      = fit_g3$classification,
    cluster_g3_prob = apply(fit_g3$z, 1, max)
  )

tab_g2 <- table(GMM_cluster = clean$cluster_g2, Label = clean$pitch_type)
tab_g3 <- table(GMM_cluster = clean$cluster_g3, Label = clean$pitch_type)

message("Contingency table — G=2:")
print(tab_g2)
message("Contingency table — G=3:")
print(tab_g3)

purity_g3 <- sum(apply(tab_g3, 1, max)) / sum(tab_g3)
message(sprintf("Cluster purity (G=3): %.3f", purity_g3))

gmm_results <- list(
  fit_bic           = fit_bic,
  fit_g2            = fit_g2,
  fit_g3            = fit_g3,
  ari_bic           = ari_bic,
  ari_g2            = ari_g2,
  ari_g3            = ari_g3,
  purity_g3         = purity_g3,
  tab_g2            = tab_g2,
  tab_g3            = tab_g3,
  feature_cols      = feature_cols,
  include_spin_axis = INCLUDE_SPIN_AXIS
)

saveRDS(gmm_results, "data/clean/gmm_results.rds")
saveRDS(clean,       "data/clean/cu_sl_st_clean.rds")
message("GMM results saved to data/clean/gmm_results.rds")
