# 03_gmm.R
# Gaussian mixture model on (pfx_x_adj, pfx_z, release_speed) — all
# standardized — to ask whether data-driven clusters align with SI/FT labels.
#
# Strategy:
#   1. Fit GMMs with G = 2..8 components via mclust::Mclust (BIC selection).
#   2. Retain the BIC-optimal model and the forced-G=2 model for comparison.
#   3. Compute adjusted Rand index (ARI) between GMM hard assignments and
#      the SI/FT label to quantify label–cluster alignment.
#   4. Save results and the cluster-assignment column to data/clean/.

library(dplyr)
library(mclust)

clean <- readRDS("data/clean/si_ft_clean.rds")

features <- clean |>
  select(pfx_x_z, pfx_z_z, speed_z) |>
  as.matrix()

set.seed(42)

# BIC-optimal model across G = 2..8 and all mclust covariance parameterizations.
fit_bic <- Mclust(features, G = 2:8, verbose = FALSE)
message(sprintf(
  "BIC-optimal model: G = %d, model = %s (BIC = %.1f)",
  fit_bic$G, fit_bic$modelName, fit_bic$bic
))

# Forced two-component model using the same covariance structure as the winner.
fit_g2 <- Mclust(features, G = 2, modelNames = fit_bic$modelName,
                 verbose = FALSE)

# Adjusted Rand Index: 1 = perfect alignment, 0 = no better than chance.
ari_bic <- adjustedRandIndex(fit_bic$classification, clean$pitch_type)
ari_g2  <- adjustedRandIndex(fit_g2$classification,  clean$pitch_type)
message(sprintf("ARI (BIC-optimal, G=%d): %.3f", fit_bic$G, ari_bic))
message(sprintf("ARI (forced G=2):        %.3f", ari_g2))

# Attach cluster assignments to clean data.
clean <- clean |>
  mutate(
    cluster_bic = fit_bic$classification,
    cluster_g2  = fit_g2$classification,
    # Soft membership: posterior probability of the most likely cluster.
    cluster_g2_prob = apply(fit_g2$z, 1, max)
  )

# Contingency table: rows = GMM cluster (G=2), cols = pitch label.
tab <- table(GMM_cluster = clean$cluster_g2, Label = clean$pitch_type)
message("Contingency table (forced G=2 vs. pitch label):")
print(tab)

# Purity: fraction of the majority label within each cluster.
purity <- sum(apply(tab, 1, max)) / sum(tab)
message(sprintf("Cluster purity (G=2): %.3f", purity))

gmm_results <- list(
  fit_bic  = fit_bic,
  fit_g2   = fit_g2,
  ari_bic  = ari_bic,
  ari_g2   = ari_g2,
  purity   = purity,
  tab_g2   = tab
)

saveRDS(gmm_results, "data/clean/gmm_results.rds")
saveRDS(clean,       "data/clean/si_ft_clean.rds")  # overwrite with cluster cols
message("GMM results saved to data/clean/gmm_results.rds")
