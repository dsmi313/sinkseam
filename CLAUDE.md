# sinkseam — Project Context for Claude Code

## What this project does

Empirical test of whether the breaking-ball taxonomy in MLB Statcast is
supported by data. Specifically: do the curveball (CU), slider (SL), and
sweeper (ST) represent meaningfully distinct pitch types, or are they points
on a continuous movement spectrum?

## Pitch types and year range

**Target labels:** `CU`, `SL`, `ST` — three labels, not two.

```r
pitch_type %in% c("CU", "SL", "ST")
```

Do **not** filter for `SI` or `FT`. Those were a previous version of this
project and are no longer relevant.

**Year range:** `2022:2024`

```r
YEARS <- 2022:2024
```

`ST` (sweeper) did not exist as a Statcast label before 2022. Using earlier
years would mean the sweeper bucket is empty or contains mislabeled pitches.
`CU` and `SL` go back further, but 2022 is the earliest year where all three
labels coexist cleanly.

## Analysis pipeline (in order)

| Script | Purpose |
|---|---|
| `R/01_data_pull.R` | Pull raw Statcast data via `baseballr` |
| `R/02_clean.R` | Feature engineering, outcome construction, dummy labels |
| `R/03_pca.R` | PCA — visualize continuum vs. cluster structure |
| `R/04_gmm.R` | GMM via `mclust` — formal cluster identification |
| `R/05_jags_model.R` | Bayesian hierarchical model via `jagsUI` |
| `R/06_figures.R` | Six publication-ready PNGs → `reports/figures/` |

## Key modeling decisions

**`pfx_x` sign convention:** flip for LHP so glove-side break is consistently
directed. After adjustment, arm-side = positive, glove-side (the dominant
direction for CU/SL/ST) = negative.

```r
pfx_x_adj = if_else(p_throws == "L", -pfx_x, pfx_x)
```

**Label encoding:** CU is the reference category. Two binary dummies:

```r
label_sl = as.integer(pitch_type == "SL")   # 1=SL, 0=CU or ST
label_st = as.integer(pitch_type == "ST")   # 1=ST, 0=CU or SL
# CU: both = 0
```

**JAGS estimands** (three per outcome model):
- `beta_sl` — SL vs CU effect, controlling for movement
- `beta_st` — ST vs CU effect, controlling for movement
- `beta_st_vs_sl <- beta_st - beta_sl` — derived contrast, computed inside JAGS

If all three 95% credible intervals include zero, the taxonomy is unsupported.

**Priors:** Kéry & Schaub (2012) flat priors: `dnorm(0, 0.001)` for fixed
effects, `dgamma(0.001, 0.001)` for precision parameters.

**Convergence criterion:** Gelman–Rubin R̂ < 1.1 for all monitored nodes.

## Outcomes

| Column | Type | Definition |
|---|---|---|
| `whiff` | Binary | Swing-and-miss or foul-tip |
| `chase` | Binary | Swing on out-of-zone pitch (zones 11–14); NA for in-zone |
| `woba` | Continuous | Linear-weight wOBA, PA-ending pitches only |

## Optional spin axis feature

`release_spin_axis` is pulled and standardized as `spin_axis_z`. Set
`INCLUDE_SPIN_AXIS <- TRUE` in `03_pca.R` and `04_gmm.R` to add it as a
4th feature. Default is FALSE.

## Target journal

*Journal of Quantitative Analysis in Sports* (JQAS, De Gruyter)

## What NOT to do

- Do not change the pitch type filter to `c("SI", "FT")` or `c("SL", "ST")` —
  the current three-class filter `c("CU", "SL", "ST")` is correct.
- Do not change `YEARS` to anything before 2022 — ST does not exist earlier.
- Do not revert to a single `beta_label` binary in the JAGS model — the
  three-class design requires two dummies (`beta_sl`, `beta_st`) with CU as
  the reference category.
