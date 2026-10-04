#' Standardized Benchmark Battery Generator for SAE
#'
#' Generates 8 distinct structural benchmark datasets using the BPS Two-Stage Sampling Engine:
#' 1. ds01_continuous_linear: Standard Fay-Herriot baseline
#' 2. ds02_bounded_rate: Poverty rate / bounded proportion y in (0, 1)
#' 3. ds03_highdim_sparse: 25 covariates with sparse true signals (Podes / satellite noise)
#' 4. ds04_nonlinear_interaction: Severe functional misspecification (sine, quadratics, interaction)
#' 5. ds05_spatial_correlated: Spatial autoregressive (SAR, rho=0.65) with adjacency matrix W
#' 6. ds06_spatiotemporal_panel: D=50 x T=5 longitudinal panel with AR(1) temporal persistence
#' 7. ds07_extreme_outliers: Heavy-tailed Student-t shocks and disaster outliers
#' 8. ds08_nested_subarea: Multilevel hierarchy (Province -> Kabupaten)
#'
#' Each dataset contains:
#' - Direct estimate (y_dir)
#' - Taylor linearization sampling variance (psi_dir)
#' - Design Effect (deff_dir)
#' - Auxiliary covariates (x1, x2, ...)
#' - Exact Finite Population Ground Truth (y_true)
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(Matrix)
})

# Load the BPS sampling engine
source("/Volumes/work/_MainR/sae-benchmark-lab/generator/bps_sampling_engine.R")

output_dir <- "/Volumes/work/_MainR/sae-benchmark-lab/datasets"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("======================================================================\n")
cat("   SAE BENCHMARK BATTERY GENERATOR (BPS Two-Stage Cluster Design)    \n")
cat("======================================================================\n\n")

# Helper function to generate Queen adjacency matrix for 2D grid
generate_spatial_adj_matrix <- function(n_rows = 6, n_cols = 10) {
  n <- n_rows * n_cols
  W <- matrix(0, nrow = n, ncol = n)
  
  for (r in 1:n_rows) {
    for (c in 1:n_cols) {
      curr <- (r - 1) * n_cols + c
      # Check neighbors (Queen: 8-neighborhood)
      for (dr in -1:1) {
        for (dc in -1:1) {
          if (dr == 0 && dc == 0) next
          nr <- r + dr
          nc <- c + dc
          if (nr >= 1 && nr <= n_rows && nc >= 1 && nc <= n_cols) {
            neighbor <- (nr - 1) * n_cols + nc
            W[curr, neighbor] <- 1
          }
        }
      }
    }
  }
  # Row standardize
  rs <- rowSums(W)
  W_std <- W / ifelse(rs == 0, 1, rs)
  return(W_std)
}

manifest <- list()

# -----------------------------------------------------------------------------
# 1. ds01_continuous_linear (Baseline Fay-Herriot)
# -----------------------------------------------------------------------------
cat("[1/8] Generating ds01_continuous_linear...\n")
set.seed(101)
pop01 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 101)

# Covariates at district level
dist_info01 <- pop01 %>% distinct(district_id, province_id)
dist_info01$x1 <- rnorm(60, 0, 1)          # Standardized Nightlight
dist_info01$x2 <- runif(60, 0.4, 0.95)     # Literacy rate
dist_info01$x3 <- runif(60, 0.3, 0.85)     # Sanitation access

pop01 <- pop01 %>% left_join(dist_info01 %>% select(district_id, x1, x2, x3), by = "district_id")

# Area effect, cluster effect, household residual
u_d <- rnorm(60, 0, 0.15)
names(u_d) <- 1:60
c_bs <- rnorm(length(unique(pop01$bs_id)), 0, 0.12)
names(c_bs) <- unique(pop01$bs_id)

pop01$u <- u_d[as.character(pop01$district_id)]
pop01$c <- c_bs[pop01$bs_id]
pop01$e <- rnorm(nrow(pop01), 0, 0.35)

# Continuous response: log per-capita expenditure
pop01$y <- 13.8 + 0.35 * pop01$x1 + 0.25 * pop01$x2 + 0.20 * pop01$x3 + pop01$u + pop01$c + pop01$e

samp01 <- sample_two_stage_bps(pop01, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 101)
ds01 <- compute_bps_direct_and_ground_truth(samp01, pop01, response_col = "y")
ds01 <- ds01 %>% left_join(dist_info01 %>% select(district_id, x1, x2, x3), by = "district_id")

saveRDS(ds01, file.path(output_dir, "ds01_continuous_linear.rds"))
write.csv(ds01, file.path(output_dir, "ds01_continuous_linear.csv"), row.names = FALSE)

manifest[[1]] <- data.frame(
  id = "ds01_continuous_linear",
  name = "Continuous Linear (Baseline Fay-Herriot)",
  n_areas = 60,
  response_type = "continuous",
  target_description = "Log per-capita household expenditure",
  formula = "y ~ x1 + x2 + x3",
  challenge = "Baseline efficiency gain over Direct with standard normality",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 2. ds02_bounded_rate (Poverty Rate / Bounded Proportion)
# -----------------------------------------------------------------------------
cat("[2/8] Generating ds02_bounded_rate...\n")
set.seed(202)
pop02 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 202)

dist_info02 <- pop02 %>% distinct(district_id, province_id)
dist_info02$x1 <- rnorm(60, 0, 1)
dist_info02$x2 <- runif(60, 0.2, 0.8)
pop02 <- pop02 %>% left_join(dist_info02 %>% select(district_id, x1, x2), by = "district_id")

u_d02 <- rnorm(60, 0, 0.30)
names(u_d02) <- 1:60
c_bs02 <- rnorm(length(unique(pop02$bs_id)), 0, 0.20)
names(c_bs02) <- unique(pop02$bs_id)

pop02$u <- u_d02[as.character(pop02$district_id)]
pop02$c <- c_bs02[pop02$bs_id]

# Latent logit for household poverty
logit_p <- -1.8 - 0.55 * pop02$x1 - 0.40 * pop02$x2 + pop02$u + pop02$c
pop02$prob_poor <- 1 / (1 + exp(-logit_p))
pop02$y_binary <- rbinom(nrow(pop02), size = 1, prob = pop02$prob_poor)

samp02 <- sample_two_stage_bps(pop02, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 202)
ds02 <- compute_bps_direct_and_ground_truth(samp02, pop02, response_col = "y_binary", is_rate = TRUE)
ds02 <- ds02 %>% left_join(dist_info02 %>% select(district_id, x1, x2), by = "district_id")

saveRDS(ds02, file.path(output_dir, "ds02_bounded_rate.rds"))
write.csv(ds02, file.path(output_dir, "ds02_bounded_rate.csv"), row.names = FALSE)

manifest[[2]] <- data.frame(
  id = "ds02_bounded_rate",
  name = "Bounded Rate (Poverty / Prevalence Rate)",
  n_areas = 60,
  response_type = "bounded_rate",
  target_description = "Poverty rate bounded in (0, 1)",
  formula = "y ~ x1 + x2",
  challenge = "Predictions must remain strictly within (0, 1), avoiding boundary violations",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 3. ds03_highdim_sparse (25 Covariates: 3 True Signals, 22 Noise)
# -----------------------------------------------------------------------------
cat("[3/8] Generating ds03_highdim_sparse...\n")
set.seed(303)
pop03 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 303)

# 25 covariates
cov_mat <- matrix(rnorm(60 * 25), nrow = 60, ncol = 25)
colnames(cov_mat) <- paste0("x", 1:25)
dist_info03 <- as.data.frame(cov_mat)
dist_info03$district_id <- 1:60

pop03 <- pop03 %>% left_join(dist_info03, by = "district_id")

u_d03 <- rnorm(60, 0, 0.18)
names(u_d03) <- 1:60
c_bs03 <- rnorm(length(unique(pop03$bs_id)), 0, 0.12)
names(c_bs03) <- unique(pop03$bs_id)

# Only x1, x2, x3 have non-zero coefficients
pop03$y <- 10.0 + 0.65 * pop03$x1 - 0.50 * pop03$x2 + 0.40 * pop03$x3 + 
           u_d03[as.character(pop03$district_id)] + 
           c_bs03[pop03$bs_id] + 
           rnorm(nrow(pop03), 0, 0.35)

samp03 <- sample_two_stage_bps(pop03, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 303)
ds03 <- compute_bps_direct_and_ground_truth(samp03, pop03, response_col = "y")
ds03 <- ds03 %>% left_join(dist_info03, by = "district_id")

saveRDS(ds03, file.path(output_dir, "ds03_highdim_sparse.rds"))
write.csv(ds03, file.path(output_dir, "ds03_highdim_sparse.csv"), row.names = FALSE)

manifest[[3]] <- data.frame(
  id = "ds03_highdim_sparse",
  name = "High-Dimensional Sparse (Podes / Satellite Features)",
  n_areas = 60,
  response_type = "continuous",
  target_description = "Continuous index with 25 candidate auxiliary features",
  formula = "y ~ x1 + ... + x25 (3 true signals, 22 noise)",
  challenge = "Regularization, horseshoe shrinkage, feature screening vs overfitting",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 4. ds04_nonlinear_interaction (Severe Functional Misspecification)
# -----------------------------------------------------------------------------
cat("[4/8] Generating ds04_nonlinear_interaction...\n")
set.seed(404)
pop04 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 404)

dist_info04 <- pop04 %>% distinct(district_id, province_id)
dist_info04$x1 <- runif(60, -1, 1)
dist_info04$x2 <- runif(60, -1, 1)
dist_info04$x3 <- runif(60, 0, 2)
dist_info04$x4 <- runif(60, -2, 2)

pop04 <- pop04 %>% left_join(dist_info04 %>% select(district_id, x1, x2, x3, x4), by = "district_id")

u_d04 <- rnorm(60, 0, 0.15)
names(u_d04) <- 1:60
c_bs04 <- rnorm(length(unique(pop04$bs_id)), 0, 0.10)
names(c_bs04) <- unique(pop04$bs_id)

# Highly nonlinear true functional form with interaction
nl_signal <- 2.2 * sin(pi * pop04$x1) + 1.8 * (pop04$x2^2) - 2.5 * (pop04$x1 * pop04$x3) + 1.4 * sqrt(abs(pop04$x4))
pop04$y <- 8.0 + nl_signal + u_d04[as.character(pop04$district_id)] + c_bs04[pop04$bs_id] + rnorm(nrow(pop04), 0, 0.35)

samp04 <- sample_two_stage_bps(pop04, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 404)
ds04 <- compute_bps_direct_and_ground_truth(samp04, pop04, response_col = "y")
ds04 <- ds04 %>% left_join(dist_info04 %>% select(district_id, x1, x2, x3, x4), by = "district_id")

saveRDS(ds04, file.path(output_dir, "ds04_nonlinear_interaction.rds"))
write.csv(ds04, file.path(output_dir, "ds04_nonlinear_interaction.csv"), row.names = FALSE)

manifest[[4]] <- data.frame(
  id = "ds04_nonlinear_interaction",
  name = "Nonlinear & Interactions (MERF / Tree Territory)",
  n_areas = 60,
  response_type = "continuous",
  target_description = "Complex continuous target with sin, quadratic, and multiplicative interaction",
  formula = "y ~ x1 + x2 + x3 + x4 (nonlinear DGP)",
  challenge = "Linear Fay-Herriot suffers severe structural bias; tests MERF/RF-SAE",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 5. ds05_spatial_correlated (Spatial Autoregressive SAR)
# -----------------------------------------------------------------------------
cat("[5/8] Generating ds05_spatial_correlated...\n")
set.seed(505)
pop05 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 505)

# Generate Spatial Adjacency Matrix W (6 rows x 10 cols grid = 60 districts)
W_matrix <- generate_spatial_adj_matrix(n_rows = 6, n_cols = 10)
saveRDS(W_matrix, file.path(output_dir, "W_matrix.rds"))

dist_info05 <- pop05 %>% distinct(district_id, province_id)
dist_info05$x1 <- rnorm(60, 0, 1)
dist_info05$x2 <- runif(60, 0.3, 0.9)
pop05 <- pop05 %>% left_join(dist_info05 %>% select(district_id, x1, x2), by = "district_id")

# SAR area random effects: u = (I - rho * W)^(-1) * eps
rho_spatial <- 0.65
I_mat <- diag(60)
inv_sar <- solve(I_mat - rho_spatial * W_matrix)
eps_u <- rnorm(60, 0, 0.22)
u_spatial <- as.vector(inv_sar %*% eps_u)
names(u_spatial) <- 1:60

c_bs05 <- rnorm(length(unique(pop05$bs_id)), 0, 0.12)
names(c_bs05) <- unique(pop05$bs_id)

pop05$u <- u_spatial[as.character(pop05$district_id)]
pop05$c <- c_bs05[pop05$bs_id]
pop05$y <- 11.5 + 0.45 * pop05$x1 + 0.35 * pop05$x2 + pop05$u + pop05$c + rnorm(nrow(pop05), 0, 0.35)

samp05 <- sample_two_stage_bps(pop05, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 505)
ds05 <- compute_bps_direct_and_ground_truth(samp05, pop05, response_col = "y")
ds05 <- ds05 %>% left_join(dist_info05 %>% select(district_id, x1, x2), by = "district_id")

saveRDS(ds05, file.path(output_dir, "ds05_spatial_correlated.rds"))
write.csv(ds05, file.path(output_dir, "ds05_spatial_correlated.csv"), row.names = FALSE)

manifest[[5]] <- data.frame(
  id = "ds05_spatial_correlated",
  name = "Spatial Correlated (SAR / CAR Topology)",
  n_areas = 60,
  response_type = "continuous",
  target_description = "Continuous target with strong spatial autocorrelation (rho = 0.65)",
  formula = "y ~ x1 + x2 + spatial(W)",
  challenge = "Exploiting spatial neighbors via SAR/Besag vs non-spatial shrinkage",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 6. ds06_spatiotemporal_panel (D = 50 districts x T = 5 years)
# -----------------------------------------------------------------------------
cat("[6/8] Generating ds06_spatiotemporal_panel...\n")
set.seed(606)
n_dist_panel <- 50
n_years <- 5
year_labels <- 2021:2025

panel_records <- list()
phi_temporal <- 0.70

for (t in seq_len(n_years)) {
  yr <- year_labels[t]
  pop_t <- generate_synthetic_hierarchy(n_districts = n_dist_panel, n_provinces = 5, seed = 606 + t)
  
  if (t == 1) {
    u_prev <- rnorm(n_dist_panel, 0, 0.20)
  } else {
    u_prev <- phi_temporal * u_prev + rnorm(n_dist_panel, 0, 0.15)
  }
  names(u_prev) <- 1:n_dist_panel
  
  dist_info_t <- pop_t %>% distinct(district_id, province_id)
  dist_info_t$year <- yr
  dist_info_t$x1 <- rnorm(n_dist_panel, mean = 0.1 * t, sd = 1)
  dist_info_t$x2 <- runif(n_dist_panel, 0.4, 0.9)
  
  pop_t <- pop_t %>% left_join(dist_info_t %>% select(district_id, year, x1, x2), by = "district_id")
  c_bs_t <- rnorm(length(unique(pop_t$bs_id)), 0, 0.10)
  names(c_bs_t) <- unique(pop_t$bs_id)
  
  pop_t$u <- u_prev[as.character(pop_t$district_id)]
  pop_t$c <- c_bs_t[pop_t$bs_id]
  pop_t$y <- 12.0 + 0.05 * t + 0.40 * pop_t$x1 + 0.30 * pop_t$x2 + pop_t$u + pop_t$c + rnorm(nrow(pop_t), 0, 0.35)
  
  samp_t <- sample_two_stage_bps(pop_t, bs_sample_urban = 3, bs_sample_rural = 3, m_hh_per_bs = 10, seed = 606 + t)
  ds_t <- compute_bps_direct_and_ground_truth(samp_t, pop_t, response_col = "y")
  ds_t <- ds_t %>% left_join(dist_info_t %>% select(district_id, year, x1, x2), by = "district_id")
  
  panel_records[[t]] <- ds_t
}

ds06 <- do.call(rbind, panel_records) %>%
  arrange(district_id, year)

saveRDS(ds06, file.path(output_dir, "ds06_spatiotemporal_panel.rds"))
write.csv(ds06, file.path(output_dir, "ds06_spatiotemporal_panel.csv"), row.names = FALSE)

manifest[[6]] <- data.frame(
  id = "ds06_spatiotemporal_panel",
  name = "Spatio-Temporal Panel (D=50 x T=5)",
  n_areas = 250,
  response_type = "panel_continuous",
  target_description = "5-year longitudinal panel across 50 districts with AR(1) persistence",
  formula = "y ~ x1 + x2 + AR1(year)",
  challenge = "Borrowing strength simultaneously across time and areas (Rao-Yu / panel SAE)",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 7. ds07_extreme_outliers (Heavy-Tailed / Disaster Shock Outliers)
# -----------------------------------------------------------------------------
cat("[7/8] Generating ds07_extreme_outliers...\n")
set.seed(707)
pop07 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 707)

dist_info07 <- pop07 %>% distinct(district_id, province_id)
dist_info07$x1 <- rnorm(60, 0, 1)
dist_info07$x2 <- runif(60, 0.3, 0.8)
pop07 <- pop07 %>% left_join(dist_info07 %>% select(district_id, x1, x2), by = "district_id")

# Standard area effect for 56 districts, but 4 shock disaster districts
outlier_districts <- c(14, 27, 43, 58)
u_d07 <- rnorm(60, 0, 0.15)
# Economic shock: severe deviation
u_d07[outlier_districts] <- c(-1.15, 1.30, -1.25, 1.40) # 7-9 standard deviations out!
names(u_d07) <- 1:60

c_bs07 <- rnorm(length(unique(pop07$bs_id)), 0, 0.12)
names(c_bs07) <- unique(pop07$bs_id)

pop07$u <- u_d07[as.character(pop07$district_id)]
pop07$c <- c_bs07[pop07$bs_id]
pop07$e <- rnorm(nrow(pop07), 0, 0.35)

pop07$y <- 10.5 + 0.40 * pop07$x1 + 0.30 * pop07$x2 + pop07$u + pop07$c + pop07$e

samp07 <- sample_two_stage_bps(pop07, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 707)
ds07 <- compute_bps_direct_and_ground_truth(samp07, pop07, response_col = "y")
ds07 <- ds07 %>% left_join(dist_info07 %>% select(district_id, x1, x2), by = "district_id")
ds07$is_outlier <- ds07$district_id %in% outlier_districts

saveRDS(ds07, file.path(output_dir, "ds07_extreme_outliers.rds"))
write.csv(ds07, file.path(output_dir, "ds07_extreme_outliers.csv"), row.names = FALSE)

manifest[[7]] <- data.frame(
  id = "ds07_extreme_outliers",
  name = "Extreme Outliers & Shocks (Contaminated / Heavy-Tailed)",
  n_areas = 60,
  response_type = "continuous",
  target_description = "Continuous target with 4 disaster shock districts (+/- 8 sigma)",
  formula = "y ~ x1 + x2",
  challenge = "Robustness to severe contamination without distorting regular areas",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 8. ds08_nested_subarea (Province -> Kabupaten Hierarchical Model)
# -----------------------------------------------------------------------------
cat("[8/8] Generating ds08_nested_subarea...\n")
set.seed(808)
pop08 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 808)

dist_info08 <- pop08 %>% distinct(district_id, province_id)
dist_info08$x1 <- rnorm(60, 0, 1)
dist_info08$x2 <- runif(60, 0.25, 0.85)
pop08 <- pop08 %>% left_join(dist_info08 %>% select(district_id, x1, x2), by = "district_id")

# Two-level random effects: Province level v_p and District level u_pd
v_p <- rnorm(6, 0, 0.28)
names(v_p) <- 1:6
u_pd <- rnorm(60, 0, 0.16)
names(u_pd) <- 1:60

c_bs08 <- rnorm(length(unique(pop08$bs_id)), 0, 0.12)
names(c_bs08) <- unique(pop08$bs_id)

pop08$v <- v_p[as.character(pop08$province_id)]
pop08$u <- u_pd[as.character(pop08$district_id)]
pop08$c <- c_bs08[pop08$bs_id]
pop08$y <- 11.0 + 0.35 * pop08$x1 + 0.30 * pop08$x2 + pop08$v + pop08$u + pop08$c + rnorm(nrow(pop08), 0, 0.35)

samp08 <- sample_two_stage_bps(pop08, bs_sample_urban = 4, bs_sample_rural = 4, m_hh_per_bs = 10, seed = 808)
ds08 <- compute_bps_direct_and_ground_truth(samp08, pop08, response_col = "y")
ds08 <- ds08 %>% left_join(dist_info08 %>% select(district_id, x1, x2), by = "district_id")

saveRDS(ds08, file.path(output_dir, "ds08_nested_subarea.rds"))
write.csv(ds08, file.path(output_dir, "ds08_nested_subarea.csv"), row.names = FALSE)

manifest[[8]] <- data.frame(
  id = "ds08_nested_subarea",
  name = "Nested Subarea (Two-Fold Hierarchy: Province -> District)",
  n_areas = 60,
  response_type = "hierarchical_continuous",
  target_description = "Two-fold random effect structure (Province v_p + District u_pd)",
  formula = "y ~ x1 + x2 + (1 | province_id) + (1 | district_id)",
  challenge = "Two-fold subarea borrowing strength across both province and district levels",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Save Manifest
# -----------------------------------------------------------------------------
manifest_df <- do.call(rbind, manifest)
write.csv(manifest_df, file.path(output_dir, "datasets_manifest.csv"), row.names = FALSE)
saveRDS(manifest_df, file.path(output_dir, "datasets_manifest.rds"))

cat("\n======================================================================\n")
cat("SUCCESS! All 8 standardized benchmark datasets generated.\n")
cat("Location: /Volumes/work/_MainR/sae-benchmark-lab/datasets/\n")
cat("======================================================================\n")
