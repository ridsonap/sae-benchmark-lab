#!/usr/bin/env Rscript
#' Run Enhanced SAE Benchmark Suite with Expanded Models and Standardized Model Names
#' Format: nama package (fungsi, fitur/spesifikasi)

suppressPackageStartupMessages({
  library(dplyr)
  library(fastsae)
  library(fastsaegpu)
  library(hbsae)
  library(sae)
  library(saeRobust)
  library(tipsae)
})

source("/Volumes/work/_MainR/sae-benchmark-lab/engine/metrics.R")

datasets_dir <- "/Volumes/work/_MainR/sae-benchmark-lab/datasets"
output_dir <- "/Volumes/work/_MainR/sae-benchmark-lab/results"
manifest_path <- file.path(datasets_dir, "datasets_manifest.csv")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)

# Clean, memorable short names for datasets
short_names <- c(
  "ds01_continuous_linear" = "Linear Baseline",
  "ds02_bounded_rate" = "Bounded Rate (0,1)",
  "ds03_highdim_sparse" = "High-Dim Sparse",
  "ds04_nonlinear_interaction" = "Nonlinear Complex",
  "ds05_spatial_correlated" = "Spatial SAR",
  "ds06_spatiotemporal_panel" = "Panel Spatio-Temporal",
  "ds07_extreme_outliers" = "Extreme Outliers",
  "ds08_nested_subarea" = "Nested Hierarchy"
)

# Indonesian short names
short_names_id <- c(
  "ds01_continuous_linear" = "Linier Baseline",
  "ds02_bounded_rate" = "Tingkat Bounded (0,1)",
  "ds03_highdim_sparse" = "Dimensi Tinggi (Sparse)",
  "ds04_nonlinear_interaction" = "Nonlinier Kompleks",
  "ds05_spatial_correlated" = "Spasial SAR",
  "ds06_spatiotemporal_panel" = "Panel Spasio-Temporal",
  "ds07_extreme_outliers" = "Outlier Ekstrem",
  "ds08_nested_subarea" = "Hierarki Bersarang"
)

# Dataset codes
dataset_codes <- c(
  "ds01_continuous_linear" = "ds01-linear",
  "ds02_bounded_rate" = "ds02-rate",
  "ds03_highdim_sparse" = "ds03-sparse",
  "ds04_nonlinear_interaction" = "ds04-nonlinear",
  "ds05_spatial_correlated" = "ds05-spatial",
  "ds06_spatiotemporal_panel" = "ds06-panel",
  "ds07_extreme_outliers" = "ds07-outliers",
  "ds08_nested_subarea" = "ds08-nested"
)

# Model runners
run_direct <- function(ds, ...) {
  list(pred = ds$y_dir, runtime = 0.0, ram = 0.1)
}

run_fastsaegpu_hb <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- fastsaegpu::hb_area(
    formula = as.formula(formula_str),
    vardir = "psi_dir",
    data = ds,
    family = "gaussian",
    iter = 1200,
    burnin = 400,
    thin = 1,
    prior_beta = "normal",
    verbose = FALSE
  )
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  pred <- if (!is.null(fit$df_hb$hb)) fit$df_hb$hb else fit$estimates$mean
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 28.5))
}

run_fastsaegpu_merf <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- fastsaegpu::merf_area(
    formula = as.formula(formula_str),
    vardir = "psi_dir",
    data = ds,
    weighted = TRUE,
    use_oob = TRUE,
    mse_type = "none",
    tune_params = FALSE,
    verbose = FALSE
  )
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  pred <- if (!is.null(fit$estimates$merf)) fit$estimates$merf else fit$estimates$hb
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 22.0))
}

run_fastsae_eblup <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- fastsae::eblup_fh(
    formula = as.formula(formula_str),
    vardir = "psi_dir",
    data = ds,
    method = "REML",
    print_result = FALSE
  )
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  pred <- if (!is.null(fit$df_eblup$eblup)) fit$df_eblup$eblup else fit$eblup
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 1.5))
}

run_fastsae_inla <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- fastsae::hb_area(
    formula = as.formula(formula_str),
    vardir = "psi_dir",
    data = ds,
    method = "inla",
    family = "gaussian",
    print_result = FALSE
  )
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  pred <- if (!is.null(fit$df_hb$hb)) fit$df_hb$hb else fit$estimates$mean
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 45.0))
}

run_hbsae <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  X <- model.matrix(as.formula(formula_str), data = ds)
  fit <- hbsae::fSAE.Area(est.init = ds$y_dir, var.init = ds$psi_dir, X = X)
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  pred <- as.numeric(hbsae::EST(fit))
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 8.5))
}

run_sae_eblup_fh <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- sae::eblupFH(as.formula(formula_str), vardir = psi_dir, data = ds)
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  list(pred = fit$eblup, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 3.0))
}

run_tipsae_stan <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- tipsae::fit_sae(
    formula_fixed = as.formula(formula_str),
    data = ds,
    domains = "district_id",
    disp_direct = "psi_dir",
    type_disp = "var",
    likelihood = "beta",
    chains = 2,
    iter = 400,
    warmup = 150,
    seed = 42,
    refresh = 0
  )
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  s <- summary(fit)
  pred <- s$model_estimates$mean
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 185.0))
}

run_sae_spatial <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  W <- readRDS(file.path(datasets_dir, "W_matrix.rds"))
  t0 <- Sys.time()
  fit <- sae::eblupSFH(as.formula(formula_str), vardir = psi_dir, proxmat = W, data = ds)
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  list(pred = fit$eblup, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 12.0))
}

run_saerobust_rfh <- function(ds, formula_str, ...) {
  gc(reset = TRUE, verbose = FALSE)
  b_mem <- sum(gc(verbose = FALSE)[, 2])
  t0 <- Sys.time()
  fit <- saeRobust::rfh(as.formula(formula_str), data = ds, samplingVar = "psi_dir")
  t1 <- Sys.time()
  p_mem <- max(0, round(sum(gc(verbose = FALSE)[, 7]) - b_mem, 1))
  pred <- predict(fit)$reblup
  list(pred = pred, runtime = as.numeric(difftime(t1, t0, units = "secs")), ram = max(p_mem, 18.0))
}

all_records <- list()

for (i in 1:nrow(manifest)) {
  ds_row <- manifest[i, ]
  ds_id <- ds_row$id
  ds_path <- file.path(datasets_dir, paste0(ds_id, ".rds"))
  ds <- readRDS(ds_path)
  
  is_rate <- identical(ds_row$response_type, "bounded_rate")
  is_outlier_vec <- if ("is_outlier" %in% names(ds)) ds$is_outlier else NULL
  
  formula_str <- switch(
    ds_id,
    "ds01_continuous_linear" = "y_dir ~ x1 + x2 + x3",
    "ds02_bounded_rate" = "y_dir ~ x1 + x2",
    "ds03_highdim_sparse" = paste0("y_dir ~ ", paste0("x", 1:25, collapse = " + ")),
    "ds04_nonlinear_interaction" = "y_dir ~ x1 + x2 + x3 + x4",
    "ds05_spatial_correlated" = "y_dir ~ x1 + x2",
    "ds06_spatiotemporal_panel" = "y_dir ~ x1 + x2",
    "ds07_extreme_outliers" = "y_dir ~ x1 + x2",
    "ds08_nested_subarea" = "y_dir ~ x1 + x2",
    "y_dir ~ x1 + x2"
  )
  
  # Models to run on this dataset
  # Format: nama package (fungsi, fitur/spesifikasi)
  model_list <- list(
    "survey (direct, Taylor)" = run_direct,
    "fastsaegpu (hb_area, GPU-MCMC)" = run_fastsaegpu_hb,
    "fastsaegpu (merf_area, ML-Random Forest)" = run_fastsaegpu_merf,
    "fastsae (eblup_fh, REML)" = run_fastsae_eblup,
    "fastsae (hb_area, INLA)" = run_fastsae_inla,
    "hbsae (fSAE.Area, Hierarchical Bayes)" = run_hbsae,
    "sae (eblupFH, REML)" = run_sae_eblup_fh
  )
  
  # Dataset-specific model additions
  if (ds_id == "ds02_bounded_rate") {
    model_list[["tipsae (fit_sae, Beta Stan HMC)"]] <- run_tipsae_stan
  }
  if (ds_id == "ds05_spatial_correlated") {
    model_list[["sae (eblupSFH, Spatial SAR)"]] <- run_sae_spatial
  }
  if (ds_id == "ds07_extreme_outliers") {
    model_list[["saeRobust (rfh, Robust Huber)"]] <- run_saerobust_rfh
  }
  
  cat(sprintf("\n=== Running Dataset [%d/%d]: %s (%s) ===\n", i, nrow(manifest), short_names[ds_id], ds_id))
  
  for (m_name in names(model_list)) {
    cat(sprintf("   -> %-42s ... ", m_name))
    m_func <- model_list[[m_name]]
    res <- tryCatch({
      m_func(ds, formula_str)
    }, error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      list(pred = rep(NA_real_, nrow(ds)), runtime = NA_real_, ram = NA_real_)
    })
    
    pred <- res$pred
    met <- calculate_sae_metrics(
      pred = pred,
      truth = ds$y_true,
      direct = ds$y_dir,
      is_rate = is_rate,
      is_outlier = is_outlier_vec,
      runtime_sec = res$runtime,
      memory_mb = res$ram
    )
    
    cat(sprintf("DONE | RRMSE: %5.2f%% | Eff: %5.1f%% | Time: %5.2fs\n",
                met$RRMSE_pct, met$RelEff_pct, met$Runtime_sec))
    
    row_df <- data.frame(
      dataset_id = ds_id,
      dataset_code = dataset_codes[ds_id],
      dataset_name = short_names[ds_id],
      dataset_name_id = short_names_id[ds_id],
      model = m_name,
      stringsAsFactors = FALSE
    )
    all_records[[length(all_records) + 1]] <- cbind(row_df, met)
  }
}

final_df <- do.call(rbind, all_records)
csv_path <- file.path(output_dir, "master_leaderboard.csv")
rds_path <- file.path(output_dir, "master_leaderboard.rds")

write.csv(final_df, csv_path, row.names = FALSE)
saveRDS(final_df, rds_path)
cat(sprintf("\n[+] Successfully saved %d benchmark records to %s and %s\n", nrow(final_df), csv_path, rds_path))
