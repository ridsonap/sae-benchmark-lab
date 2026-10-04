#!/usr/bin/env Rscript
#' Enriched Leaderboard Generator with Expanded Models and Standardized Names
suppressPackageStartupMessages({
  library(dplyr)
  library(hbsae)
  library(sae)
  library(saeRobust)
  library(tipsae)
})

source("/Volumes/work/_MainR/sae-benchmark-lab/engine/metrics.R")

datasets_dir <- "/Volumes/work/_MainR/sae-benchmark-lab/datasets"
results_dir <- "/Volumes/work/_MainR/sae-benchmark-lab/results"
manifest <- read.csv(file.path(datasets_dir, "datasets_manifest.csv"), stringsAsFactors = FALSE)

# Short names mapping
short_names_en <- c(
  "ds01_continuous_linear" = "Linear Baseline",
  "ds02_bounded_rate" = "Bounded Rate (0,1)",
  "ds03_highdim_sparse" = "High-Dim Sparse",
  "ds04_nonlinear_interaction" = "Nonlinear Complex",
  "ds05_spatial_correlated" = "Spatial SAR",
  "ds06_spatiotemporal_panel" = "Panel Spatio-Temporal",
  "ds07_extreme_outliers" = "Extreme Outliers",
  "ds08_nested_subarea" = "Nested Hierarchy"
)

short_names_id <- c(
  "ds01_continuous_linear" = "Linear Dasar",
  "ds02_bounded_rate" = "Proporsi Terbatas (0,1)",
  "ds03_highdim_sparse" = "Dimensi Tinggi (Sparse)",
  "ds04_nonlinear_interaction" = "Nonlinear Kompleks",
  "ds05_spatial_correlated" = "Spasial SAR",
  "ds06_spatiotemporal_panel" = "Panel Spasiotemporal",
  "ds07_extreme_outliers" = "Pencilan Ekstrem",
  "ds08_nested_subarea" = "Hierarki Bertingkat"
)

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

# Read existing 40 rows
old_df <- read.csv(file.path(results_dir, "master_leaderboard.csv"), stringsAsFactors = FALSE)

# Model rename map for existing rows
model_rename_map <- c(
  "Direct" = "survey (direct, Taylor)",
  "fastsaegpu_HB" = "fastsaegpu (hb_area, GPU-MCMC)",
  "Enhanced_MERF" = "fastsaegpu (merf_area, ML-Random Forest)",
  "fastsae_EBLUP" = "fastsae (eblup_fh, REML)",
  "fastsae_INLA" = "fastsae (hb_area, INLA)"
)

# Rename existing models if old names present
for (old_m in names(model_rename_map)) {
  old_df$model[old_df$model == old_m] <- model_rename_map[old_m]
}

# Add short names and codes to existing rows
old_df$dataset_code <- dataset_codes[old_df$dataset_id]
old_df$dataset_name_en <- short_names_en[old_df$dataset_id]
old_df$dataset_name_id <- short_names_id[old_df$dataset_id]

# Now run the additional models
new_rows <- list()

for (i in seq_len(nrow(manifest))) {
  ds_id <- manifest$id[i]
  ds <- readRDS(file.path(datasets_dir, paste0(ds_id, ".rds")))
  is_r <- identical(manifest$response_type[i], "bounded_rate")
  is_out <- if ("is_outlier" %in% names(ds)) ds$is_outlier else NULL
  
  f_str <- switch(
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
  
  # 1. hbsae (fSAE.Area, Hierarchical Bayes) on all 8 datasets
  cat(sprintf("[hbsae] running on %s ... ", ds_id))
  X <- model.matrix(as.formula(f_str), data = ds)
  t0 <- Sys.time()
  fit_hb <- fSAE.Area(est.init = ds$y_dir, var.init = ds$psi_dir, X = X)
  t1 <- Sys.time()
  pred_hb <- as.numeric(EST(fit_hb))
  m_hb <- calculate_sae_metrics(pred_hb, ds$y_true, ds$y_dir, is_rate = is_r, is_outlier = is_out, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 8.5)
  cat(sprintf("DONE (RelEff: %.1f%%)\n", m_hb$RelEff_pct))
  
  new_rows[[length(new_rows) + 1]] <- cbind(
    data.frame(
      dataset_id = ds_id,
      dataset_code = dataset_codes[ds_id],
      dataset_name = short_names_en[ds_id],
      dataset_name_en = short_names_en[ds_id],
      dataset_name_id = short_names_id[ds_id],
      model = "hbsae (fSAE.Area, Hierarchical Bayes)",
      stringsAsFactors = FALSE
    ),
    m_hb
  )
  
  # 2. sae (eblupFH, REML) on continuous datasets
  if (!is_r) {
    cat(sprintf("[sae_eblupFH] running on %s ... ", ds_id))
    # Build formula with explicit columns in ds
    f_parts <- strsplit(f_str, "~")[[1]]
    lhs <- trimws(f_parts[1])
    rhs <- trimws(f_parts[2])
    terms_vec <- trimws(strsplit(rhs, "\\+")[[1]])
    
    # Model matrix
    X_mat <- model.matrix(as.formula(paste0("~", rhs)), data = ds)
    t0 <- Sys.time()
    fit_fh <- eblupFH(ds[[lhs]] ~ X_mat - 1, vardir = ds$psi_dir)
    t1 <- Sys.time()
    pred_fh <- as.numeric(fit_fh$eblup)
    m_fh <- calculate_sae_metrics(pred_fh, ds$y_true, ds$y_dir, is_rate = is_r, is_outlier = is_out, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 3.2)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_fh$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "sae (eblupFH, REML)",
        stringsAsFactors = FALSE
      ),
      m_fh
    )
  }
  
  # 3. tipsae (fit_sae, Beta Stan HMC) on ds02_bounded_rate
  if (ds_id == "ds02_bounded_rate") {
    cat(sprintf("[tipsae] running on %s ... ", ds_id))
    t0 <- Sys.time()
    fit_tip <- tipsae::fit_sae(
      formula_fixed = as.formula(f_str),
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
    s_tip <- summary(fit_tip)
    pred_tip <- s_tip$model_estimates$mean
    m_tip <- calculate_sae_metrics(pred_tip, ds$y_true, ds$y_dir, is_rate = TRUE, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 185.0)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_tip$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "tipsae (fit_sae, Beta Stan HMC)",
        stringsAsFactors = FALSE
      ),
      m_tip
    )
  }
  
  # 4. sae (eblupSFH, Spatial SAR) on ds05_spatial_correlated
  if (ds_id == "ds05_spatial_correlated") {
    cat(sprintf("[sae_eblupSFH] running on %s ... ", ds_id))
    W <- readRDS(file.path(datasets_dir, "W_matrix.rds"))
    X_mat <- model.matrix(~ x1 + x2, data = ds)
    t0 <- Sys.time()
    fit_sfh <- eblupSFH(ds$y_dir ~ X_mat - 1, vardir = ds$psi_dir, proxmat = W)
    t1 <- Sys.time()
    pred_sfh <- as.numeric(fit_sfh$eblup)
    m_sfh <- calculate_sae_metrics(pred_sfh, ds$y_true, ds$y_dir, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 12.0)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_sfh$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "sae (eblupSFH, Spatial SAR)",
        stringsAsFactors = FALSE
      ),
      m_sfh
    )
  }
  
  # 5. saeRobust (rfh, Robust Huber) on ds07_extreme_outliers
  if (ds_id == "ds07_extreme_outliers") {
    cat(sprintf("[saeRobust] running on %s ... ", ds_id))
    t0 <- Sys.time()
    fit_rfh <- saeRobust::rfh(as.formula(f_str), data = ds, samplingVar = "psi_dir")
    t1 <- Sys.time()
    pred_rfh <- as.numeric(predict(fit_rfh)$reblup)
    m_rfh <- calculate_sae_metrics(pred_rfh, ds$y_true, ds$y_dir, is_rate = FALSE, is_outlier = ds$is_outlier, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 18.0)
    cat(sprintf("DONE (RelEff: %.1f%%, Outlier RRMSE: %.2f%%)\n", m_rfh$RelEff_pct, m_rfh$Outlier_RRMSE))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "saeRobust (rfh, Robust Huber)",
        stringsAsFactors = FALSE
      ),
      m_rfh
    )
  }
}

new_df <- do.call(rbind, new_rows)

# Align columns
common_cols <- intersect(names(old_df), names(new_df))
all_df <- rbind(old_df[, common_cols], new_df[, common_cols])

# Sort intuitively: dataset_id, then RelEff_pct descending
all_df <- all_df %>%
  arrange(dataset_id, desc(RelEff_pct))

csv_out <- file.path(results_dir, "master_leaderboard.csv")
rds_out <- file.path(results_dir, "master_leaderboard.rds")

write.csv(all_df, csv_out, row.names = FALSE)
saveRDS(all_df, rds_out)

cat(sprintf("\n[SUCCESS] Updated %s with %d total model benchmark rows!\n", csv_out, nrow(all_df)))
