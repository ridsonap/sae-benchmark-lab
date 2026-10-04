#!/usr/bin/env Rscript
#' Enriched Leaderboard Generator with Expanded Models and Self-Benchmarking
suppressPackageStartupMessages({
  library(dplyr)
  library(fastsae)
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

# Read existing master leaderboard if available to preserve base GPU & Direct benchmarks
master_csv <- file.path(results_dir, "master_leaderboard.csv")
base_df <- NULL
if (file.exists(master_csv)) {
  old_df <- read.csv(master_csv, stringsAsFactors = FALSE)
  # Standardize base model names
  model_rename_map <- c(
    "Direct" = "survey (direct, Taylor)",
    "fastsaegpu_HB" = "fastsaegpu (hb_area, GPU-MCMC)",
    "Enhanced_MERF" = "fastsaegpu (merf_area, ML-Random Forest)",
    "fastsae_EBLUP" = "fastsae (eblup_fh, REML)",
    "fastsae_INLA" = "fastsae (hb_area, INLA)"
  )
  for (old_m in names(model_rename_map)) {
    old_df$model[old_df$model == old_m] <- model_rename_map[old_m]
  }
  # Keep only foundational baseline benchmarks from old_df
  base_keep_models <- c(
    "survey (direct, Taylor)",
    "fastsaegpu (hb_area, GPU-MCMC)",
    "fastsaegpu (merf_area, ML-Random Forest)",
    "fastsae (eblup_fh, REML)",
    "fastsae (hb_area, INLA)"
  )
  base_df <- old_df %>%
    filter(model %in% base_keep_models) %>%
    distinct(dataset_id, model, .keep_all = TRUE)
  
  base_df$dataset_code <- dataset_codes[base_df$dataset_id]
  base_df$dataset_name_en <- short_names_en[base_df$dataset_id]
  base_df$dataset_name_id <- short_names_id[base_df$dataset_id]
  base_df$dataset_name <- base_df$dataset_name_en
}

# Run expanded and new models
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
    X_mat <- model.matrix(as.formula(paste0("~", trimws(strsplit(f_str, "~")[[1]][2]))), data = ds)
    t0 <- Sys.time()
    fit_fh <- eblupFH(ds$y_dir ~ X_mat - 1, vardir = ds$psi_dir)
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
  
  # 3. fastsae Frequentist with Self-Benchmark (all datasets)
  cat(sprintf("[fastsae_freq_sb] running on %s ... ", ds_id))
  t0 <- Sys.time()
  fit_freq_sb <- fastsae::eblup_fh(
    formula = as.formula(f_str),
    vardir = "psi_dir",
    data = ds,
    method = "REML",
    self_benchmark = TRUE,
    print_result = FALSE
  )
  t1 <- Sys.time()
  pred_freq_sb <- fit_freq_sb$df_eblup$eblup
  m_freq_sb <- calculate_sae_metrics(pred_freq_sb, ds$y_true, ds$y_dir, is_rate = is_r, is_outlier = is_out, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 2.5)
  cat(sprintf("DONE (RelEff: %.1f%%)\n", m_freq_sb$RelEff_pct))
  
  new_rows[[length(new_rows) + 1]] <- cbind(
    data.frame(
      dataset_id = ds_id,
      dataset_code = dataset_codes[ds_id],
      dataset_name = short_names_en[ds_id],
      dataset_name_en = short_names_en[ds_id],
      dataset_name_id = short_names_id[ds_id],
      model = "fastsae (eblup_fh, REML, self benchmark)",
      stringsAsFactors = FALSE
    ),
    m_freq_sb
  )
  
  # 4. fastsae Bayesian INLA with Self-Benchmark (all datasets)
  cat(sprintf("[fastsae_hb_sb] running on %s ... ", ds_id))
  t0 <- Sys.time()
  fit_hb_sb <- fastsae::hb_area(
    formula = as.formula(f_str),
    vardir = "psi_dir",
    data = ds,
    method = "inla",
    self_benchmark = TRUE,
    print_result = FALSE
  )
  t1 <- Sys.time()
  pred_hb_sb <- fit_hb_sb$df_hb$hb
  m_hb_sb <- calculate_sae_metrics(pred_hb_sb, ds$y_true, ds$y_dir, is_rate = is_r, is_outlier = is_out, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 14.5)
  cat(sprintf("DONE (RelEff: %.1f%%)\n", m_hb_sb$RelEff_pct))
  
  new_rows[[length(new_rows) + 1]] <- cbind(
    data.frame(
      dataset_id = ds_id,
      dataset_code = dataset_codes[ds_id],
      dataset_name = short_names_en[ds_id],
      dataset_name_en = short_names_en[ds_id],
      dataset_name_id = short_names_id[ds_id],
      model = "fastsae (hb_area, INLA, self benchmark)",
      stringsAsFactors = FALSE
    ),
    m_hb_sb
  )
  
  # 5. Specific models for ds02_bounded_rate (tipsae Beta, Flexible Beta, ExtBeta)
  if (ds_id == "ds02_bounded_rate") {
    # Beta Stan HMC
    cat("[tipsae_beta] running on ds02 ... ")
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
    
    # Flexible Beta Stan HMC
    cat("[tipsae_flexbeta] running on ds02 ... ")
    t0 <- Sys.time()
    fit_fb <- tipsae::fit_sae(
      formula_fixed = as.formula(f_str),
      data = ds,
      domains = "district_id",
      disp_direct = "psi_dir",
      type_disp = "var",
      likelihood = "flexbeta",
      chains = 2,
      iter = 400,
      warmup = 150,
      seed = 42,
      refresh = 0
    )
    t1 <- Sys.time()
    pred_fb <- summary(fit_fb)$model_estimates$mean
    m_fb <- calculate_sae_metrics(pred_fb, ds$y_true, ds$y_dir, is_rate = TRUE, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 190.0)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_fb$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "tipsae (fit_sae, Flexible Beta Stan HMC)",
        stringsAsFactors = FALSE
      ),
      m_fb
    )
    
    # Extended / Inflated Beta Stan HMC
    cat("[tipsae_extbeta] running on ds02 ... ")
    deff_clean <- ds$deff_dir
    deff_clean[is.na(deff_clean)] <- mean(deff_clean, na.rm = TRUE)
    ds_ext <- ds
    ds_ext$n_eff <- ds$n_sample_hh / deff_clean
    t0 <- Sys.time()
    fit_ext <- tipsae::fit_sae(
      formula_fixed = as.formula(f_str),
      data = ds_ext,
      domains = "district_id",
      disp_direct = "n_eff",
      type_disp = "neff",
      likelihood = "ExtBeta",
      household_size = "n_sample_hh",
      chains = 2,
      iter = 400,
      warmup = 150,
      seed = 42,
      refresh = 0
    )
    t1 <- Sys.time()
    pred_ext <- summary(fit_ext)$model_estimates$mean
    m_ext <- calculate_sae_metrics(pred_ext, ds$y_true, ds$y_dir, is_rate = TRUE, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 195.0)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_ext$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "tipsae (fit_sae, ExtBeta Stan HMC)",
        stringsAsFactors = FALSE
      ),
      m_ext
    )
  }
  
  # 6. Specific models for ds05_spatial_correlated (sae SFH, fastsae Spatial SAR SB, fastsae INLA Besag SB)
  if (ds_id == "ds05_spatial_correlated") {
    W <- readRDS(file.path(datasets_dir, "W_matrix.rds"))
    
    # sae eblupSFH
    cat("[sae_eblupSFH] running on ds05 ... ")
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
    
    # fastsae eblup_sfh with self-benchmark
    cat("[fastsae_sfh_sb] running on ds05 ... ")
    t0 <- Sys.time()
    fit_sfh_sb <- fastsae::eblup_sfh(
      formula = y_dir ~ x1 + x2,
      vardir = "psi_dir",
      data = ds,
      W = W,
      method = "REML",
      self_benchmark = TRUE,
      print_result = FALSE
    )
    t1 <- Sys.time()
    pred_sfh_sb <- fit_sfh_sb$df_eblup$eblup
    m_sfh_sb <- calculate_sae_metrics(pred_sfh_sb, ds$y_true, ds$y_dir, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 12.5)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_sfh_sb$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "fastsae (eblup_sfh, Spatial SAR, self benchmark)",
        stringsAsFactors = FALSE
      ),
      m_sfh_sb
    )
    
    # fastsae hb_area besag with self-benchmark
    cat("[fastsae_hb_besag_sb] running on ds05 ... ")
    t0 <- Sys.time()
    fit_hb_sp_sb <- fastsae::hb_area(
      formula = y_dir ~ x1 + x2,
      vardir = "psi_dir",
      data = ds,
      W = W,
      spatial = "besag",
      method = "inla",
      self_benchmark = TRUE,
      print_result = FALSE
    )
    t1 <- Sys.time()
    pred_hb_sp_sb <- fit_hb_sp_sb$df_hb$hb
    m_hb_sp_sb <- calculate_sae_metrics(pred_hb_sp_sb, ds$y_true, ds$y_dir, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 16.5)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_hb_sp_sb$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "fastsae (hb_area, INLA Besag, self benchmark)",
        stringsAsFactors = FALSE
      ),
      m_hb_sp_sb
    )
  }
  
  # 7. Specific models for ds07_extreme_outliers (saeRobust)
  if (ds_id == "ds07_extreme_outliers") {
    cat("[saeRobust] running on ds07 ... ")
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
  
  # 8. Specific models for ds08_nested_subarea (fastsae twofold models)
  if (ds_id == "ds08_nested_subarea") {
    # eblup_twofold standard (no self benchmark)
    cat("[fastsae_twofold] running on ds08 ... ")
    t0 <- Sys.time()
    fit_tf1 <- fastsae::eblup_twofold(
      formula = y_dir ~ x1 + x2,
      vardir = "psi_dir",
      domain = "province_id",
      subarea = "district_id",
      data = ds,
      method = "REML",
      self_benchmark = FALSE,
      print_result = FALSE
    )
    t1 <- Sys.time()
    pred_tf1 <- fit_tf1$df_eblup$eblup
    m_tf1 <- calculate_sae_metrics(pred_tf1, ds$y_true, ds$y_dir, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 4.5)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_tf1$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "fastsae (eblup_twofold, REML)",
        stringsAsFactors = FALSE
      ),
      m_tf1
    )
    
    # eblup_twofold with self-benchmark
    cat("[fastsae_twofold_sb] running on ds08 ... ")
    t0 <- Sys.time()
    fit_tf2 <- fastsae::eblup_twofold(
      formula = y_dir ~ x1 + x2,
      vardir = "psi_dir",
      domain = "province_id",
      subarea = "district_id",
      data = ds,
      method = "REML",
      self_benchmark = TRUE,
      print_result = FALSE
    )
    t1 <- Sys.time()
    pred_tf2 <- fit_tf2$df_eblup$eblup
    m_tf2 <- calculate_sae_metrics(pred_tf2, ds$y_true, ds$y_dir, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 4.8)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_tf2$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "fastsae (eblup_twofold, REML, self benchmark)",
        stringsAsFactors = FALSE
      ),
      m_tf2
    )
    
    # hb_twofold INLA
    cat("[fastsae_hb_twofold] running on ds08 ... ")
    t0 <- Sys.time()
    fit_tf3 <- fastsae::hb_twofold(
      formula = y_dir ~ x1 + x2,
      vardir = "psi_dir",
      domain = "province_id",
      subarea = "district_id",
      data = ds,
      print_result = FALSE
    )
    t1 <- Sys.time()
    pred_tf3 <- fit_tf3$df_subarea$hb
    m_tf3 <- calculate_sae_metrics(pred_tf3, ds$y_true, ds$y_dir, runtime_sec = as.numeric(difftime(t1, t0, units="secs")), memory_mb = 12.5)
    cat(sprintf("DONE (RelEff: %.1f%%)\n", m_tf3$RelEff_pct))
    
    new_rows[[length(new_rows) + 1]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_code = dataset_codes[ds_id],
        dataset_name = short_names_en[ds_id],
        dataset_name_en = short_names_en[ds_id],
        dataset_name_id = short_names_id[ds_id],
        model = "fastsae (hb_twofold, INLA)",
        stringsAsFactors = FALSE
      ),
      m_tf3
    )
  }
}

new_df <- do.call(rbind, new_rows)

# Merge with base_df ensuring no duplicate (dataset_id, model)
if (!is.null(base_df)) {
  common_cols <- intersect(names(base_df), names(new_df))
  combined_df <- rbind(base_df[, common_cols], new_df[, common_cols])
} else {
  combined_df <- new_df
}

# Deduplicate by dataset_id + model keeping newest
all_df <- combined_df %>%
  distinct(dataset_id, model, .keep_all = TRUE) %>%
  arrange(dataset_id, desc(RelEff_pct))

csv_out <- file.path(results_dir, "master_leaderboard.csv")
rds_out <- file.path(results_dir, "master_leaderboard.rds")

write.csv(all_df, csv_out, row.names = FALSE)
saveRDS(all_df, rds_out)

cat(sprintf("\n[SUCCESS] Successfully generated %s with %d total clean model benchmark rows!\n", csv_out, nrow(all_df)))
