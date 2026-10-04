#' Automated SAE Benchmark Runner Engine
#'
#' Evaluates multiple Small Area Estimation (SAE) models across the standardized
#' battery of 8 BPS-mimicking benchmark datasets.
#'
#' Built-in model wrappers:
#' - Direct (Survey weighted Taylor linearization)
#' - fastsaegpu::hb_area (Full Hierarchical Bayes with synergy hyperpriors)
#' - fastsaegpu::merf_area (Mixed Effects Random Forest)
#' - fastsae::sae_eblup (Fay-Herriot REML EBLUP)
#' - fastsae::sae_inla (Integrated Nested Laplace Approximations)
#' - tipsae::fit_sae (Stan HMC Bayesian SAE)
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Source metrics module
source("/Volumes/work/_MainR/sae-benchmark-lab/engine/metrics.R")

#' Model Wrappers
#'
#' Each wrapper accepts (dataset, formula_obj, options) and returns a numeric vector of area estimates.

wrapper_direct <- function(ds, ...) {
  return(ds$y_dir)
}

wrapper_fastsaegpu_hb <- function(ds, formula_str, ...) {
  if (!requireNamespace("fastsaegpu", quietly = TRUE)) stop("fastsaegpu not installed")
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
  if (!is.null(fit$df_hb$hb)) return(fit$df_hb$hb)
  if (!is.null(fit$estimates$mean)) return(fit$estimates$mean)
  return(fit$estimates$hb)
}

wrapper_fastsaegpu_merf <- function(ds, formula_str, ...) {
  if (!requireNamespace("fastsaegpu", quietly = TRUE)) stop("fastsaegpu not installed")
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
  if (!is.null(fit$estimates$merf)) return(fit$estimates$merf)
  if (!is.null(fit$estimates$hb)) return(fit$estimates$hb)
  return(fit$estimates$estimate)
}

wrapper_fastsae_eblup <- function(ds, formula_str, ...) {
  if (!requireNamespace("fastsae", quietly = TRUE)) stop("fastsae not installed")
  fit <- fastsae::eblup_fh(
    formula = as.formula(formula_str),
    vardir = "psi_dir",
    data = ds,
    method = "REML",
    print_result = FALSE
  )
  if (!is.null(fit$df_eblup$eblup)) return(fit$df_eblup$eblup)
  if (!is.null(fit$fit$eblup)) return(fit$fit$eblup)
  return(fit$eblup)
}

wrapper_fastsae_inla <- function(ds, formula_str, ...) {
  if (!requireNamespace("fastsae", quietly = TRUE)) stop("fastsae not installed")
  fit <- fastsae::hb_area(
    formula = as.formula(formula_str),
    vardir = "psi_dir",
    data = ds,
    method = "inla",
    family = "gaussian",
    print_result = FALSE
  )
  if (!is.null(fit$df_hb$hb)) return(fit$df_hb$hb)
  if (!is.null(fit$estimates$mean)) return(fit$estimates$mean)
  return(fit$hb)
}

wrapper_tipsae_stan <- function(ds, formula_str, ...) {
  if (!requireNamespace("tipsae", quietly = TRUE)) stop("tipsae not installed")
  fit <- tipsae::fit_sae(
    formula_mod = as.formula(formula_str),
    vardir = "psi_dir",
    srs_data = ds,
    chains = 2,
    iter_warmup = 500,
    iter_sampling = 500,
    adapt_delta = 0.95,
    seed = 42,
    show_progress = FALSE
  )
  sum_tab <- tipsae::summary(fit)$estimates
  return(sum_tab$mean)
}

#' Run Benchmark Suite
#'
#' @param models Named list of model functions. Default runs Direct, fastsaegpu HB, fastsae EBLUP, fastsae INLA, MERF.
#' @param dataset_ids Character vector of dataset IDs to run. Defaults to all available in datasets/ directory.
#' @param datasets_dir Directory path containing generated benchmark datasets.
#' @param output_dir Directory path to write leaderboard and plots.
#' @return A data.frame with complete benchmark evaluation metrics.
run_benchmark_suite <- function(models = NULL,
                                dataset_ids = NULL,
                                datasets_dir = "/Volumes/work/_MainR/sae-benchmark-lab/datasets",
                                output_dir = "/Volumes/work/_MainR/sae-benchmark-lab/results") {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  plots_dir <- file.path(output_dir, "plots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  
  manifest_path <- file.path(datasets_dir, "datasets_manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("Manifest not found. Please run generator/make_all_datasets.R first.")
  }
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  
  if (!is.null(dataset_ids)) {
    manifest <- manifest[manifest$id %in% dataset_ids, ]
  }
  
  # Default models to evaluate
  if (is.null(models)) {
    models <- list(
      "Direct" = wrapper_direct,
      "fastsaegpu_HB" = wrapper_fastsaegpu_hb,
      "Enhanced_MERF" = wrapper_fastsaegpu_merf,
      "fastsae_EBLUP" = wrapper_fastsae_eblup,
      "fastsae_INLA" = wrapper_fastsae_inla
    )
  }
  
  cat("======================================================================\n")
  cat("           STARTING AUTOMATED SAE BENCHMARK EVALUATION               \n")
  cat(sprintf(" Evaluating %d models across %d datasets\n", length(models), nrow(manifest)))
  cat("======================================================================\n\n")
  
  all_results <- list()
  all_predictions <- list()
  
  for (i in seq_len(nrow(manifest))) {
    ds_row <- manifest[i, ]
    ds_id <- ds_row$id
    ds_path <- file.path(datasets_dir, paste0(ds_id, ".rds"))
    
    if (!file.exists(ds_path)) {
      cat(sprintf("[-] Warning: Dataset file %s does not exist, skipping.\n", ds_path))
      next
    }
    
    ds <- readRDS(ds_path)
    is_rate <- identical(ds_row$response_type, "bounded_rate")
    is_outlier_vec <- if ("is_outlier" %in% names(ds)) ds$is_outlier else NULL
    
    cat(sprintf("[%d/%d] Testing Dataset: %s (%s)\n", i, nrow(manifest), ds_row$name, ds_id))
    
    # Determine base formula for models
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
    
    for (m_name in names(models)) {
      m_fn <- models[[m_name]]
      cat(sprintf("   -> Running %-18s ... ", m_name))
      
      gc(reset = TRUE, verbose = FALSE)
      base_mem_mb <- sum(gc(verbose = FALSE)[, 2])
      start_time <- Sys.time()
      pred <- tryCatch({
        m_fn(ds, formula_str)
      }, error = function(e) {
        cat(sprintf("FAILED: %s\n", e$message))
        return(rep(NA_real_, nrow(ds)))
      })
      end_time <- Sys.time()
      gc_after <- gc(verbose = FALSE)
      peak_mem_mb <- max(0, round(sum(gc_after[, 7]) - base_mem_mb, 2))
      runtime_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      if (all(is.na(pred))) {
        metrics <- calculate_sae_metrics(pred, ds$y_true, ds$y_dir, is_rate, is_outlier_vec, runtime_sec, peak_mem_mb)
      } else {
        metrics <- calculate_sae_metrics(pred, ds$y_true, ds$y_dir, is_rate, is_outlier_vec, runtime_sec, peak_mem_mb)
        cat(sprintf("DONE | RRMSE: %5.2f%% | ARB: %5.2f%% | Eff: %5.1f%% | RAM: %5.1fMB | Time: %4.2fs\n",
                    metrics$RRMSE_pct, metrics$ARB_pct, metrics$RelEff_pct, metrics$Peak_RAM_MB, metrics$Runtime_sec))
        
        # Save individual prediction vectors
        pred_record <- data.frame(
          dataset_id = ds_id,
          model = m_name,
          district_id = ds$district_id,
          y_true = ds$y_true,
          y_dir = ds$y_dir,
          y_pred = pred,
          stringsAsFactors = FALSE
        )
        all_predictions[[length(all_predictions) + 1]] <- pred_record
      }
      
      res_row <- cbind(
        data.frame(
          dataset_id = ds_id,
          dataset_name = ds_row$name,
          model = m_name,
          stringsAsFactors = FALSE
        ),
        metrics
      )
      all_results[[length(all_results) + 1]] <- res_row
    }
    cat("\n")
  }
  
  leaderboard <- do.call(rbind, all_results)
  
  # Write leaderboard CSV and RDS (merging if running subset)
  lb_csv <- file.path(output_dir, "master_leaderboard.csv")
  lb_rds <- file.path(output_dir, "master_leaderboard.rds")
  if (!is.null(dataset_ids) && file.exists(lb_csv)) {
    tryCatch({
      existing_lb <- read.csv(lb_csv, stringsAsFactors = FALSE)
      keep_lb <- existing_lb[!(existing_lb$dataset_id %in% unique(leaderboard$dataset_id)), ]
      leaderboard <- rbind(keep_lb, leaderboard)
      leaderboard <- leaderboard[order(leaderboard$dataset_id, leaderboard$model), ]
    }, error = function(e) NULL)
  }
  write.csv(leaderboard, lb_csv, row.names = FALSE)
  saveRDS(leaderboard, lb_rds)
  
  if (length(all_predictions) > 0) {
    preds_df <- do.call(rbind, all_predictions)
    saveRDS(preds_df, file.path(output_dir, "all_model_predictions.rds"))
  }
  
  cat("======================================================================\n")
  cat("   GENERATING COMPARATIVE VISUALIZATION PLOTS                        \n")
  cat("======================================================================\n")
  
  # Plot 1: RRMSE % across models by dataset
  tryCatch({
    p1 <- ggplot(leaderboard, aes(x = dataset_id, y = RRMSE_pct, fill = model)) +
      geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      ) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = "SAE Benchmark: Relative Root Mean Squared Error (RRMSE %)",
        subtitle = "Lower is better | Evaluated against Exact BPS Two-Stage Finite Population Ground Truth",
        x = "Benchmark Dataset Archetype",
        y = "RRMSE (%)",
        fill = "Model"
      )
    ggsave(file.path(plots_dir, "leaderboard_rrmse_comparison.png"), p1, width = 12, height = 6, dpi = 300)
    cat("Saved: results/plots/leaderboard_rrmse_comparison.png\n")
  }, error = function(e) cat(sprintf("Plot 1 error: %s\n", e$message)))
  
  # Plot 2: Relative Efficiency vs Direct
  tryCatch({
    valid_eff <- leaderboard %>% filter(!is.na(RelEff_pct) & model != "Direct")
    if (nrow(valid_eff) > 0) {
      p2 <- ggplot(valid_eff, aes(x = dataset_id, y = RelEff_pct, fill = model)) +
        geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
        geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        ) +
        scale_fill_brewer(palette = "Dark2") +
        labs(
          title = "Relative Efficiency Gain over Direct Estimator (%)",
          subtitle = "Higher is better | 100% = Baseline Direct Survey Estimate",
          x = "Benchmark Dataset Archetype",
          y = "Relative Efficiency (%)",
          fill = "Model"
        )
      ggsave(file.path(plots_dir, "leaderboard_relative_efficiency.png"), p2, width = 12, height = 6, dpi = 300)
      cat("Saved: results/plots/leaderboard_relative_efficiency.png\n")
    }
  }, error = function(e) cat(sprintf("Plot 2 error: %s\n", e$message)))
  
  cat("\nBenchmark evaluation complete! Summary saved to:\n")
  cat(sprintf(" -> Leaderboard: %s\n", lb_csv))
  cat(sprintf(" -> Plots:       %s\n\n", plots_dir))
  
  return(leaderboard)
}
