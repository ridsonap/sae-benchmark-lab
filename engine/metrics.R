#' Comprehensive Small Area Estimation Benchmark Evaluation Metrics
#'
#' Standardized metrics to assess accuracy, bias, efficiency, and robustness against exact Ground Truth:
#' - ARB: Absolute Relative Bias (%)
#' - RRMSE: Relative Root Mean Squared Error (%)
#' - RMSE: Root Mean Squared Error
#' - MAE: Mean Absolute Error
#' - Corr: Pearson Correlation with Ground Truth
#' - RelEff: Relative Efficiency vs Direct Estimator (MSE_direct / MSE_model * 100)
#' - Outlier_RRMSE / Regular_RRMSE: For outlier shock benchmarks
#' - Boundary_Violations: Number of estimates outside valid range (e.g. for rates)
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

#' Calculate SAE Metrics
#'
#' @param pred Vector of model estimates.
#' @param truth Vector of exact Ground Truth population values.
#' @param direct Vector of direct survey estimates (optional, used for Relative Efficiency).
#' @param is_rate Logical, whether target is a bounded rate in (0, 1).
#' @param is_outlier Logical vector indicating outlier districts (optional).
#' @param runtime_sec Numeric runtime in seconds.
#' @param memory_mb Numeric peak RAM consumption in megabytes (MB).
#' @return A single-row data.frame of evaluation metrics.
calculate_sae_metrics <- function(pred,
                                  truth,
                                  direct = NULL,
                                  is_rate = FALSE,
                                  is_outlier = NULL,
                                  runtime_sec = NA_real_,
                                  memory_mb = NA_real_) {
  # Input sanitation
  pred <- as.numeric(pred)
  truth <- as.numeric(truth)
  
  valid_idx <- !is.na(pred) & !is.na(truth) & !is.infinite(pred) & !is.infinite(truth)
  n_valid <- sum(valid_idx)
  n_total <- length(truth)
  
  if (n_valid == 0) {
    return(data.frame(
      N = n_total,
      N_valid = 0,
      ARB_pct = NA_real_,
      RRMSE_pct = NA_real_,
      RMSE = NA_real_,
      MAE = NA_real_,
      Corr = NA_real_,
      RelEff_pct = NA_real_,
      Regular_RRMSE = NA_real_,
      Outlier_RRMSE = NA_real_,
      Boundary_Violations = NA_integer_,
      Peak_RAM_MB = memory_mb,
      Runtime_sec = runtime_sec,
      stringsAsFactors = FALSE
    ))
  }
  
  p <- pred[valid_idx]
  t <- truth[valid_idx]
  
  # Absolute Relative Bias (%)
  # For rates close to 0, avoid division by 0 by using max(abs(t), 1e-6)
  denom_t <- ifelse(abs(t) < 1e-6, 1e-6, abs(t))
  rel_errors <- (p - t) / denom_t
  arb_pct <- mean(abs(rel_errors)) * 100
  
  # Relative Root Mean Squared Error (%)
  rrmse_pct <- sqrt(mean(rel_errors^2)) * 100
  
  # Standard RMSE & MAE
  rmse <- sqrt(mean((p - t)^2))
  mae <- mean(abs(p - t))
  
  # Pearson Correlation
  corr_val <- if (sd(p) > 0 && sd(t) > 0) cor(p, t) else NA_real_
  
  # Relative Efficiency vs Direct
  rel_eff_pct <- NA_real_
  if (!is.null(direct) && length(direct) == n_total) {
    d <- as.numeric(direct)[valid_idx]
    mse_direct <- mean((d - t)^2, na.rm = TRUE)
    mse_model <- mean((p - t)^2, na.rm = TRUE)
    if (mse_model > 1e-12) {
      rel_eff_pct <- (mse_direct / mse_model) * 100
    }
  }
  
  # Outlier-specific breakdown
  regular_rrmse <- NA_real_
  outlier_rrmse <- NA_real_
  if (!is.null(is_outlier) && length(is_outlier) == n_total) {
    outlier_vec <- is_outlier[valid_idx]
    if (any(outlier_vec)) {
      outlier_rrmse <- sqrt(mean(rel_errors[outlier_vec]^2)) * 100
    }
    if (any(!outlier_vec)) {
      regular_rrmse <- sqrt(mean(rel_errors[!outlier_vec]^2)) * 100
    }
  }
  
  # Boundary violations (for rates in [0, 1])
  boundary_viol <- 0L
  if (is_rate) {
    boundary_viol <- sum(p < 0 | p > 1)
  }
  
  res <- data.frame(
    N = n_total,
    N_valid = n_valid,
    ARB_pct = round(arb_pct, 2),
    RRMSE_pct = round(rrmse_pct, 2),
    RMSE = round(rmse, 4),
    MAE = round(mae, 4),
    Corr = round(corr_val, 4),
    RelEff_pct = round(rel_eff_pct, 1),
    Regular_RRMSE = round(regular_rrmse, 2),
    Outlier_RRMSE = round(outlier_rrmse, 2),
    Boundary_Violations = boundary_viol,
    Peak_RAM_MB = round(memory_mb, 2),
    Runtime_sec = round(runtime_sec, 2),
    stringsAsFactors = FALSE
  )
  
  return(res)
}
