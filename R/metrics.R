#' SAE evaluation metrics against finite-population ground truth
#'
#' @param pred numeric vector of model estimates (length D)
#' @param truth numeric vector of exact ground truth
#' @param direct numeric vector of direct survey estimates (for RelEff), or NULL
#' @param is_rate logical, bounded rate in (0,1)?
#' @param is_outlier logical vector flagging outlier areas, or NULL
#' @param runtime_sec numeric runtime in seconds
#' @param memory_mb numeric peak RAM in MB
#' @return single-row data.frame
#' @export
calculate_sae_metrics <- function(pred,
                                  truth,
                                  direct = NULL,
                                  is_rate = FALSE,
                                  is_outlier = NULL,
                                  runtime_sec = NA_real_,
                                  memory_mb = NA_real_) {
  pred <- as.numeric(pred)
  truth <- as.numeric(truth)

  valid_idx <- !is.na(pred) & !is.na(truth) &
    !is.infinite(pred) & !is.infinite(truth)
  n_valid <- sum(valid_idx)
  n_total <- length(truth)

  na_row <- function() {
    data.frame(
      N = n_total, N_valid = 0,
      ARB_pct = NA_real_, RRMSE_pct = NA_real_,
      RMSE = NA_real_, MAE = NA_real_, Corr = NA_real_,
      RelEff_pct = NA_real_,
      Regular_RRMSE = NA_real_, Outlier_RRMSE = NA_real_,
      Boundary_Violations = NA_integer_,
      Peak_RAM_MB = round2(memory_mb),
      Runtime_sec = round2(runtime_sec),
      stringsAsFactors = FALSE
    )
  }

  if (n_valid == 0) return(na_row())

  p <- pred[valid_idx]
  t <- truth[valid_idx]

  denom_t <- ifelse(abs(t) < 1e-6, 1e-6, abs(t))
  rel_errors <- (p - t) / denom_t
  arb_pct <- mean(abs(rel_errors)) * 100
  rrmse_pct <- sqrt(mean(rel_errors^2)) * 100

  rmse <- sqrt(mean((p - t)^2))
  mae <- mean(abs(p - t))
  corr_val <- if (stats::sd(p) > 0 && stats::sd(t) > 0) {
    suppressWarnings(stats::cor(p, t))
  } else {
    NA_real_
  }

  rel_eff_pct <- NA_real_
  if (!is.null(direct) && length(direct) == n_total) {
    d <- as.numeric(direct)[valid_idx]
    mse_direct <- mean((d - t)^2, na.rm = TRUE)
    mse_model <- mean((p - t)^2, na.rm = TRUE)
    if (!is.na(mse_model) && mse_model > 1e-12) {
      rel_eff_pct <- (mse_direct / mse_model) * 100
    }
  }

  regular_rrmse <- NA_real_
  outlier_rrmse <- NA_real_
  if (!is.null(is_outlier) && length(is_outlier) == n_total) {
    ov <- is_outlier[valid_idx]
    ov[is.na(ov)] <- FALSE
    if (any(ov)) outlier_rrmse <- sqrt(mean(rel_errors[ov]^2)) * 100
    if (any(!ov)) regular_rrmse <- sqrt(mean(rel_errors[!ov]^2)) * 100
  }

  boundary_viol <- 0L
  if (isTRUE(is_rate)) boundary_viol <- sum(p < 0 | p > 1)

  data.frame(
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
}

round2 <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_real_)
  round(as.numeric(x), 2)
}
