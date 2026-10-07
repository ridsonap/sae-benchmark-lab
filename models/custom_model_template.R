#' Custom Model Adapter Template for SAE Benchmark Lab
#'
#' Cara baru (disarankan, 3 baris):
#'   source("R/metrics.R"); source("R/datasets.R")
#'   source("R/benchmark.R"); source("R/web.R")
#'   benchmark_sae(my_custom_sae_model, "MyModel (pkg, v1)", dataset_ids = NULL)
#'
#' Hasil otomatis masuk results/master_leaderboard.csv + docs/leaderboard.json
#' dan tampil di docs/index.html per dataset.
#'
#' Syarat fungsi: terima `(ds, formula_str)` dan kembalikan vektor numerik
#' prediksi area sepanjang `nrow(ds)`.
#'
#' @example
#' source("models/custom_model_template.R")
#' source("R/metrics.R"); source("R/datasets.R")
#' source("R/benchmark.R"); source("R/web.R")
#' benchmark_sae(my_custom_sae_model, "MyCustomModel (custom, v1)")

my_custom_sae_model <- function(ds, formula_str, ...) {
  # 1. ds contains:
  #    - district_id: integer / character ID of areas
  #    - y_dir: direct survey estimate (Taylor linearized)
  #    - psi_dir: sampling variance
  #    - deff_dir: design effect
  #    - x1, x2, ...: auxiliary covariates
  
  # 2. Extract formula
  f <- as.formula(formula_str)
  
  # 3. Fit your model here
  # For example:
  # fit <- my_fitting_function(formula = f, vardir = "psi_dir", data = ds)
  # estimates <- predict(fit)
  
  # Placeholder: Simple synthetic shrinkage estimator example
  # theta_d = gamma_d * y_dir + (1 - gamma_d) * x_beta
  fit_lm <- lm(f, data = ds)
  x_beta <- predict(fit_lm, newdata = ds)
  sigma2_u <- max(0.001, var(residuals(fit_lm)) - mean(ds$psi_dir))
  gamma <- sigma2_u / (sigma2_u + ds$psi_dir)
  estimates <- gamma * ds$y_dir + (1 - gamma) * x_beta
  
  return(as.numeric(estimates))
}
