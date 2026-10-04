#' Custom Model Adapter Template for SAE Benchmark Lab
#'
#' Use this template to benchmark your own custom SAE model (e.g. your own Stan/INLA/EBLUP/ML script).
#' Simply implement the function below so that it takes `(ds, formula_str)` and returns a numeric vector
#' of area predictions matching `nrow(ds)`.
#'
#' @example
#' source("models/custom_model_template.R")
#' source("engine/benchmark_runner.R")
#' run_benchmark_suite(models = list("MyCustomModel" = my_custom_sae_model))

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
