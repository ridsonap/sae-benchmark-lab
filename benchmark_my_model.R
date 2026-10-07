#!/usr/bin/env Rscript
# Contoh: benchmark model SAE sendiri dalam 3 baris.
#
#   Rscript benchmark_my_model.R
#
# Ganti fungsi my_model di bawah dengan model Anda.
# Syarat: function(ds, formula_str) -> vektor numerik sepanjang nrow(ds).

source("R/metrics.R")
source("R/datasets.R")
source("R/benchmark.R")
source("R/web.R")

# --- 1. Definisikan model Anda di sini ---
my_model <- function(ds, formula_str) {
  fit <- lm(as.formula(formula_str), data = ds)
  as.numeric(predict(fit, newdata = ds))
}

# --- 2. Jalankan benchmark (ubah dataset_ids sesuai kebutuhan) ---
# dataset_ids = NULL  -> semua 8 dataset
# dataset_ids = "ds01" -> fuzzy match ke ds01_continuous_linear
res <- benchmark_sae(
  model_fn = my_model,
  model_name = "demo (lm, v1)",
  dataset_ids = c("ds01_continuous_linear", "ds02_bounded_rate")
)

print(res[, c("dataset_id", "model", "RRMSE_pct", "ARB_pct", "RelEff_pct", "Corr", "Runtime_sec")])
cat("\nBuka docs/index.html untuk melihat leaderboard.\n")
