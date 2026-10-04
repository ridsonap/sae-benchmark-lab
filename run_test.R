#!/usr/bin/env Rscript
#' SAE Benchmark Lab - Quick Test Runner
#'
#' Usage:
#'   Rscript run_test.R                     # Run standard model battery on all 8 datasets
#'   Rscript run_test.R --quick             # Run quick check on first 3 datasets
#'   Rscript run_test.R --dataset=ds01      # Run only specific dataset
#'
#' Or inside an R session:
#'   source("run_test.R")
#'   # Define your custom model:
#'   my_custom_model <- function(ds, formula_str) {
#'     # Return vector of estimates for each area
#'   }
#'   run_benchmark_suite(models = list("MyModel" = my_custom_model))
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

suppressPackageStartupMessages({
  library(dplyr)
})

# Source benchmark engine and pages generator
source("/Volumes/work/_MainR/sae-benchmark-lab/engine/benchmark_runner.R")
source("/Volumes/work/_MainR/sae-benchmark-lab/engine/generate_pages.R")

args <- commandArgs(trailingOnly = TRUE)

selected_datasets <- NULL
if ("--quick" %in% args) {
  selected_datasets <- c("ds01_continuous_linear", "ds02_bounded_rate", "ds03_highdim_sparse")
  cat("[Info] Running in --quick mode (evaluating 3 datasets).\n")
} else {
  ds_arg <- grep("^--dataset=", args, value = TRUE)
  if (length(ds_arg) > 0) {
    val <- sub("^--dataset=", "", ds_arg[1])
    selected_datasets <- val
    cat(sprintf("[Info] Running only specified dataset: %s\n", selected_datasets))
  }
}

# Run the benchmark suite
if (sys.nframe() == 0L) {
  res <- run_benchmark_suite(
    dataset_ids = selected_datasets,
    datasets_dir = "/Volumes/work/_MainR/sae-benchmark-lab/datasets",
    output_dir = "/Volumes/work/_MainR/sae-benchmark-lab/results"
  )
  
  cat("\n======================================================================\n")
  cat("                     BENCHMARK SUITE SUMMARY LEADERBOARD             \n")
  cat("======================================================================\n")
  
  display_cols <- c("dataset_id", "model", "RRMSE_pct", "ARB_pct", "RelEff_pct", "Corr", "Peak_RAM_MB", "Runtime_sec")
  print(res[, display_cols], row.names = FALSE)
  cat("======================================================================\n")
  
  cat("\n======================================================================\n")
  cat("   UPDATING GITHUB PAGES INTERACTIVE DASHBOARD                       \n")
  cat("======================================================================\n")
  tryCatch({
    generate_benchmark_dashboard()
  }, error = function(e) {
    cat(sprintf("[-] Warning: Failed to refresh dashboard: %s\n", e$message))
  })
}
