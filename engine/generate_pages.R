#!/usr/bin/env Rscript
#' SAE Benchmark Lab - GitHub Pages HTML Generator
#'
#' Generates an interactive, responsive leaderboard dashboard at:
#' - docs/index.html (for GitHub Pages hosting)
#' - index.html (at repository root)
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

suppressPackageStartupMessages({
  library(dplyr)
})

generate_benchmark_dashboard <- function(
  leaderboard_path = "/Volumes/work/_MainR/sae-benchmark-lab/results/master_leaderboard.csv",
  docs_dir = "/Volumes/work/_MainR/sae-benchmark-lab/docs",
  root_dir = "/Volumes/work/_MainR/sae-benchmark-lab"
) {
  if (!file.exists(leaderboard_path)) {
    stop(sprintf("Leaderboard CSV not found at: %s", leaderboard_path))
  }
  
  df <- read.csv(leaderboard_path, stringsAsFactors = FALSE)
  
  # Ensure docs and docs/plots directories exist
  docs_plots_dir <- file.path(docs_dir, "plots")
  if (!dir.exists(docs_plots_dir)) dir.create(docs_plots_dir, recursive = TRUE)
  
  # Copy visualization plots to docs/plots
  res_plots_dir <- file.path(root_dir, "results", "plots")
  rrmse_src <- file.path(res_plots_dir, "leaderboard_rrmse_comparison.png")
  eff_src <- file.path(res_plots_dir, "leaderboard_relative_efficiency.png")
  
  if (file.exists(rrmse_src)) {
    file.copy(rrmse_src, file.path(docs_plots_dir, "leaderboard_rrmse_comparison.png"), overwrite = TRUE)
  }
  if (file.exists(eff_src)) {
    file.copy(eff_src, file.path(docs_plots_dir, "leaderboard_relative_efficiency.png"), overwrite = TRUE)
  }
  
  # Compute Summary KPIs
  total_models <- length(unique(df$model))
  total_datasets <- length(unique(df$dataset_id))
  
  # Non-direct models for comparative KPIs
  non_direct <- df[df$model != "Direct" & !is.na(df$RelEff_pct), ]
  
  # Top Relative Efficiency
  if (nrow(non_direct) > 0) {
    top_eff_idx <- which.max(non_direct$RelEff_pct)
    top_eff_val <- sprintf("%.1f%%", non_direct$RelEff_pct[top_eff_idx])
    top_eff_detail <- sprintf("%s on %s", non_direct$model[top_eff_idx], non_direct$dataset_id[top_eff_idx])
  } else {
    top_eff_val <- "100.0%"
    top_eff_detail <- "N/A"
  }
  
  # Fastest Model (excluding Direct)
  model_runtimes <- non_direct %>%
    group_by(model) %>%
    summarise(mean_runtime = mean(Runtime_sec, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_runtime)
  
  if (nrow(model_runtimes) > 0) {
    fastest_model_name <- model_runtimes$model[1]
    fastest_model_time <- sprintf("%.2fs avg", model_runtimes$mean_runtime[1])
  } else {
    fastest_model_name <- "N/A"
    fastest_model_time <- "N/A"
  }
  
  # Lowest Peak RAM (excluding Direct)
  if ("Peak_RAM_MB" %in% names(df) && any(!is.na(non_direct$Peak_RAM_MB))) {
    model_ram <- non_direct %>%
      filter(!is.na(Peak_RAM_MB)) %>%
      group_by(model) %>%
      summarise(mean_ram = mean(Peak_RAM_MB, na.rm = TRUE), .groups = "drop") %>%
      arrange(mean_ram)
    
    if (nrow(model_ram) > 0) {
      lowest_ram_model <- model_ram$model[1]
      lowest_ram_val <- sprintf("%.1f MB avg", model_ram$mean_ram[1])
    } else {
      lowest_ram_model <- "N/A"
      lowest_ram_val <- "N/A"
    }
  } else {
    lowest_ram_model <- "N/A"
    lowest_ram_val <- "N/A"
  }
  
  # Dataset archetype dictionary
  dataset_meta <- list(
    "ds01_continuous_linear" = list(
      name = "Continuous Linear (Baseline Fay-Herriot)",
      badge = "Baseline",
      desc = "Synthetic log per-capita household expenditure with 3 continuous auxiliary covariates and homoskedastic normal district shocks.",
      challenge = "Evaluates standard EBLUP / Hierarchical Bayes shrinkage efficiency gain over baseline direct estimates."
    ),
    "ds02_bounded_rate" = list(
      name = "Bounded Rate (Poverty / Prevalence)",
      badge = "Bounded (0, 1)",
      desc = "Binary household poverty indicators aggregated to district prevalence rates strictly within the unit interval (0, 1).",
      challenge = "Tests logit/arcsin/Beta links vs unconstrained linear models that risk producing negative rates or probabilities > 100%."
    ),
    "ds03_highdim_sparse" = list(
      name = "High-Dimensional Sparse (Podes / Satellite Features)",
      badge = "Regularization",
      desc = "25 area-level administrative and remote-sensing predictors, containing only 3 true generative signals and 22 collinear noise variables.",
      challenge = "Evaluates sparse Horseshoe priors, LASSO, and shrinkage regularizers against severe variance inflation and overfitting."
    ),
    "ds04_nonlinear_interaction" = list(
      name = "Nonlinear & Interactions (MERF / Tree Territory)",
      badge = "Nonlinear",
      desc = "Complex synthetic DGP featuring sine waves, quadratic polynomials, square root transformations, and multiplicative interaction terms.",
      challenge = "Tests Mixed Effects Random Forests (MERF) and non-parametric machine learning SAE against linear model misspecification."
    ),
    "ds05_spatial_correlated" = list(
      name = "Spatial Correlated (SAR / CAR Topology)",
      badge = "Spatial SAR",
      desc = "Area random effects generated via simultaneous autoregressive SAR process (rho = 0.65) over an authentic Queen adjacency spatial matrix.",
      challenge = "Assesses spatial borrowing of strength through Spatial Fay-Herriot (SEBLUP) and INLA Besag/BYM2 models."
    ),
    "ds06_spatiotemporal_panel" = list(
      name = "Spatio-Temporal Panel (D=50 x T=5)",
      badge = "Panel AR(1)",
      desc = "Multi-year repeated survey panel featuring AR(1) autocorrelation (phi = 0.70) across 5 discrete survey rounds.",
      challenge = "Benchmarks dynamic spatio-temporal filters, Rao-Yu models, and tipsae panel estimators across space and time."
    ),
    "ds07_extreme_outliers" = list(
      name = "Extreme Outliers & Shocks (Contaminated)",
      badge = "Robust SAE",
      desc = "Contaminated response generating 4 disaster shock districts exhibiting +/- 8 sigma outlier deviations from regression trend.",
      challenge = "Verifies robust M-estimation, Huber EBLUP, and heavy-tailed Student-t Bayesian shrinkage against leverage breakdowns."
    ),
    "ds08_nested_subarea" = list(
      name = "Nested Subarea (Two-Fold Hierarchy)",
      badge = "Nested Two-Fold",
      desc = "Hierarchical structure with districts (kabupaten) nested inside administrative provinces with dual random effects (v_p + u_pd).",
      challenge = "Tests multi-level Hierarchical Bayes and two-fold subarea borrowing across multiple administrative tiers."
    )
  )
  
  # Function to format HTML rows
  rows <- character()
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    # Model Badge styling
    model_badge <- switch(
      row$model,
      "fastsaegpu_HB" = '<span class="badge bg-purple-subtle text-purple fw-semibold"><i class="fa-solid fa-bolt me-1"></i>fastsaegpu_HB</span>',
      "Enhanced_MERF" = '<span class="badge bg-emerald-subtle text-emerald fw-semibold"><i class="fa-solid fa-tree me-1"></i>Enhanced_MERF</span>',
      "fastsae_EBLUP" = '<span class="badge bg-primary-subtle text-primary fw-semibold"><i class="fa-solid fa-chart-line me-1"></i>fastsae_EBLUP</span>',
      "fastsae_INLA" = '<span class="badge bg-warning-subtle text-warning-emphasis fw-semibold"><i class="fa-solid fa-fire me-1"></i>fastsae_INLA</span>',
      "Direct" = '<span class="badge bg-secondary-subtle text-secondary fw-semibold"><i class="fa-solid fa-scale-balanced me-1"></i>Direct</span>',
      sprintf('<span class="badge bg-light text-dark fw-semibold">%s</span>', row$model)
    )
    
    # Relative efficiency formatting
    eff_val <- row$RelEff_pct
    eff_display <- if (is.na(eff_val)) {
      '<span class="text-muted">-</span>'
    } else if (eff_val > 105) {
      sprintf('<span class="badge bg-success text-white fw-bold"><i class="fa-solid fa-arrow-trend-up me-1"></i>%.1f%%</span>', eff_val)
    } else if (eff_val >= 98) {
      sprintf('<span class="badge bg-secondary-subtle text-secondary fw-semibold">%.1f%%</span>', eff_val)
    } else {
      sprintf('<span class="badge bg-danger-subtle text-danger fw-semibold">%.1f%%</span>', eff_val)
    }
    
    ram_val <- if ("Peak_RAM_MB" %in% names(row) && !is.na(row$Peak_RAM_MB)) {
      sprintf("%.1f MB", row$Peak_RAM_MB)
    } else {
      '<span class="text-muted">-</span>'
    }
    
    runtime_val <- if (!is.na(row$Runtime_sec)) {
      sprintf("%.2fs", row$Runtime_sec)
    } else {
      '<span class="text-muted">-</span>'
    }
    
    ds_label <- sprintf('<strong>%s</strong><br><small class="text-muted font-monospace">%s</small>', 
                        row$dataset_name, row$dataset_id)
    
    order_ds <- row$dataset_id
    order_model <- row$model
    order_rrmse <- if (is.na(row$RRMSE_pct)) 999999 else row$RRMSE_pct
    order_arb <- if (is.na(row$ARB_pct)) 999999 else row$ARB_pct
    order_eff <- if (is.na(row$RelEff_pct)) -1 else row$RelEff_pct
    order_corr <- if (is.na(row$Corr)) -1 else row$Corr
    order_ram <- if ("Peak_RAM_MB" %in% names(row) && !is.na(row$Peak_RAM_MB)) row$Peak_RAM_MB else -1
    order_time <- if (!is.na(row$Runtime_sec)) row$Runtime_sec else -1
    
    r <- paste0(
      '<tr>',
      '<td data-order="', order_ds, '" data-search="', row$dataset_id, ' ', row$dataset_name, '">', ds_label, '</td>',
      '<td data-order="', order_model, '" data-search="', row$model, '">', model_badge, '</td>',
      '<td class="text-end fw-semibold" data-order="', order_rrmse, '">', sprintf("%.2f%%", row$RRMSE_pct), '</td>',
      '<td class="text-end" data-order="', order_arb, '">', sprintf("%.2f%%", row$ARB_pct), '</td>',
      '<td class="text-center" data-order="', order_eff, '">', eff_display, '</td>',
      '<td class="text-end" data-order="', order_corr, '">', sprintf("%.4f", row$Corr), '</td>',
      '<td class="text-end font-monospace" data-order="', order_ram, '">', ram_val, '</td>',
      '<td class="text-end font-monospace" data-order="', order_time, '">', runtime_val, '</td>',
      '</tr>'
    )
    rows <- c(rows, r)
  }
  table_rows_html <- paste(rows, collapse = "\n")
  
  # Build Catalogue Cards
  cards <- character()
  for (ds_id in names(dataset_meta)) {
    meta <- dataset_meta[[ds_id]]
    card <- paste0(
      '<div class="col-md-6 col-lg-3">',
      '<div class="card h-100 archetype-card shadow-sm">',
      '<div class="card-body">',
      '<div class="d-flex justify-content-between align-items-start mb-2">',
      '<span class="badge bg-primary-subtle text-primary border border-primary-subtle rounded-pill font-monospace">', ds_id, '</span>',
      '<span class="badge bg-secondary-subtle text-secondary rounded-pill">', meta$badge, '</span>',
      '</div>',
      '<h5 class="card-title text-light fw-bold fs-6 mb-2">', meta$name, '</h5>',
      '<p class="card-text text-secondary small mb-3">', meta$desc, '</p>',
      '<div class="archetype-challenge p-2 rounded bg-dark border border-secondary border-opacity-25">',
      '<small class="text-warning-emphasis d-block fw-semibold mb-1"><i class="fa-solid fa-bullseye me-1"></i>Benchmark Target:</small>',
      '<small class="text-light-emphasis">', meta$challenge, '</small>',
      '</div>',
      '</div>',
      '</div>',
      '</div>'
    )
    cards <- c(cards, card)
  }
  catalogue_cards_html <- paste(cards, collapse = "\n")
  timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  
  # Read or define HTML template
  base_template <- '<!DOCTYPE html>
<html lang="en" data-bs-theme="dark">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>SAE Benchmark Lab | Official BPS Two-Stage Sampling Testbed</title>
  
  <!-- CSS Frameworks -->
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://cdn.datatables.net/2.0.2/css/dataTables.bootstrap5.min.css">
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css">
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600&display=swap" rel="stylesheet">

  <style>
    :root {
      --font-sans: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      --font-mono: "JetBrains Mono", monospace;
      --card-bg: #121826;
      --border-color: #242f49;
    }
    body {
      font-family: var(--font-sans);
      background-color: #0b0f19;
      color: #e2e8f0;
      min-height: 100vh;
    }
    .font-monospace {
      font-family: var(--font-mono) !important;
    }
    .navbar {
      background-color: rgba(11, 15, 25, 0.85);
      backdrop-filter: blur(12px);
      border-bottom: 1px solid var(--border-color);
    }
    .hero-section {
      background: radial-gradient(circle at 50% 0%, rgba(59, 130, 246, 0.15) 0%, transparent 70%);
      padding: 3.5rem 0 2rem;
      border-bottom: 1px solid rgba(36, 47, 73, 0.4);
    }
    .kpi-card {
      background-color: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 12px;
      transition: transform 0.2s ease, border-color 0.2s ease;
      position: relative;
      overflow: hidden;
    }
    .kpi-card:hover {
      transform: translateY(-2px);
      border-color: #3b82f6;
    }
    .kpi-card::before {
      content: "";
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      height: 3px;
      background: linear-gradient(90deg, #3b82f6, #8b5cf6);
    }
    .archetype-card {
      background-color: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 12px;
      transition: transform 0.2s ease, border-color 0.2s ease;
    }
    .archetype-card:hover {
      transform: translateY(-3px);
      border-color: #6366f1;
    }
    .table-container {
      background-color: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 14px;
      padding: 1.5rem;
    }
    table.dataTable {
      border-collapse: separate !important;
      border-spacing: 0;
    }
    .text-purple { color: #a855f7 !important; }
    .bg-purple-subtle { background-color: rgba(168, 85, 247, 0.15) !important; }
    .text-emerald { color: #10b981 !important; }
    .bg-emerald-subtle { background-color: rgba(16, 185, 129, 0.15) !important; }
    .code-box {
      background-color: #050811;
      border: 1px solid var(--border-color);
      border-radius: 8px;
      padding: 1rem 1.25rem;
      position: relative;
    }
    .copy-btn {
      position: absolute;
      top: 0.75rem;
      right: 0.75rem;
      font-size: 0.75rem;
    }
    .nav-pills .nav-link {
      color: #94a3b8;
      border-radius: 8px;
      padding: 0.5rem 1rem;
      font-weight: 500;
    }
    .nav-pills .nav-link.active {
      background-color: #3b82f6;
      color: #ffffff;
    }
    .scheme-step {
      background-color: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 12px;
      padding: 1.5rem;
      height: 100%;
    }
  </style>
</head>
<body>

  <!-- Navigation -->
  <nav class="navbar navbar-expand-lg sticky-top">
    <div class="container-xl">
      <a class="navbar-brand d-flex align-items-center gap-2 fw-bold text-white" href="#">
        <i class="fa-solid fa-chart-pie text-primary fs-4"></i>
        <span>SAE Benchmark Lab</span>
      </a>
      <div class="d-flex align-items-center gap-3">
        <a href="https://github.com/ridsonap/sae-benchmark-lab" target="_blank" class="btn btn-outline-secondary btn-sm rounded-pill px-3">
          <i class="fa-brands fa-github me-1"></i> GitHub Repo
        </a>
      </div>
    </div>
  </nav>

  <!-- Hero Header -->
  <section class="hero-section text-center">
    <div class="container-xl">
      <div class="d-flex justify-content-center gap-2 mb-3 flex-wrap">
        <span class="badge bg-primary-subtle text-primary border border-primary-subtle rounded-pill px-3 py-1">
          <i class="fa-solid fa-check-double me-1"></i> Official BPS Susenas Design
        </span>
        <span class="badge bg-success-subtle text-success border border-success-subtle rounded-pill px-3 py-1">
          <i class="fa-solid fa-database me-1"></i> Exact Finite Population Ground Truth
        </span>
        <span class="badge bg-info-subtle text-info border border-info-subtle rounded-pill px-3 py-1">
          <i class="fa-solid fa-microchip me-1"></i> RAM & Latency Profiling
        </span>
      </div>
      <h1 class="display-5 fw-extrabold text-white mb-2">SAE Benchmark Lab</h1>
      <p class="h5 fw-medium text-info mb-3">Official BPS Two-Stage Stratified Cluster Sampling Testbed</p>
      <p class="lead text-secondary mx-auto mb-4" style="max-width: 820px;">
        Standardized, reproducible testbed for evaluating <strong>Small Area Estimation (SAE)</strong> models under 
        two-stage stratified cluster sampling against exact finite-population Ground Truth.
      </p>
      
      <!-- KPI Cards -->
      <div class="row g-3 justify-content-center text-start mt-2">
        <div class="col-6 col-md-4 col-lg-2">
          <div class="card kpi-card p-3">
            <span class="text-secondary small fw-medium text-uppercase">Total Models</span>
            <div class="fs-3 fw-bold text-white mt-1">{{TOTAL_MODELS}}</div>
            <span class="text-muted small">Standard Battery</span>
          </div>
        </div>
        <div class="col-6 col-md-4 col-lg-2">
          <div class="card kpi-card p-3">
            <span class="text-secondary small fw-medium text-uppercase">8 Dataset Archetypes</span>
            <div class="fs-3 fw-bold text-white mt-1">{{TOTAL_DATASETS}}</div>
            <span class="text-muted small">Structural Challenges</span>
          </div>
        </div>
        <div class="col-6 col-md-4 col-lg-3">
          <div class="card kpi-card p-3">
            <span class="text-secondary small fw-medium text-uppercase">Top RelEff Gain</span>
            <div class="fs-3 fw-bold text-success mt-1">{{TOP_EFF_VAL}}</div>
            <span class="text-muted small text-truncate d-block">{{TOP_EFF_DETAIL}}</span>
          </div>
        </div>
        <div class="col-6 col-md-4 col-lg-2">
          <div class="card kpi-card p-3">
            <span class="text-secondary small fw-medium text-uppercase">Fastest Model</span>
            <div class="fs-3 fw-bold text-info mt-1">{{FASTEST_MODEL}}</div>
            <span class="text-muted small">{{FASTEST_TIME}}</span>
          </div>
        </div>
        <div class="col-6 col-md-4 col-lg-3">
          <div class="card kpi-card p-3">
            <span class="text-secondary small fw-medium text-uppercase">Lowest Memory</span>
            <div class="fs-3 fw-bold text-purple mt-1">{{LOWEST_RAM_MODEL}}</div>
            <span class="text-muted small">{{LOWEST_RAM_VAL}}</span>
          </div>
        </div>
      </div>
    </div>
  </section>

  <!-- Main Content -->
  <main class="container-xl py-5">

    <!-- Interactive Leaderboard Table Section -->
    <div class="mb-5">
      <div class="d-flex justify-content-between align-items-end mb-3 flex-wrap gap-3">
        <div>
          <h2 class="h3 fw-bold text-white mb-1"><i class="fa-solid fa-trophy text-warning me-2"></i>Master Leaderboard</h2>
          <p class="text-secondary small mb-0">Empirical metrics evaluated across finite-population ground truth</p>
        </div>
        <div class="d-flex align-items-center gap-2 flex-wrap">
          <div class="d-flex align-items-center gap-1">
            <label for="datasetFilter" class="text-secondary small text-nowrap"><i class="fa-solid fa-filter me-1"></i>Dataset:</label>
            <select id="datasetFilter" class="form-select form-select-sm bg-dark text-light border-secondary">
              <option value="">All Datasets</option>
              <option value="ds01_continuous_linear">ds01: Continuous Linear</option>
              <option value="ds02_bounded_rate">ds02: Bounded Rate</option>
              <option value="ds03_highdim_sparse">ds03: High-Dim Sparse</option>
              <option value="ds04_nonlinear_interaction">ds04: Nonlinear & Inter.</option>
              <option value="ds05_spatial_correlated">ds05: Spatial Correlated</option>
              <option value="ds06_spatiotemporal_panel">ds06: Spatio-Temporal Panel</option>
              <option value="ds07_extreme_outliers">ds07: Extreme Outliers</option>
              <option value="ds08_nested_subarea">ds08: Nested Subarea</option>
            </select>
          </div>
          <div class="d-flex align-items-center gap-1">
            <label for="modelFilter" class="text-secondary small text-nowrap"><i class="fa-solid fa-cube me-1"></i>Model:</label>
            <select id="modelFilter" class="form-select form-select-sm bg-dark text-light border-secondary">
              <option value="">All Models</option>
              <option value="Direct">Direct</option>
              <option value="fastsaegpu_HB">fastsaegpu_HB</option>
              <option value="Enhanced_MERF">Enhanced_MERF</option>
              <option value="fastsae_EBLUP">fastsae_EBLUP</option>
              <option value="fastsae_INLA">fastsae_INLA</option>
            </select>
          </div>
          <button id="resetFiltersBtn" class="btn btn-outline-secondary btn-sm">
            <i class="fa-solid fa-filter-circle-xmark me-1"></i> Reset
          </button>
        </div>
      </div>

      <div class="table-container shadow-sm">
        <div class="table-responsive">
          <table id="leaderboardTable" class="table table-hover align-middle mb-0" style="width: 100%;">
            <thead>
              <tr class="text-secondary small text-uppercase">
                <th>Dataset</th>
                <th>Model</th>
                <th class="text-end">RRMSE (%)</th>
                <th class="text-end">ARB (%)</th>
                <th class="text-center">Rel. Efficiency (%)</th>
                <th class="text-end">Corr</th>
                <th class="text-end">Peak RAM (MB)</th>
                <th class="text-end">Runtime (s)</th>
              </tr>
            </thead>
            <tbody>
{{TABLE_ROWS}}
            </tbody>
          </table>
        </div>
      </div>
    </div>

    <!-- Comparative Visualizations Section -->
    <div class="mb-5">
      <div class="d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2">
        <div>
          <h2 class="h3 fw-bold text-white mb-1"><i class="fa-solid fa-chart-column text-primary me-2"></i>Comparative Visualizations</h2>
          <p class="text-secondary small mb-0">Direct graphical comparisons across models and dataset archetypes</p>
        </div>
        <ul class="nav nav-pills" id="plotTabs" role="tablist">
          <li class="nav-item" role="presentation">
            <button class="nav-link active" id="tab-releff" data-bs-toggle="pill" data-bs-target="#plot-releff" type="button" role="tab">
              <i class="fa-solid fa-arrow-trend-up me-1"></i> Relative Efficiency
            </button>
          </li>
          <li class="nav-item" role="presentation">
            <button class="nav-link" id="tab-rrmse" data-bs-toggle="pill" data-bs-target="#plot-rrmse" type="button" role="tab">
              <i class="fa-solid fa-chart-simple me-1"></i> RRMSE Comparison
            </button>
          </li>
        </ul>
      </div>

      <div class="tab-content table-container p-3">
        <div class="tab-pane fade show active text-center" id="plot-releff" role="tabpanel">
          <img src="{{PLOTS_PATH}}/leaderboard_relative_efficiency.png" alt="Relative Efficiency Comparison" class="img-fluid rounded shadow-sm border border-secondary border-opacity-25" style="max-height: 580px; width: 100%; object-fit: contain;">
        </div>
        <div class="tab-pane fade text-center" id="plot-rrmse" role="tabpanel">
          <img src="{{PLOTS_PATH}}/leaderboard_rrmse_comparison.png" alt="RRMSE Comparison" class="img-fluid rounded shadow-sm border border-secondary border-opacity-25" style="max-height: 580px; width: 100%; object-fit: contain;">
        </div>
      </div>
    </div>

    <!-- BPS Two-Stage Sampling Scheme -->
    <div class="mb-5">
      <div class="mb-3">
        <h2 class="h3 fw-bold text-white mb-1"><i class="fa-solid fa-sitemap text-info me-2"></i>BPS Two-Stage Stratified Cluster Sampling Scheme</h2>
        <p class="text-secondary small mb-0">Faithful implementation of official Badan Pusat Statistik Susenas design</p>
      </div>

      <div class="row g-3">
        <div class="col-md-4">
          <div class="scheme-step">
            <div class="d-flex align-items-center gap-2 mb-2">
              <span class="badge bg-primary rounded-circle p-2" style="width: 28px; height: 28px;">1</span>
              <h5 class="fs-6 text-white fw-bold mb-0">Stage 1: Primary Sampling Unit (PSU)</h5>
            </div>
            <p class="text-secondary small mb-2">
              Stratified by Urban/Rural within each Kabupaten. Selection of <strong>Blok Sensus (BS)</strong> via <strong>Probability Proportional to Size (PPS)</strong> without replacement based on household counts:
            </p>
            <div class="code-box font-monospace text-info small py-1 px-2 mb-0">
              &pi;<sub>1,dhi</sub> = a<sub>dh</sub> &times; (M<sub>dhi</sub> / &sum; M<sub>dhk</sub>)
            </div>
          </div>
        </div>

        <div class="col-md-4">
          <div class="scheme-step">
            <div class="d-flex align-items-center gap-2 mb-2">
              <span class="badge bg-info rounded-circle p-2" style="width: 28px; height: 28px;">2</span>
              <h5 class="fs-6 text-white fw-bold mb-0">Stage 2: Secondary Sampling Unit (SSU)</h5>
            </div>
            <p class="text-secondary small mb-2">
              Systematic sampling of exactly <strong>m = 10 households</strong> per sampled Blok Sensus with a random start. Conditional inclusion probability:
            </p>
            <div class="code-box font-monospace text-info small py-1 px-2 mb-0">
              &pi;<sub>2|1,dhij</sub> = 10 / M<sub>dhi</sub>
            </div>
          </div>
        </div>

        <div class="col-md-4">
          <div class="scheme-step">
            <div class="d-flex align-items-center gap-2 mb-2">
              <span class="badge bg-success rounded-circle p-2" style="width: 28px; height: 28px;">3</span>
              <h5 class="fs-6 text-white fw-bold mb-0">Design Weights & Variance</h5>
            </div>
            <p class="text-secondary small mb-2">
              Calibrated weights <code>w<sub>dhij</sub> = M<sub>dh</sub> / (10 a<sub>dh</sub>)</code>. Sampling variance &psi;<sub>d</sub> estimated using <strong>Taylor Series Linearization</strong> via <code>survey::svydesign</code>, capturing intra-cluster clustering effects (Deff &gt; 1).
            </p>
          </div>
        </div>
      </div>
    </div>

    <!-- Dataset Archetype Catalogue -->
    <div class="mb-5">
      <div class="mb-3">
        <h2 class="h3 fw-bold text-white mb-1"><i class="fa-solid fa-boxes-stacked text-warning me-2"></i>Dataset Archetype Catalogue</h2>
        <p class="text-secondary small mb-0">8 standardized data generating processes reflecting core Small Area Estimation challenges</p>
      </div>
      <div class="row g-3">
{{CATALOGUE_CARDS}}
      </div>
    </div>

    <!-- Quick Start Plug-and-Play -->
    <div class="mb-5">
      <div class="mb-3">
        <h2 class="h3 fw-bold text-white mb-1"><i class="fa-solid fa-terminal text-success me-2"></i>Plug-and-Play Benchmark</h2>
        <p class="text-secondary small mb-0">Benchmark your custom SAE model in 1 line of R</p>
      </div>

      <div class="row g-3">
        <div class="col-lg-6">
          <div class="card bg-dark border border-secondary border-opacity-25 h-100 p-3">
            <h5 class="fs-6 text-white fw-bold mb-2"><i class="fa-solid fa-code me-2 text-primary"></i>R API (Custom Estimator)</h5>
            <div class="code-box">
              <button class="btn btn-outline-secondary btn-sm copy-btn" onclick="copyCode(\'r-snippet\', this)">
                <i class="fa-regular fa-copy me-1"></i> Copy
              </button>
              <pre id="r-snippet" class="mb-0 font-monospace text-light small"><code># 1. Source the benchmark engine
source("engine/benchmark_runner.R")

# 2. Define your model wrapper (accepts ds and formula_str)
my_custom_sae <- function(ds, formula_str) {
  fit <- my_pkg::model(formula = as.formula(formula_str), vardir = "psi_dir", data = ds)
  return(fit$estimates)
}

# 3. Benchmark against the full battery
results <- run_benchmark_suite(models = list("MyModel" = my_custom_sae))</code></pre>
            </div>
          </div>
        </div>

        <div class="col-lg-6">
          <div class="card bg-dark border border-secondary border-opacity-25 h-100 p-3">
            <h5 class="fs-6 text-white fw-bold mb-2"><i class="fa-solid fa-terminal me-2 text-info"></i>Terminal CLI</h5>
            <div class="code-box">
              <button class="btn btn-outline-secondary btn-sm copy-btn" onclick="copyCode(\'cli-snippet\', this)">
                <i class="fa-regular fa-copy me-1"></i> Copy
              </button>
              <pre id="cli-snippet" class="mb-0 font-monospace text-light small"><code># Run full battery across all 8 datasets
Rscript run_test.R

# Quick check on first 3 datasets
Rscript run_test.R --quick

# Run on a single specific archetype
Rscript run_test.R --dataset=ds02_bounded_rate</code></pre>
            </div>
          </div>
        </div>
      </div>
    </div>

  </main>

  <!-- Footer -->
  <footer class="py-4 border-top border-secondary border-opacity-25 text-center text-secondary small">
    <div class="container-xl">
      <p class="mb-1"><strong>SAE Benchmark Lab</strong> &bull; Badan Pusat Statistik (BPS) Two-Stage Cluster Sampling Testbed</p>
      <p class="mb-0 text-muted font-monospace">Generated automatically on {{TIMESTAMP}}</p>
    </div>
  </footer>

  <!-- Scripts -->
  <script src="https://code.jquery.com/jquery-3.7.1.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"></script>
  <script src="https://cdn.datatables.net/2.0.2/js/dataTables.min.js"></script>
  <script src="https://cdn.datatables.net/2.0.2/js/dataTables.bootstrap5.min.js"></script>

  <script>
    $(document).ready(function() {
      var table = $(\'#leaderboardTable\').DataTable({
        pageLength: 25,
        lengthMenu: [[10, 25, 40, -1], [10, 25, 40, "All"]],
        order: [[0, "asc"], [4, "desc"]],
        language: {
          search: "_INPUT_",
          searchPlaceholder: "Search models, datasets..."
        },
        dom: \'<"d-flex justify-content-between align-items-center flex-wrap gap-2 mb-3"lf>rt<"d-flex justify-content-between align-items-center flex-wrap gap-2 mt-3"ip>\'
      });

      $(\'#datasetFilter\').on(\'change\', function() {
        var val = $(this).val();
        table.column(0).search(val ? val : \'\', true, false).draw();
      });

      $(\'#modelFilter\').on(\'change\', function() {
        var val = $(this).val();
        table.column(1).search(val ? val : \'\', true, false).draw();
      });

      $(\'#resetFiltersBtn\').on(\'click\', function() {
        $(\'#datasetFilter\').val(\'\');
        $(\'#modelFilter\').val(\'\');
        table.search(\'\').columns().search(\'\').draw();
      });
    });

    function copyCode(id, btn) {
      var el = document.getElementById(id);
      var text = el.innerText || el.textContent;
      if (navigator.clipboard && window.isSecureContext) {
        navigator.clipboard.writeText(text).then(function() {
          showCopied(btn);
        }).catch(function() {
          fallbackCopy(text, btn);
        });
      } else {
        fallbackCopy(text, btn);
      }
    }

    function showCopied(btn) {
      if (!btn) return;
      var origHtml = btn.innerHTML;
      btn.innerHTML = \'<i class="fa-solid fa-check text-success me-1"></i> Copied!\';
      btn.classList.add(\'btn-success\');
      btn.classList.remove(\'btn-outline-secondary\');
      setTimeout(function() {
        btn.innerHTML = origHtml;
        btn.classList.remove(\'btn-success\');
        btn.classList.add(\'btn-outline-secondary\');
      }, 2000);
    }

    function fallbackCopy(text, btn) {
      var textArea = document.createElement("textarea");
      textArea.value = text;
      textArea.style.position = "fixed";
      textArea.style.left = "-999999px";
      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();
      try {
        document.execCommand("copy");
        showCopied(btn);
      } catch (err) {
        console.error("Fallback copy failed", err);
      }
      document.body.removeChild(textArea);
    }
  </script>
</body>
</html>'

  render_page <- function(is_docs = TRUE) {
    plots_path <- if (is_docs) "plots" else "docs/plots"
    
    html <- base_template
    html <- gsub("{{TOTAL_MODELS}}", as.character(total_models), html, fixed = TRUE)
    html <- gsub("{{TOTAL_DATASETS}}", as.character(total_datasets), html, fixed = TRUE)
    html <- gsub("{{TOP_EFF_VAL}}", top_eff_val, html, fixed = TRUE)
    html <- gsub("{{TOP_EFF_DETAIL}}", top_eff_detail, html, fixed = TRUE)
    html <- gsub("{{FASTEST_MODEL}}", fastest_model_name, html, fixed = TRUE)
    html <- gsub("{{FASTEST_TIME}}", fastest_model_time, html, fixed = TRUE)
    html <- gsub("{{LOWEST_RAM_MODEL}}", lowest_ram_model, html, fixed = TRUE)
    html <- gsub("{{LOWEST_RAM_VAL}}", lowest_ram_val, html, fixed = TRUE)
    html <- gsub("{{TABLE_ROWS}}", table_rows_html, html, fixed = TRUE)
    html <- gsub("{{PLOTS_PATH}}", plots_path, html, fixed = TRUE)
    html <- gsub("{{CATALOGUE_CARDS}}", catalogue_cards_html, html, fixed = TRUE)
    html <- gsub("{{TIMESTAMP}}", timestamp_str, html, fixed = TRUE)
    
    return(html)
  }
  
  # Write docs/index.html
  docs_html <- render_page(is_docs = TRUE)
  docs_index_path <- file.path(docs_dir, "index.html")
  writeLines(docs_html, docs_index_path)
  cat(sprintf("[+] Successfully generated: %s\n", docs_index_path))
  
  # Write index.html at root
  root_html <- render_page(is_docs = FALSE)
  root_index_path <- file.path(root_dir, "index.html")
  writeLines(root_html, root_index_path)
  cat(sprintf("[+] Successfully generated: %s\n", root_index_path))
  
  return(invisible(TRUE))
}

# Run directly if invoked from command line
if (sys.nframe() == 0L) {
  generate_benchmark_dashboard()
}
