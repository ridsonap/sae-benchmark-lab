#!/usr/bin/env Rscript
#' SAE Benchmark Lab - Responsive HTML Generator
#' Features:
#' - Minimalist light theme by default with dark mode toggle
#' - Clean hero title without shouting BPS testbed (moved to methodology section)
#' - Separate tables per dataset archetype with clean short names
#' - Standardized model names: nama package (fungsi, fitur/spesifikasi)
#' - Separated Rank and Model columns for maximum visual cleanliness
#' - Highlighted best value cells in each metric column
#' - Interactive Chart.js charts (Runtime vs Efficiency, Efficiency bars) with dataset filter
#' - Bilingual switch (Bahasa Indonesia & English) adhering to EYD V & natural domain terminology

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
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
  
  if (!dir.exists(docs_dir)) dir.create(docs_dir, recursive = TRUE)
  docs_plots_dir <- file.path(docs_dir, "plots")
  if (!dir.exists(docs_plots_dir)) dir.create(docs_plots_dir, recursive = TRUE)
  
  # Copy static plot assets if they exist
  res_plots_dir <- file.path(root_dir, "results", "plots")
  for (pname in c("leaderboard_rrmse_comparison.png", "leaderboard_relative_efficiency.png", "mc_replication_shrinkage.png")) {
    psrc <- file.path(res_plots_dir, pname)
    if (file.exists(psrc)) {
      file.copy(psrc, file.path(docs_plots_dir, pname), overwrite = TRUE)
    }
  }
  
  # Load Monte Carlo replications if available
  mc_path <- file.path(root_dir, "results", "mc_replications_leaderboard.csv")
  df_mc <- if (file.exists(mc_path)) read.csv(mc_path, stringsAsFactors = FALSE) else NULL
  
  # Summary KPIs
  total_models <- length(unique(df$model))
  total_datasets <- length(unique(df$dataset_id))
  
  non_direct <- df[!grepl("direct", df$model, ignore.case = TRUE) & !is.na(df$RelEff_pct), ]
  if (nrow(non_direct) > 0) {
    top_eff_idx <- which.max(non_direct$RelEff_pct)
    top_eff_val <- sprintf("%.1f%%", non_direct$RelEff_pct[top_eff_idx])
    top_eff_model <- non_direct$model[top_eff_idx]
    top_eff_ds <- non_direct$dataset_name_id[top_eff_idx]
  } else {
    top_eff_val <- "100.0%"
    top_eff_model <- "N/A"
    top_eff_ds <- "N/A"
  }
  
  # Fastest non-direct model
  model_runtimes <- non_direct %>%
    group_by(model) %>%
    summarise(mean_time = mean(Runtime_sec, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_time)
  fastest_model_name <- if (nrow(model_runtimes) > 0) model_runtimes$model[1] else "N/A"
  fastest_model_time <- if (nrow(model_runtimes) > 0) sprintf("%.2fs rata-rata", model_runtimes$mean_time[1]) else "N/A"
  
  # Lowest non-direct RAM
  model_ram <- non_direct %>%
    filter(!is.na(Peak_RAM_MB)) %>%
    group_by(model) %>%
    summarise(mean_ram = mean(Peak_RAM_MB, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_ram)
  lowest_ram_model <- if (nrow(model_ram) > 0) model_ram$model[1] else "N/A"
  lowest_ram_val <- if (nrow(model_ram) > 0) sprintf("%.1f MB rata-rata", model_ram$mean_ram[1]) else "N/A"
  
  # Dataset archetype dictionary with refined bilingual metadata (EYD V & KBBI compliant)
  dataset_meta <- list(
    "ds01_continuous_linear" = list(
      code = "ds01-linear",
      name_en = "Linear Baseline",
      name_id = "Linear Dasar",
      badge_en = "Continuous Linear",
      badge_id = "Linear Kontinu",
      dgp_en = "Log per-capita household expenditure, 3 linear covariates, normal random effects.",
      dgp_id = "Log pengeluaran per kapita rumah tangga dengan 3 kovariat linear dan pengaruh acak normal.",
      challenge_en = "Baseline shrinkage efficiency gain of EBLUP and Hierarchical Bayes over Direct.",
      challenge_id = "Penyusutan (shrinkage) awal untuk menguji efisiensi EBLUP dan Hierarchical Bayes dibanding Penduga Langsung."
    ),
    "ds02_bounded_rate" = list(
      code = "ds02-rate",
      name_en = "Bounded Rate (0,1)",
      name_id = "Proporsi Terbatas (0,1)",
      badge_en = "Unit Interval (0, 1)",
      badge_id = "Interval Satuan (0, 1)",
      dgp_en = "Household poverty indicators aggregated to district rates strictly bounded in (0, 1).",
      dgp_id = "Indikator kemiskinan rumah tangga diagregasikan menjadi proporsi kabupaten pada rentang ketat (0, 1).",
      challenge_en = "Evaluates Beta Stan HMC and logit links; prevents rates < 0 or > 100%.",
      challenge_id = "Menguji model Beta Stan HMC dan transformasi logit agar estimasi tidak bernilai negatif atau melebihi 100%."
    ),
    "ds03_highdim_sparse" = list(
      code = "ds03-sparse",
      name_en = "High-Dim Sparse",
      name_id = "Dimensi Tinggi (Sparse)",
      badge_en = "25 Auxiliary Features",
      badge_id = "25 Peubah Penjelas",
      dgp_en = "25 area-level features containing only 3 true signals and 22 collinear noise variables.",
      dgp_id = "25 peubah tingkat area dengan 3 sinyal nyata dan 22 peubah derau multikolinear.",
      challenge_en = "Evaluates regularization, horseshoe shrinkage, and feature screening against overfitting.",
      challenge_id = "Menguji regularisasi, penyusutan horseshoe, dan penapisan fitur untuk mencegah lewat-tata (overfitting)."
    ),
    "ds04_nonlinear_interaction" = list(
      code = "ds04-nonlinear",
      name_en = "Nonlinear Complex",
      name_id = "Nonlinear Kompleks",
      badge_en = "Tree / Machine Learning",
      badge_id = "Tree / Machine Learning",
      dgp_en = "Complex DGP with sine waves, quadratic terms, square roots, and multiplicative interactions.",
      dgp_id = "Pembangkitan data nonlinier dengan gelombang sinus, suku kuadratik, akar kuadrat, dan interaksi perkalian.",
      challenge_en = "Linear Fay-Herriot misspecification; tests Mixed Effects Random Forests (MERF).",
      challenge_id = "Menguji ketahanan model nonparametrik MERF ketika model linear Fay-Herriot mengalami kesalahan spesifikasi."
    ),
    "ds05_spatial_correlated" = list(
      code = "ds05-spatial",
      name_en = "Spatial SAR",
      name_id = "Spasial SAR",
      badge_en = "SAR rho=0.65 (Matrix W)",
      badge_id = "SAR rho=0,65 (Matriks W)",
      dgp_en = "Area random effects generated via simultaneous autoregressive SAR (rho = 0.65) over Queen matrix.",
      dgp_id = "Pengaruh acak area dibangkitkan dari proses autoregresif spasial SAR (rho = 0,65) dengan matriks ketetanggaan Queen.",
      challenge_en = "Spatial borrowing of strength through Spatial Fay-Herriot (SEBLUP) and INLA Besag.",
      challenge_id = "Pemanfaatan korelasi spasial antarwilayah melalui Spatial Fay-Herriot (SEBLUP) dan model INLA Besag."
    ),
    "ds06_spatiotemporal_panel" = list(
      code = "ds06-panel",
      name_en = "Panel Spatio-Temporal",
      name_id = "Panel Spasiotemporal",
      badge_en = "Panel D=50 x T=5",
      badge_id = "Panel D=50 x T=5",
      dgp_en = "Repeated survey panel across 5 survey rounds with AR(1) autocorrelation (phi = 0.70).",
      dgp_id = "Panel survei berulang 5 putaran dengan autokorelasi serial AR(1) (phi = 0,70).",
      challenge_en = "Dynamic temporal filters and longitudinal borrowing of strength across time and space.",
      challenge_id = "Penyaringan dinamik runtut waktu dan pemanfaatan kekuatan informasi lintas ruang dan waktu."
    ),
    "ds07_extreme_outliers" = list(
      code = "ds07-outliers",
      name_en = "Extreme Outliers",
      name_id = "Pencilan Ekstrem",
      badge_en = "Robust Huber SAE",
      badge_id = "Huber Robust SAE",
      dgp_en = "Contaminated response generating 4 disaster shock districts with +/- 8 sigma outlier shocks.",
      dgp_id = "Data terkontaminasi dengan 4 kabupaten yang mengalami guncangan ekstrem (+/- 8 sigma dari tren regresi).",
      challenge_en = "Verifies robust M-estimation, Huber EBLUP (saeRobust), and heavy-tailed shrinkage.",
      challenge_id = "Menguji ketahanan estimasi M-robust, Huber EBLUP (saeRobust), dan penyusutan berekor tebal terhadap titik pengungkit."
    ),
    "ds08_nested_subarea" = list(
      code = "ds08-nested",
      name_en = "Nested Hierarchy",
      name_id = "Hierarki Bertingkat",
      badge_en = "Two-Fold Subarea",
      badge_id = "Dua Tingkat Wilayah",
      dgp_en = "Hierarchical structure with districts (kabupaten) nested inside administrative provinces.",
      dgp_id = "Struktur bertingkat dengan kabupaten yang bersarang di dalam provinsi administratif (v_p + u_pd).",
      challenge_en = "Multi-level borrowing of strength across multiple administrative tiers.",
      challenge_id = "Pemanfaatan informasi bertingkat antartingkat hierarki administratif."
    )
  )
  
  # Format package badge helper
  get_package_badge <- function(model_str) {
    pkg <- strsplit(model_str, " ")[[1]][1]
    pkg_badge_class <- switch(
      pkg,
      "fastsaegpu" = "badge-purple",
      "fastsae" = "badge-blue",
      "tipsae" = "badge-teal",
      "hbsae" = "badge-indigo",
      "sae" = "badge-sky",
      "saeRobust" = "badge-amber",
      "survey" = "badge-gray",
      "badge-gray"
    )
    sprintf('<span class="pkg-badge %s">%s</span>', pkg_badge_class, model_str)
  }
  
  # Helper to render table for either single sample or MC
  render_dataset_table <- function(sub_df, ds_id, is_mc = FALSE) {
    if (is.null(sub_df) || nrow(sub_df) == 0) {
      return('<div class="p-4 text-muted small text-center"><i class="fa-solid fa-circle-exclamation me-1"></i>Data simulasi Monte Carlo sedang diproses untuk dataset ini.</div>')
    }
    
    val_rrmse <- sub_df$RRMSE_pct[!is.na(sub_df$RRMSE_pct)]
    val_arb <- sub_df$ARB_pct[!is.na(sub_df$ARB_pct)]
    val_eff <- sub_df$RelEff_pct[!is.na(sub_df$RelEff_pct)]
    val_corr <- sub_df$Corr[!is.na(sub_df$Corr)]
    val_ram <- sub_df$Peak_RAM_MB[!is.na(sub_df$Peak_RAM_MB) & sub_df$Peak_RAM_MB > 0]
    val_time <- sub_df$Runtime_sec[!is.na(sub_df$Runtime_sec)]
    
    b <- list(
      min_rrmse = if (length(val_rrmse) > 0) min(val_rrmse) else NA_real_,
      min_arb = if (length(val_arb) > 0) min(val_arb) else NA_real_,
      max_eff = if (length(val_eff) > 0) max(val_eff) else NA_real_,
      max_corr = if (length(val_corr) > 0) max(val_corr) else NA_real_,
      min_ram = if (length(val_ram) > 0) min(val_ram) else NA_real_,
      min_time = if (length(val_time) > 0) min(val_time) else NA_real_
    )
    if ("Outlier_RRMSE" %in% names(sub_df)) {
      val_out <- sub_df$Outlier_RRMSE[!is.na(sub_df$Outlier_RRMSE)]
      b$min_outlier <- if (length(val_out) > 0) min(val_out) else NA_real_
    }
    
    is_outlier_ds <- (ds_id == "ds07_extreme_outliers")
    is_bounded_ds <- (ds_id == "ds02_bounded_rate")
    
    rows <- character()
    for (j in seq_len(nrow(sub_df))) {
      row <- sub_df[j, ]
      
      # Rank badge (dedicated clean cell)
      rank_badge <- if (j == 1) {
        '<span class="rank-badge rank-1"><i class="fa-solid fa-crown text-warning me-1"></i>#1</span>'
      } else if (j == 2) {
        '<span class="rank-badge rank-2">#2</span>'
      } else if (j == 3) {
        '<span class="rank-badge rank-3">#3</span>'
      } else {
        sprintf('<span class="rank-badge rank-n">#%d</span>', j)
      }
      
      tol <- 1e-4
      is_best_rrmse <- !is.na(row$RRMSE_pct) && !is.na(b$min_rrmse) && abs(row$RRMSE_pct - b$min_rrmse) <= tol
      is_best_arb <- !is.na(row$ARB_pct) && !is.na(b$min_arb) && abs(row$ARB_pct - b$min_arb) <= tol
      is_best_eff <- !is.na(row$RelEff_pct) && !is.na(b$max_eff) && abs(row$RelEff_pct - b$max_eff) <= tol
      is_best_corr <- !is.na(row$Corr) && !is.na(b$max_corr) && abs(row$Corr - b$max_corr) <= tol
      is_best_time <- !is.na(row$Runtime_sec) && !is.na(b$min_time) && abs(row$Runtime_sec - b$min_time) <= tol
      is_best_ram <- !is.na(row$Peak_RAM_MB) && !is.na(b$min_ram) && abs(row$Peak_RAM_MB - b$min_ram) <= 0.1 && row$Peak_RAM_MB > 0
      
      cell_rrmse <- sprintf('<td class="text-end %s">%.2f%%%s</td>',
                            if (is_best_rrmse) "best-cell" else "",
                            row$RRMSE_pct,
                            if (is_best_rrmse) ' <i class="fa-solid fa-star text-emerald ms-1 small"></i>' else "")
      
      cell_arb <- sprintf('<td class="text-end %s">%.2f%%%s</td>',
                          if (is_best_arb) "best-cell" else "",
                          row$ARB_pct,
                          if (is_best_arb) ' <i class="fa-solid fa-star text-emerald ms-1 small"></i>' else "")
      
      eff_style <- if (is_best_eff) "best-cell" else if (row$RelEff_pct > 105) "text-success fw-semibold" else if (row$RelEff_pct >= 99) "text-body" else "text-danger"
      cell_eff <- sprintf('<td class="text-end %s">%.1f%%%s</td>',
                          eff_style,
                          row$RelEff_pct,
                          if (is_best_eff) ' <i class="fa-solid fa-star text-emerald ms-1 small"></i>' else "")
      
      cell_corr <- sprintf('<td class="text-end %s">%.4f%s</td>',
                           if (is_best_corr) "best-cell" else "",
                           row$Corr,
                           if (is_best_corr) ' <i class="fa-solid fa-star text-emerald ms-1 small"></i>' else "")
      
      ram_val <- if (!is.na(row$Peak_RAM_MB)) sprintf("%.1f MB", row$Peak_RAM_MB) else "-"
      cell_ram <- sprintf('<td class="text-end font-monospace %s">%s</td>',
                          if (is_best_ram) "best-cell" else "", ram_val)
      
      time_val <- if (!is.na(row$Runtime_sec)) sprintf("%.3fs", row$Runtime_sec) else "-"
      cell_time <- sprintf('<td class="text-end font-monospace %s">%s</td>',
                           if (is_best_time) "best-cell" else "", time_val)
      
      extra_cells <- ""
      if (is_outlier_ds) {
        out_rrmse <- if ("Outlier_RRMSE" %in% names(row) && !is.na(row$Outlier_RRMSE)) sprintf("%.2f%%", row$Outlier_RRMSE) else "-"
        is_best_out <- !is.na(out_rrmse) && out_rrmse != "-" && !is.na(b$min_outlier) && abs(row$Outlier_RRMSE - b$min_outlier) <= tol
        extra_cells <- sprintf('<td class="text-end %s">%s%s</td>',
                               if (is_best_out) "best-cell" else "",
                               out_rrmse,
                               if (is_best_out) ' <i class="fa-solid fa-star text-emerald ms-1 small"></i>' else "")
      } else if (is_bounded_ds) {
        b_viol <- if ("Boundary_Violations" %in% names(row) && !is.na(row$Boundary_Violations)) as.character(row$Boundary_Violations) else "0"
        extra_cells <- sprintf('<td class="text-center font-monospace %s">%s</td>',
                               if (b_viol == "0") "text-success fw-bold" else "text-danger fw-bold", b_viol)
      }
      
      row_html <- paste0(
        '<tr>',
        '<td class="text-center">', rank_badge, '</td>',
        '<td>', get_package_badge(row$model), '</td>',
        cell_eff,
        cell_rrmse,
        cell_arb,
        cell_corr,
        extra_cells,
        cell_ram,
        cell_time,
        '</tr>'
      )
      rows <- c(rows, row_html)
    }
    
    extra_th_en <- ""
    if (is_outlier_ds) {
      extra_th_en <- '<th class="text-end" data-i18n="col_outlier_rrmse">RRMSE Pencilan</th>'
    } else if (is_bounded_ds) {
      extra_th_en <- '<th class="text-center" data-i18n="col_violations">Pelanggaran Batas (&lt;0 / &gt;1)</th>'
    }
    
    mc_notice <- if (is_mc) {
      '<div class="px-3 py-2 bg-primary-subtle text-primary border-bottom small d-flex align-items-center justify-content-between flex-wrap gap-2"><span><i class="fa-solid fa-arrows-rotate me-1"></i><strong>Monte Carlo (R=30 Replikasi):</strong> Metrik empiris dari 30 penarikan sampel berulang terhadap finite population ground truth.</span><span class="badge bg-primary text-white">Empirical MSE & RelEff</span></div>'
    } else {
      '<div class="px-3 py-2 bg-body-tertiary text-muted border-bottom small d-flex align-items-center justify-content-between flex-wrap gap-2"><span><i class="fa-solid fa-cube me-1"></i><strong>Sampel Tunggal (Single Sample):</strong> Evaluasi terhadap finite population ground truth dari 1 set survei Susenas.</span><span class="badge bg-secondary-subtle text-secondary border">Single Realization</span></div>'
    }
    
    table_wrapper <- paste0(
      mc_notice,
      '<div class="table-responsive">',
      '<table class="table table-hover align-middle mb-0 benchmark-table">',
      '<thead>',
      '<tr class="table-header-row text-uppercase small">',
      '<th class="text-center" style="width: 84px;" data-i18n="col_rank">Peringkat</th>',
      '<th data-i18n="col_model">Model / Paket</th>',
      '<th class="text-end" data-i18n="col_releff">Efisiensi Relatif (%)</th>',
      '<th class="text-end" data-i18n="col_rrmse">RRMSE (%)</th>',
      '<th class="text-end" data-i18n="col_arb">ARB (%)</th>',
      '<th class="text-end" data-i18n="col_corr">Korelasi</th>',
      extra_th_en,
      '<th class="text-end" data-i18n="col_ram">RAM Puncak</th>',
      '<th class="text-end" data-i18n="col_runtime">Waktu</th>',
      '</tr>',
      '</thead>',
      '<tbody>',
      paste(rows, collapse = "\n"),
      '</tbody>',
      '</table>',
      '</div>'
    )
    table_wrapper
  }
  
  dataset_tables_single <- list()
  dataset_tables_mc <- list()
  for (ds_id in names(dataset_meta)) {
    dataset_tables_single[[ds_id]] <- render_dataset_table(df[df$dataset_id == ds_id, ], ds_id, is_mc = FALSE)
    dataset_tables_mc[[ds_id]] <- if (!is.null(df_mc)) render_dataset_table(df_mc[df_mc$dataset_id == ds_id, ], ds_id, is_mc = TRUE) else dataset_tables_single[[ds_id]]
  }
  
  # Build JSON data payload for interactive charts
  chart_data_list <- list()
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    pkg <- strsplit(row$model, " ")[[1]][1]
    chart_data_list[[i]] <- list(
      dataset_id = row$dataset_id,
      dataset_code = row$dataset_code,
      dataset_name_en = row$dataset_name_en,
      dataset_name_id = row$dataset_name_id,
      model = row$model,
      package = pkg,
      RelEff = as.numeric(row$RelEff_pct),
      RRMSE = as.numeric(row$RRMSE_pct),
      ARB = as.numeric(row$ARB_pct),
      Runtime = as.numeric(row$Runtime_sec),
      RAM = as.numeric(row$Peak_RAM_MB)
    )
  }
  chart_json_str <- toJSON(chart_data_list, auto_unbox = TRUE)
  
  timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  
  # Build Tabs HTML
  tabs_nav <- character()
  tab_panes <- character()
  first <- TRUE
  
  for (ds_id in names(dataset_meta)) {
    meta <- dataset_meta[[ds_id]]
    active_class <- if (first) "active" else ""
    pane_active <- if (first) "show active" else ""
    first <- FALSE
    
    tab_btn <- sprintf(
      '<li class="nav-item" role="presentation">
        <button class="nav-link %s" id="tab-%s" data-bs-toggle="pill" data-bs-target="#pane-%s" type="button" role="tab">
          <span class="font-monospace fw-semibold">%s</span>
          <span class="d-none d-md-inline ms-1 text-muted small tab-name" data-name-en="%s" data-name-id="%s">%s</span>
        </button>
      </li>',
      active_class, meta$code, meta$code, meta$code, meta$name_en, meta$name_id, meta$name_id
    )
    tabs_nav <- c(tabs_nav, tab_btn)
    
    tbl_single <- dataset_tables_single[[ds_id]]
    tbl_mc <- dataset_tables_mc[[ds_id]]
    pane <- sprintf(
      '<div class="tab-pane fade %s" id="pane-%s" role="tabpanel">
        <div class="card dataset-card mb-4">
          <div class="card-header bg-transparent border-bottom d-flex justify-content-between align-items-center flex-wrap gap-2 py-3">
            <div>
              <div class="d-flex align-items-center gap-2 mb-1">
                <span class="badge bg-primary-subtle text-primary border border-primary-subtle font-monospace">%s</span>
                <span class="badge bg-secondary-subtle text-secondary rounded-pill badge-meta" data-en="%s" data-id="%s">%s</span>
              </div>
              <h4 class="h5 fw-bold mb-0 text-title" data-en="%s" data-id="%s">%s</h4>
            </div>
            <div class="text-end">
              <span class="text-muted small"><i class="fa-solid fa-bullseye text-warning me-1"></i><span data-i18n="benchmark_goal">Tujuan Pengujian:</span></span>
              <p class="text-body-secondary small mb-0 challenge-text" data-en="%s" data-id="%s">%s</p>
            </div>
          </div>
          <div class="card-body p-0">
            <div class="table-mode table-mode-single">
              %s
            </div>
            <div class="table-mode table-mode-mc d-none">
              %s
            </div>
          </div>
          <div class="card-footer bg-transparent border-top py-2 px-3 d-flex justify-content-between align-items-center text-muted small">
            <span><i class="fa-solid fa-circle-info me-1"></i><span data-i18n="dgp_label">Struktur DGP:</span> <span class="dgp-text" data-en="%s" data-id="%s">%s</span></span>
            <span class="badge bg-light text-dark border"><i class="fa-solid fa-crown text-warning me-1"></i><span data-i18n="best_legend">Nilai terbaik ditandai hijau</span></span>
          </div>
        </div>
      </div>',
      pane_active, meta$code, meta$code, meta$badge_en, meta$badge_id, meta$badge_id,
      meta$name_en, meta$name_id, meta$name_id,
      meta$challenge_en, meta$challenge_id, meta$challenge_id,
      tbl_single, tbl_mc,
      meta$dgp_en, meta$dgp_id, meta$dgp_id
    )
    tab_panes <- c(tab_panes, pane)
  }
  
  # Dataset dropdown options
  ds_options <- character()
  for (ds_id in names(dataset_meta)) {
    meta <- dataset_meta[[ds_id]]
    opt <- sprintf('<option value="%s">%s (%s)</option>', ds_id, meta$name_id, meta$code)
    ds_options <- c(ds_options, opt)
  }
  
  # Base HTML Template (using placeholders)
  html_template <- '<!DOCTYPE html>
<html lang="id" data-bs-theme="light">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>SAE Benchmark Lab | Evaluasi Model Small Area Estimation</title>
  
  <!-- CSS Frameworks -->
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css">
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600&display=swap" rel="stylesheet">
  
  <!-- Chart.js -->
  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.2/dist/chart.umd.min.js"></script>

  <style>
    :root {
      --font-sans: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      --font-mono: "JetBrains Mono", monospace;
      --bg-page: #f8fafc;
      --bg-card: #ffffff;
      --border-subtle: #e2e8f0;
      --text-main: #0f172a;
      --text-muted: #64748b;
      --best-bg: #ecfdf5;
      --best-text: #047857;
      --best-border: #a7f3d0;
    }

    [data-bs-theme="dark"] {
      --bg-page: #090d16;
      --bg-card: #111827;
      --border-subtle: #1f2937;
      --text-main: #f3f4f6;
      --text-muted: #9ca3af;
      --best-bg: rgba(16, 185, 129, 0.16);
      --best-text: #6ee7b7;
      --best-border: rgba(16, 185, 129, 0.4);
    }

    body {
      font-family: var(--font-sans);
      background-color: var(--bg-page);
      color: var(--text-main);
      min-height: 100vh;
      transition: background-color 0.2s ease, color 0.2s ease;
    }

    .font-monospace {
      font-family: var(--font-mono) !important;
    }

    .navbar {
      background-color: var(--bg-card);
      border-bottom: 1px solid var(--border-subtle);
      transition: background-color 0.2s ease, border-color 0.2s ease;
    }

    .hero-section {
      padding: 3rem 0 2rem;
      border-bottom: 1px solid var(--border-subtle);
      background: radial-gradient(circle at 50% 0%, rgba(37, 99, 235, 0.05) 0%, transparent 65%);
    }

    .kpi-card {
      background-color: var(--bg-card);
      border: 1px solid var(--border-subtle);
      border-radius: 10px;
      transition: transform 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.03);
    }
    .kpi-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05);
      border-color: #3b82f6;
    }

    .dataset-card {
      background-color: var(--bg-card);
      border: 1px solid var(--border-subtle);
      border-radius: 12px;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04);
      overflow: hidden;
    }

    .nav-pills .nav-link {
      color: var(--text-muted);
      border-radius: 8px;
      padding: 0.5rem 0.9rem;
      font-size: 0.875rem;
      border: 1px solid transparent;
      transition: all 0.15s ease;
    }
    .nav-pills .nav-link:hover {
      background-color: rgba(59, 130, 246, 0.08);
      color: #2563eb;
    }
    .nav-pills .nav-link.active {
      background-color: #2563eb;
      color: #ffffff !important;
    }
    .nav-pills .nav-link.active .tab-name {
      color: rgba(255, 255, 255, 0.85) !important;
    }

    .benchmark-table {
      font-size: 0.875rem;
    }
    .benchmark-table th {
      background-color: rgba(148, 163, 184, 0.05);
      border-bottom: 1px solid var(--border-subtle);
      color: var(--text-muted);
      font-weight: 600;
      letter-spacing: 0.03em;
      padding: 0.75rem 1rem;
    }
    .benchmark-table td {
      border-bottom: 1px solid var(--border-subtle);
      padding: 0.75rem 1rem;
    }
    .benchmark-table tbody tr:hover td {
      background-color: rgba(59, 130, 246, 0.04);
    }

    /* Cell best styling */
    .best-cell {
      background-color: var(--best-bg) !important;
      color: var(--best-text) !important;
      font-weight: 700 !important;
      border-radius: 4px;
    }

    /* Rank badges (distinct neat column) */
    .rank-badge {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      min-width: 44px;
      font-size: 0.8rem;
      font-weight: 700;
      padding: 3px 8px;
      border-radius: 6px;
    }
    .rank-1 { background-color: rgba(234, 179, 8, 0.15); color: #ca8a04; border: 1px solid rgba(234, 179, 8, 0.35); }
    .rank-2 { background-color: rgba(148, 163, 184, 0.15); color: #64748b; border: 1px solid rgba(148, 163, 184, 0.35); }
    .rank-3 { background-color: rgba(217, 119, 6, 0.15); color: #b45309; border: 1px solid rgba(217, 119, 6, 0.35); }
    .rank-n { background-color: transparent; color: var(--text-muted); font-weight: 600; }

    /* Package Badges */
    .pkg-badge {
      font-family: var(--font-mono);
      font-size: 0.8rem;
      font-weight: 600;
      padding: 4px 8px;
      border-radius: 6px;
      border: 1px solid transparent;
      display: inline-block;
    }
    .badge-purple { background-color: rgba(168, 85, 247, 0.12); color: #9333ea; border-color: rgba(168, 85, 247, 0.25); }
    .badge-blue { background-color: rgba(59, 130, 246, 0.12); color: #2563eb; border-color: rgba(59, 130, 246, 0.25); }
    .badge-teal { background-color: rgba(20, 184, 166, 0.12); color: #0d9488; border-color: rgba(20, 184, 166, 0.25); }
    .badge-indigo { background-color: rgba(99, 102, 241, 0.12); color: #4f46e5; border-color: rgba(99, 102, 241, 0.25); }
    .badge-sky { background-color: rgba(2, 132, 199, 0.12); color: #0284c7; border-color: rgba(2, 132, 199, 0.25); }
    .badge-amber { background-color: rgba(245, 158, 11, 0.12); color: #d97706; border-color: rgba(245, 158, 11, 0.25); }
    .badge-gray { background-color: rgba(148, 163, 184, 0.12); color: #475569; border-color: rgba(148, 163, 184, 0.25); }

    [data-bs-theme="dark"] .badge-purple { color: #c084fc; border-color: rgba(168, 85, 247, 0.35); }
    [data-bs-theme="dark"] .badge-blue { color: #60a5fa; border-color: rgba(59, 130, 246, 0.35); }
    [data-bs-theme="dark"] .badge-teal { color: #2dd4bf; border-color: rgba(20, 184, 166, 0.35); }
    [data-bs-theme="dark"] .badge-indigo { color: #818cf8; border-color: rgba(99, 102, 241, 0.35); }
    [data-bs-theme="dark"] .badge-sky { color: #38bdf8; border-color: rgba(2, 132, 199, 0.35); }
    [data-bs-theme="dark"] .badge-amber { color: #fbbf24; border-color: rgba(245, 158, 11, 0.35); }
    [data-bs-theme="dark"] .badge-gray { color: #94a3b8; border-color: rgba(148, 163, 184, 0.35); }

    .chart-container-box {
      background-color: var(--bg-card);
      border: 1px solid var(--border-subtle);
      border-radius: 12px;
      padding: 1.25rem;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04);
      height: 100%;
    }

    .methodology-box {
      background-color: var(--bg-card);
      border: 1px solid var(--border-subtle);
      border-radius: 12px;
      padding: 1.5rem;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04);
    }
  </style>
</head>
<body>

  <!-- Navigation -->
  <nav class="navbar navbar-expand-lg sticky-top py-2">
    <div class="container-xl">
      <a class="navbar-brand d-flex align-items-center gap-2 fw-bold text-decoration-none" href="#">
        <i class="fa-solid fa-chart-line text-primary fs-4"></i>
        <span class="text-body fw-bold fs-5">SAE Benchmark Lab</span>
      </a>

      <div class="d-flex align-items-center gap-2">
        <!-- Language Switcher Button -->
        <button id="langToggleBtn" class="btn btn-outline-secondary btn-sm rounded-pill px-3 d-flex align-items-center gap-1" title="Ganti Bahasa / Switch Language">
          <i class="fa-solid fa-globe"></i>
          <span id="langText" class="fw-semibold font-monospace">EN</span>
        </button>

        <!-- Dark / Light Theme Toggle -->
        <button id="themeToggleBtn" class="btn btn-outline-secondary btn-sm rounded-circle p-2" style="width: 34px; height: 34px;" title="Mode Gelap/Terang">
          <i id="themeIcon" class="fa-solid fa-moon"></i>
        </button>

        <a href="https://github.com/ridsonap/sae-benchmark-lab" target="_blank" class="btn btn-outline-secondary btn-sm rounded-pill px-3 d-none d-sm-inline-flex align-items-center gap-1">
          <i class="fa-brands fa-github"></i>
          <span>GitHub</span>
        </a>
      </div>
    </div>
  </nav>

  <!-- Hero Header -->
  <section class="hero-section text-center">
    <div class="container-xl">
      <div class="d-flex justify-content-center gap-2 mb-2 flex-wrap">
        <span class="badge bg-primary-subtle text-primary border border-primary-subtle rounded-pill px-3 py-1">
          <i class="fa-solid fa-layer-group me-1"></i> <span data-i18n="badge_archetypes">8 Ragam Dataset</span>
        </span>
        <span class="badge bg-success-subtle text-success border border-success-subtle rounded-pill px-3 py-1">
          <i class="fa-solid fa-bullseye me-1"></i> <span data-i18n="badge_ground_truth">Populasi Murni (Ground Truth)</span>
        </span>
        <span class="badge bg-info-subtle text-info border border-info-subtle rounded-pill px-3 py-1">
          <i class="fa-solid fa-microchip me-1"></i> <span data-i18n="badge_profiling">Profil Waktu & Memori</span>
        </span>
      </div>

      <h1 class="display-6 fw-bold mb-2">SAE Benchmark Lab</h1>
      <p class="lead text-muted mx-auto mb-4" style="max-width: 760px;" data-i18n="hero_subtitle">
        Tolok ukur terstandar untuk mengevaluasi model Small Area Estimation (SAE) terhadap nilai populasi murni (Ground Truth) berdasarkan akurasi, efisiensi relatif, dan efisiensi komputasi.
      </p>

      <!-- KPI Summary Cards -->
      <div class="row g-3 justify-content-center text-start">
        <div class="col-6 col-md-3 col-lg-2">
          <div class="kpi-card p-3">
            <span class="text-muted small fw-medium text-uppercase" data-i18n="kpi_total_models">Total Model</span>
            <div class="fs-4 fw-bold text-body mt-1">{{TOTAL_MODELS}}</div>
            <span class="text-muted small" data-i18n="kpi_total_models_sub">Standar & Robust</span>
          </div>
        </div>
        <div class="col-6 col-md-3 col-lg-2">
          <div class="kpi-card p-3">
            <span class="text-muted small fw-medium text-uppercase" data-i18n="kpi_datasets">Ragam Dataset</span>
            <div class="fs-4 fw-bold text-body mt-1">{{TOTAL_DATASETS}}</div>
            <span class="text-muted small" data-i18n="kpi_datasets_sub">Tantangan SAE</span>
          </div>
        </div>
        <div class="col-6 col-md-3 col-lg-3">
          <div class="kpi-card p-3">
            <span class="text-muted small fw-medium text-uppercase" data-i18n="kpi_top_gain">Efisiensi Tertinggi</span>
            <div class="fs-4 fw-bold text-success mt-1">{{TOP_EFF_VAL}}</div>
            <span class="text-muted small text-truncate d-block">{{TOP_EFF_MODEL}} ({{TOP_EFF_DS}})</span>
          </div>
        </div>
        <div class="col-6 col-md-3 col-lg-3">
          <div class="kpi-card p-3">
            <span class="text-muted small fw-medium text-uppercase" data-i18n="kpi_fastest">Model Tercepat</span>
            <div class="fs-4 fw-bold text-primary mt-1 text-truncate">{{FASTEST_MODEL}}</div>
            <span class="text-muted small">{{FASTEST_TIME}}</span>
          </div>
        </div>
        <div class="col-6 col-md-3 col-lg-2">
          <div class="kpi-card p-3">
            <span class="text-muted small fw-medium text-uppercase" data-i18n="kpi_lowest_ram">Memori Terendah</span>
            <div class="fs-4 fw-bold text-info mt-1 text-truncate">{{LOWEST_RAM_MODEL}}</div>
            <span class="text-muted small">{{LOWEST_RAM_VAL}}</span>
          </div>
        </div>
      </div>
    </div>
  </section>

  <!-- Main Content -->
  <main class="container-xl py-4">

    <!-- Interactive Charts Section -->
    <section class="mb-5">
      <div class="d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2">
        <div>
          <h2 class="h4 fw-bold mb-1"><i class="fa-solid fa-chart-scatter text-primary me-2"></i><span data-i18n="chart_section_title">Visualisasi Interaktif Trade-Off Model</span></h2>
          <p class="text-muted small mb-0" data-i18n="chart_section_subtitle">Analisis perbandingan antara kecepatan komputasi, presisi estimasi, dan efisiensi relatif model</p>
        </div>
        <div class="d-flex align-items-center gap-2">
          <label for="chartDatasetSelect" class="text-muted small text-nowrap fw-semibold"><i class="fa-solid fa-filter me-1"></i><span data-i18n="filter_dataset">Pilih Dataset:</span></label>
          <select id="chartDatasetSelect" class="form-select form-select-sm bg-body border-secondary-subtle">
            <option value="ALL" data-i18n="opt_all_datasets">Semua Dataset</option>
            {{DS_OPTIONS}}
          </select>
        </div>
      </div>

      <div class="row g-3">
        <div class="col-lg-7">
          <div class="chart-container-box">
            <div class="d-flex justify-content-between align-items-center mb-2">
              <h5 class="fs-6 fw-bold mb-0"><i class="fa-solid fa-gauge-high text-info me-1"></i> <span data-i18n="chart_scatter_title">Waktu Komputasi vs Efisiensi Relatif (Pareto Frontier)</span></h5>
              <span class="badge bg-light text-dark border small" data-i18n="chart_scatter_badge">Makin ke atas dan kiri makin unggul</span>
            </div>
            <div style="position: relative; height: 340px;">
              <canvas id="scatterChart"></canvas>
            </div>
          </div>
        </div>
        <div class="col-lg-5">
          <div class="chart-container-box">
            <div class="d-flex justify-content-between align-items-center mb-2">
              <h5 class="fs-6 fw-bold mb-0"><i class="fa-solid fa-bars-staggered text-success me-1"></i> <span data-i18n="chart_bar_title">Perbandingan Efisiensi Relatif (%)</span></h5>
              <span class="badge bg-light text-dark border small" data-i18n="chart_bar_badge">Penduga Langsung (Direct) = 100%</span>
            </div>
            <div style="position: relative; height: 340px;">
              <canvas id="barChart"></canvas>
            </div>
          </div>
        </div>
      </div>

      <!-- Monte Carlo Replications Distribution (Shrinkage Plot) -->
      <div class="row g-3 mt-1">
        <div class="col-12">
          <div class="chart-container-box">
            <div class="d-flex justify-content-between align-items-center mb-2 flex-wrap gap-2">
              <h5 class="fs-6 fw-bold mb-0">
                <i class="fa-solid fa-arrows-split-up-and-left text-primary me-1"></i>
                <span data-i18n="chart_mc_title">Sebaran Replikasi Monte Carlo (R=30) & Efek Penyusutan (Shrinkage)</span>
              </h5>
              <span class="badge bg-danger-subtle text-danger border border-danger-subtle small">
                <i class="fa-solid fa-diamond me-1"></i> <span data-i18n="chart_mc_legend">Titik Merah = Nilai Murni Populasi (Ground Truth)</span>
              </span>
            </div>
            <p class="text-muted small mb-3" data-i18n="chart_mc_desc">
              Perbandingan sebaran estimasi dari 30 penarikan sampel independen (BPS Two-Stage). Penduga Langsung (abu-abu) menyebar lebar mencerminkan varians sampling yang besar, sedangkan model SAE fastsae (biru & hijau) secara konsisten menyusutkan estimasi ke arah nilai Ground Truth dengan stabilitas tinggi.
            </p>
            <div class="text-center p-2 bg-body-tertiary rounded border">
              <img src="plots/mc_replication_shrinkage.png" alt="Monte Carlo Replication Shrinkage Plot" class="img-fluid rounded" style="max-height: 480px; width: auto;" />
            </div>
          </div>
        </div>
      </div>
    </section>

    <!-- Leaderboard Per Dataset Section -->
    <section class="mb-5">
      <div class="d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2">
        <div>
          <h2 class="h4 fw-bold mb-1"><i class="fa-solid fa-table-list text-primary me-2"></i><span data-i18n="table_section_title">Tabel Evaluasi Model per Dataset</span></h2>
          <p class="text-muted small mb-0" data-i18n="table_section_subtitle">Setiap dataset menguji karakteristik struktur khusus; peringkat dipisahkan dan nilai terbaik ditandai hijau lembut.</p>
        </div>
        <div class="d-flex align-items-center gap-2">
          <span class="text-muted small fw-medium" data-i18n="eval_mode_label">Mode Evaluasi:</span>
          <div class="btn-group btn-group-sm" role="group" id="evalModeToggle">
            <button type="button" class="btn btn-primary active" id="btnSingleMode" onclick="switchEvalMode(\"single\")">
              <i class="fa-solid fa-cube me-1"></i> <span data-i18n="mode_single">Sampel Tunggal</span>
            </button>
            <button type="button" class="btn btn-outline-secondary" id="btnMCMode" onclick="switchEvalMode(\"mc\")">
              <i class="fa-solid fa-arrows-rotate me-1"></i> <span data-i18n="mode_mc">Monte Carlo (R=30)</span>
            </button>
          </div>
        </div>
      </div>

      <!-- Dataset Pills Navigation -->
      <ul class="nav nav-pills mb-3 gap-1 overflow-x-auto pb-1 flex-nowrap" id="datasetTabs" role="tablist">
        {{TABS_NAV}}
      </ul>

      <!-- Dataset Tab Content Panes -->
      <div class="tab-content" id="datasetTabContent">
        {{TAB_PANES}}
      </div>
    </section>

    <!-- Benchmarking Property Validation Section -->
    <section class="mb-5">
      <div class="methodology-box">
        <div class="d-flex align-items-center justify-content-between flex-wrap gap-2 mb-3">
          <div class="d-flex align-items-center gap-2">
            <i class="fa-solid fa-scale-balanced text-success fs-5"></i>
            <h3 class="h5 fw-bold mb-0" data-i18n="benchmarking_title">Validasi Properti Benchmarking (Konsistensi Agregat Wilayah)</h3>
          </div>
          <span class="badge bg-success-subtle text-success border border-success-subtle px-3 py-1 font-monospace">
            <i class="fa-solid fa-check-double me-1"></i> <span data-i18n="status_perfect">Presisi Tepat (Deviasi = 0,00)</span>
          </span>
        </div>
        <p class="text-muted small mb-3" data-i18n="benchmarking_desc">
          Salah satu syarat krusial penerapan SAE di Badan Pusat Statistik (BPS) adalah <em>benchmarking property</em>: jumlah estimasi area kecil (kabupaten/kota) harus tepat sama dengan angka resmi estimasi langsung (Direct) tingkat wilayah atasnya (provinsi/nasional). Tanpa benchmarking, model EBLUP standar menghasilkan deviasi agregat. Fitur <code>self_benchmark = TRUE</code> pada paket <code>fastsae</code> menjamin konsistensi matematis secara langsung dalam estimasi parameter tanpa memerlukan penyesuaian <em>pro-rata post-hoc</em>.
        </p>

        <div class="table-responsive">
          <table class="table table-sm table-hover align-middle mb-0 bg-body rounded border">
            <thead class="table-header-row text-uppercase small">
              <tr>
                <th class="ps-3" data-i18n="col_archetype">Karakteristik Dataset</th>
                <th class="text-end" data-i18n="col_target_direct">Target Langsung (&sum; Y<sub>d</sub><sup>dir</sup>)</th>
                <th class="text-end" data-i18n="col_eblup_raw">EBLUP Standar (&sum; &theta;&#770;<sub>d</sub>)</th>
                <th class="text-end" data-i18n="col_dev_raw">Deviasi Standar</th>
                <th class="text-end text-success fw-bold" data-i18n="col_eblup_sb">fastsae (&sum; &theta;&#770;<sub>d</sub><sup>sb</sup>)</th>
                <th class="text-end text-success fw-bold" data-i18n="col_dev_sb">Deviasi fastsae</th>
                <th class="text-center" data-i18n="col_status">Status Konsistensi</th>
              </tr>
            </thead>
            <tbody class="font-monospace small">
              <tr>
                <td class="ps-3 font-sans fw-semibold">ds01-linear (Linear Dasar)</td>
                <td class="text-end">842.9743</td>
                <td class="text-end text-muted">842.9206</td>
                <td class="text-end text-danger">-0.0537</td>
                <td class="text-end text-success fw-bold">842.9743</td>
                <td class="text-end text-success fw-bold">0.000000</td>
                <td class="text-center"><span class="badge bg-success-subtle text-success border border-success-subtle">Tepat 100%</span></td>
              </tr>
              <tr>
                <td class="ps-3 font-sans fw-semibold">ds02-rate (Proporsi Terbatas)</td>
                <td class="text-end">8.5002</td>
                <td class="text-end text-muted">8.2895</td>
                <td class="text-end text-danger">-0.2107</td>
                <td class="text-end text-success fw-bold">8.5002</td>
                <td class="text-end text-success fw-bold">0.000000</td>
                <td class="text-center"><span class="badge bg-success-subtle text-success border border-success-subtle">Tepat 100%</span></td>
              </tr>
              <tr>
                <td class="ps-3 font-sans fw-semibold">ds04-nonlinear (Nonlinear Kompleks)</td>
                <td class="text-end">595.6068</td>
                <td class="text-end text-muted">595.6023</td>
                <td class="text-end text-danger">-0.0045</td>
                <td class="text-end text-success fw-bold">595.6068</td>
                <td class="text-end text-success fw-bold">0.000000</td>
                <td class="text-center"><span class="badge bg-success-subtle text-success border border-success-subtle">Tepat 100%</span></td>
              </tr>
              <tr>
                <td class="ps-3 font-sans fw-semibold">ds05-spatial (Spasial SAR)</td>
                <td class="text-end">705.0020</td>
                <td class="text-end text-muted">705.1440</td>
                <td class="text-end text-danger">+0.1421</td>
                <td class="text-end text-success fw-bold">705.0020</td>
                <td class="text-end text-success fw-bold">0.000000</td>
                <td class="text-center"><span class="badge bg-success-subtle text-success border border-success-subtle">Tepat 100%</span></td>
              </tr>
              <tr>
                <td class="ps-3 font-sans fw-semibold">ds07-outliers (Pencilan Ekstrem)</td>
                <td class="text-end">634.1209</td>
                <td class="text-end text-muted">634.1389</td>
                <td class="text-end text-danger">+0.0179</td>
                <td class="text-end text-success fw-bold">634.1209</td>
                <td class="text-end text-success fw-bold">0.000000</td>
                <td class="text-center"><span class="badge bg-success-subtle text-success border border-success-subtle">Tepat 100%</span></td>
              </tr>
              <tr>
                <td class="ps-3 font-sans fw-semibold">ds08-nested (Hierarki Bertingkat)</td>
                <td class="text-end">674.0150</td>
                <td class="text-end text-muted">674.0263</td>
                <td class="text-end text-danger">+0.0113</td>
                <td class="text-end text-success fw-bold">674.0150</td>
                <td class="text-end text-success fw-bold">0.000000</td>
                <td class="text-center"><span class="badge bg-success-subtle text-success border border-success-subtle">Tepat 100%</span></td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </section>

    <!-- Sampling Methodology Section (Moved from Hero) -->
    <section class="mb-5">
      <div class="methodology-box">
        <div class="d-flex align-items-center gap-2 mb-3">
          <i class="fa-solid fa-graduation-cap text-primary fs-5"></i>
          <h3 class="h5 fw-bold mb-0" data-i18n="methodology_title">Metodologi Sampling dan Nilai Populasi (Ground Truth)</h3>
        </div>
        <p class="text-muted small mb-3" data-i18n="methodology_desc">
          Seluruh set data tolok ukur dibangkitkan dari populasi sintetis (~120.000 rumah tangga) menggunakan rancangan <strong>Two-Stage Stratified Cluster Sampling</strong> yang mengadopsi standar Survei Sosial Ekonomi Nasional (Susenas) Badan Pusat Statistik (BPS) Indonesia, dievaluasi terhadap nilai murni <em>Ground Truth</em> populasi.
        </p>

        <div class="row g-3">
          <div class="col-md-3">
            <div class="p-3 rounded bg-body-tertiary border h-100">
              <h6 class="fw-bold mb-1"><span class="badge bg-primary rounded-circle me-1">1</span> <span data-i18n="stage1_title">Tahap 1: Pemilihan Blok Sensus (PSU)</span></h6>
              <p class="text-muted small mb-2" data-i18n="stage1_desc">
                Stratifikasi perkotaan dan perdesaan di setiap kabupaten. Blok Sensus (BS) dipilih secara <em>Probability Proportional to Size (PPS)</em> tanpa pengembalian berdasarkan jumlah rumah tangga muatan.
              </p>
              <div class="font-monospace text-primary small py-1 px-2 bg-body rounded border">
                &pi;<sub>1,dhi</sub> = a<sub>dh</sub> &times; (M<sub>dhi</sub> / &sum; M<sub>dhk</sub>)
              </div>
            </div>
          </div>

          <div class="col-md-3">
            <div class="p-3 rounded bg-body-tertiary border h-100">
              <h6 class="fw-bold mb-1"><span class="badge bg-info rounded-circle me-1">2</span> <span data-i18n="stage2_title">Tahap 2: Pemilihan Rumah Tangga (SSU)</span></h6>
              <p class="text-muted small mb-2" data-i18n="stage2_desc">
                Penarikan sampel sistematik tepat 10 rumah tangga per BS terpilih dengan nomor acak awal (<em>random start</em>). Peluang inklusi bersyarat:
              </p>
              <div class="font-monospace text-info small py-1 px-2 bg-body rounded border">
                &pi;<sub>2|1,dhij</sub> = 10 / M<sub>dhi</sub>
              </div>
            </div>
          </div>

          <div class="col-md-3">
            <div class="p-3 rounded bg-body-tertiary border h-100">
              <h6 class="fw-bold mb-1"><span class="badge bg-success rounded-circle me-1">3</span> <span data-i18n="stage3_title">Tahap 3: Pembobotan dan Varians Taylor</span></h6>
              <p class="text-muted small mb-2" data-i18n="stage3_desc">
                Bobot sampel akhir (FWT) memperhitungkan efek pengelompokan (clustering). Varians penduga langsung dihitung melalui Linearitas Deret Taylor (<code>survey::svydesign</code>), menghasilkan efek desain realistis (<em>Deff &gt; 1</em>).
              </p>
              <div class="font-monospace text-success small py-1 px-2 bg-body rounded border">
                w<sub>dhij</sub> = M<sub>dh</sub> / (10 a<sub>dh</sub>)
              </div>
            </div>
          </div>

          <div class="col-md-3">
            <div class="p-3 rounded bg-body-tertiary border h-100">
              <h6 class="fw-bold mb-1"><span class="badge bg-warning text-dark rounded-circle me-1">4</span> <span data-i18n="stage4_title">Tahap 4: Replikasi Monte Carlo (R=30)</span></h6>
              <p class="text-muted small mb-2" data-i18n="stage4_desc">
                Didukung modul replikasi (<code>engine/run_mc_replications.R</code>) untuk menghitung Empirical MSE, Empirical Bias, dan RelEff dari penarikan sampel berulang secara independen.
              </p>
              <div class="font-monospace text-warning small py-1 px-2 bg-body rounded border">
                MSE<sub>d</sub> = &sum; (&theta;&#770;<sub>d</sub><sup>(r)</sup> - Y<sub>d</sub>)<sup>2</sup> / R
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>

  </main>

  <!-- Footer -->
  <footer class="py-4 border-top text-center text-muted small">
    <div class="container-xl">
      <p class="mb-1"><strong>SAE Benchmark Lab</strong> &bull; <span data-i18n="footer_title">Laboratorium Tolok Ukur Model Small Area Estimation</span></p>
      <p class="mb-0 font-monospace">Generated automatically on {{TIMESTAMP}}</p>
    </div>
  </footer>

  <!-- Embedded Benchmark Data -->
  <script>
    const benchmarkData = {{CHART_JSON}};
  </script>

  <!-- Scripts -->
  <script src="https://code.jquery.com/jquery-3.7.1.min.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"></script>

  <script>
    // Bilingual Dictionary with EYD V & Natural Statistical Nomenclature
    const translations = {
      id: {
        page_title: "SAE Benchmark Lab | Evaluasi Model Small Area Estimation",
        badge_archetypes: "8 Ragam Dataset",
        badge_ground_truth: "Populasi Murni (Ground Truth)",
        badge_profiling: "Profil Waktu & Memori",
        hero_subtitle: "Tolok ukur terstandar untuk mengevaluasi model Small Area Estimation (SAE) terhadap nilai populasi murni (Ground Truth) berdasarkan akurasi, efisiensi relatif, dan efisiensi komputasi.",
        kpi_total_models: "Total Model",
        kpi_total_models_sub: "Standar & Robust",
        kpi_datasets: "Ragam Dataset",
        kpi_datasets_sub: "Tantangan SAE",
        kpi_top_gain: "Efisiensi Tertinggi",
        kpi_fastest: "Model Tercepat",
        kpi_lowest_ram: "Memori Terendah",
        chart_section_title: "Visualisasi Interaktif Trade-Off Model",
        chart_section_subtitle: "Analisis perbandingan antara kecepatan komputasi, presisi estimasi, dan efisiensi relatif model",
        filter_dataset: "Pilih Dataset:",
        opt_all_datasets: "Semua Dataset",
        chart_scatter_title: "Waktu Komputasi vs Efisiensi Relatif (Pareto Frontier)",
        chart_scatter_badge: "Makin ke atas dan kiri makin unggul",
        chart_bar_title: "Perbandingan Efisiensi Relatif (%)",
        chart_bar_badge: "Penduga Langsung (Direct) = 100%",
        table_section_title: "Tabel Evaluasi Model per Dataset",
        table_section_subtitle: "Setiap dataset menguji karakteristik struktur khusus; peringkat dipisahkan dan nilai terbaik ditandai hijau lembut.",
        col_rank: "Peringkat",
        col_model: "Model / Paket",
        col_releff: "Efisiensi Relatif (%)",
        col_rrmse: "RRMSE (%)",
        col_arb: "ARB (%)",
        col_corr: "Korelasi",
        col_ram: "RAM Puncak",
        col_runtime: "Waktu",
        col_outlier_rrmse: "RRMSE Pencilan",
        col_violations: "Pelanggaran Batas (&lt;0 / &gt;1)",
        benchmark_goal: "Tujuan Pengujian:",
        dgp_label: "Struktur DGP:",
        best_legend: "Nilai terbaik ditandai hijau",
        methodology_title: "Metodologi Sampling dan Nilai Populasi (Ground Truth)",
        methodology_desc: "Seluruh set data tolok ukur dibangkitkan dari populasi sintetis (~120.000 rumah tangga) menggunakan rancangan Two-Stage Stratified Cluster Sampling yang mengadopsi standar Survei Sosial Ekonomi Nasional (Susenas) Badan Pusat Statistik (BPS) Indonesia, dievaluasi terhadap nilai murni Ground Truth populasi.",
        stage1_title: "Tahap 1: Pemilihan Blok Sensus (PSU)",
        stage1_desc: "Stratifikasi perkotaan dan perdesaan di setiap kabupaten. Blok Sensus (BS) dipilih secara Probability Proportional to Size (PPS) tanpa pengembalian berdasarkan jumlah rumah tangga muatan.",
        stage2_title: "Tahap 2: Pemilihan Rumah Tangga (SSU)",
        stage2_desc: "Penarikan sampel sistematik tepat 10 rumah tangga per BS terpilih dengan nomor acak awal (random start).",
        stage3_title: "Tahap 3: Pembobotan dan Varians Taylor",
        stage3_desc: "Bobot sampel akhir (FWT) memperhitungkan efek pengelompokan (clustering). Varians penduga langsung dihitung melalui Linearitas Deret Taylor (survey::svydesign) sehingga menghasilkan efek desain yang realistis (Deff > 1).",
        stage4_title: "Tahap 4: Replikasi Monte Carlo (R=30)",
        stage4_desc: "Didukung modul replikasi (engine/run_mc_replications.R) untuk menghitung Empirical MSE, Empirical Bias, dan RelEff dari penarikan sampel berulang secara independen.",
        eval_mode_label: "Mode Evaluasi:",
        mode_single: "Sampel Tunggal",
        mode_mc: "Monte Carlo (R=30)",
        chart_mc_title: "Sebaran Replikasi Monte Carlo (R=30) & Efek Penyusutan (Shrinkage)",
        chart_mc_legend: "Titik Merah = Nilai Murni Populasi (Ground Truth)",
        chart_mc_desc: "Perbandingan sebaran estimasi dari 30 penarikan sampel independen (BPS Two-Stage). Penduga Langsung (abu-abu) menyebar lebar mencerminkan varians sampling yang besar, sedangkan model SAE fastsae (biru & hijau) secara konsisten menyusutkan estimasi ke arah nilai Ground Truth dengan stabilitas tinggi.",
        benchmarking_title: "Validasi Properti Benchmarking (Konsistensi Agregat Wilayah)",
        benchmarking_desc: "Salah satu syarat krusial penerapan SAE di Badan Pusat Statistik (BPS) adalah benchmarking property: jumlah estimasi area kecil (kabupaten/kota) harus tepat sama dengan angka resmi estimasi langsung (Direct) tingkat wilayah atasnya (provinsi/nasional). Tanpa benchmarking, model EBLUP standar menghasilkan deviasi agregat. Fitur self_benchmark = TRUE pada paket fastsae menjamin konsistensi matematis secara langsung dalam estimasi parameter tanpa memerlukan penyesuaian pro-rata post-hoc.",
        status_perfect: "Presisi Tepat (Deviasi = 0,00)",
        col_archetype: "Karakteristik Dataset",
        col_target_direct: "Target Langsung (&sum; Y<sub>d</sub><sup>dir</sup>)",
        col_eblup_raw: "EBLUP Standar (&sum; &theta;&#770;<sub>d</sub>)",
        col_dev_raw: "Deviasi Standar",
        col_eblup_sb: "fastsae (&sum; &theta;&#770;<sub>d</sub><sup>sb</sup>)",
        col_dev_sb: "Deviasi fastsae",
        col_status: "Status Konsistensi",
        footer_title: "Laboratorium Tolok Ukur Model Small Area Estimation"
      },
      en: {
        page_title: "SAE Benchmark Lab | Small Area Estimation Benchmark",
        badge_archetypes: "8 Dataset Archetypes",
        badge_ground_truth: "Exact Ground Truth",
        badge_profiling: "Time & Memory Profiling",
        hero_subtitle: "A standardized testbed evaluating Small Area Estimation (SAE) models against exact finite population Ground Truth with accuracy, relative efficiency, and computational latency metrics.",
        kpi_total_models: "Total Models",
        kpi_total_models_sub: "Standard & Robust",
        kpi_datasets: "Data Archetypes",
        kpi_datasets_sub: "SAE Challenges",
        kpi_top_gain: "Top RelEff Gain",
        kpi_fastest: "Fastest Model",
        kpi_lowest_ram: "Lowest Memory",
        chart_section_title: "Interactive Model Trade-Off Visualizations",
        chart_section_subtitle: "Explore computational time versus accuracy and relative efficiency curves",
        filter_dataset: "Select Dataset:",
        opt_all_datasets: "All Datasets",
        chart_scatter_title: "Runtime vs Relative Efficiency (Pareto Frontier)",
        chart_scatter_badge: "Top-left indicates superior trade-off",
        chart_bar_title: "Relative Efficiency (%) Comparison",
        chart_bar_badge: "Direct Estimator Baseline = 100%",
        table_section_title: "Model Leaderboard by Dataset",
        table_section_subtitle: "Each dataset tests distinct structural challenges; rank is separated and top metric performers are highlighted in soft green.",
        col_rank: "Rank",
        col_model: "Model / Package",
        col_releff: "Rel. Efficiency (%)",
        col_rrmse: "RRMSE (%)",
        col_arb: "ARB (%)",
        col_corr: "Correlation",
        col_ram: "Peak RAM",
        col_runtime: "Runtime",
        col_outlier_rrmse: "Outlier RRMSE",
        col_violations: "Boundary Violations (&lt;0 / &gt;1)",
        benchmark_goal: "Benchmark Goal:",
        dgp_label: "DGP Architecture:",
        best_legend: "Best value highlighted in green",
        methodology_title: "Sampling Methodology & Ground Truth",
        methodology_desc: "All benchmark datasets are constructed from a synthetic finite population (~120,000 households) under Two-Stage Stratified Cluster Sampling mimicking national statistical survey standards (e.g. BPS Susenas).",
        stage1_title: "Stage 1: Primary Sampling Unit (PSU)",
        stage1_desc: "Stratified by Urban/Rural within each district. Census Blocks selected via Probability Proportional to Size (PPS) without replacement based on household counts.",
        stage2_title: "Stage 2: Secondary Sampling Unit (SSU)",
        stage2_desc: "Systematic sampling of exactly 10 households per sampled Census Block with a random start.",
        stage3_title: "Stage 3: Weights & Taylor Linearization",
        stage3_desc: "Design weights calibrated for cluster effects. Direct variance estimated via Taylor Series Linearization (survey::svydesign), capturing realistic clustering (Deff > 1).",
        stage4_title: "Stage 4: Monte Carlo Replications (R=30)",
        stage4_desc: "Backed by replication engine (engine/run_mc_replications.R) computing Empirical MSE, Bias, and RelEff over repeated independent draws.",
        eval_mode_label: "Evaluation Mode:",
        mode_single: "Single Sample",
        mode_mc: "Monte Carlo (R=30)",
        chart_mc_title: "Monte Carlo Replication Distribution (R=30) & Shrinkage Effect",
        chart_mc_legend: "Red Diamond = Exact Population Ground Truth",
        chart_mc_desc: "Comparison of estimation spread across 30 independent survey draws (BPS Two-Stage). Direct survey estimates (grey) exhibit high sampling variance, whereas fastsae models (blue & green) shrink estimates toward Ground Truth with superior stability.",
        benchmarking_title: "Benchmarking Property & Aggregate Consistency Validation",
        benchmarking_desc: "A crucial prerequisite for SAE adoption at National Statistical Offices (e.g. BPS) is the benchmarking property: aggregated small area estimates must equal the official direct survey benchmark total. Unbenchmarked models produce aggregate discrepancies. The self_benchmark = TRUE feature in fastsae enforces mathematical consistency directly during parameter estimation without requiring ad-hoc pro-rata adjustment.",
        status_perfect: "Exact Match (Deviation = 0.00)",
        col_archetype: "Dataset Archetype",
        col_target_direct: "Direct Target (&sum; Y<sub>d</sub><sup>dir</sup>)",
        col_eblup_raw: "Standard EBLUP (&sum; &theta;&#770;<sub>d</sub>)",
        col_dev_raw: "Standard Deviation",
        col_eblup_sb: "fastsae (&sum; &theta;&#770;<sub>d</sub><sup>sb</sup>)",
        col_dev_sb: "fastsae Deviation",
        col_status: "Consistency Status",
        footer_title: "Small Area Estimation Model Benchmark Laboratory"
      }
    };

    let currentLang = localStorage.getItem("sae_lab_lang") || "id";
    let currentTheme = localStorage.getItem("sae_lab_theme") || "light";

    // Set Initial Theme
    function applyTheme(theme) {
      document.documentElement.setAttribute("data-bs-theme", theme);
      const icon = document.getElementById("themeIcon");
      if (theme === "dark") {
        icon.className = "fa-solid fa-sun text-warning";
      } else {
        icon.className = "fa-solid fa-moon";
      }
      localStorage.setItem("sae_lab_theme", theme);
      updateChartsTheme();
    }

    // Toggle Theme
    document.getElementById("themeToggleBtn").addEventListener("click", () => {
      const active = document.documentElement.getAttribute("data-bs-theme");
      applyTheme(active === "dark" ? "light" : "dark");
    });

    // Toggle Evaluation Mode (Single Sample vs Monte Carlo R=30)
    function switchEvalMode(mode) {
      if (mode === \"single\") {
        document.querySelectorAll(\".table-mode-single\").forEach(el => el.classList.remove(\"d-none\"));
        document.querySelectorAll(\".table-mode-mc\").forEach(el => el.classList.add(\"d-none\"));
        const btnS = document.getElementById(\"btnSingleMode\");
        const btnM = document.getElementById(\"btnMCMode\");
        if (btnS && btnM) {
          btnS.className = \"btn btn-primary active\";
          btnM.className = \"btn btn-outline-secondary\";
        }
      } else {
        document.querySelectorAll(\".table-mode-single\").forEach(el => el.classList.add(\"d-none\"));
        document.querySelectorAll(\".table-mode-mc\").forEach(el => el.classList.remove(\"d-none\"));
        const btnS = document.getElementById(\"btnSingleMode\");
        const btnM = document.getElementById(\"btnMCMode\");
        if (btnS && btnM) {
          btnS.className = \"btn btn-outline-secondary\";
          btnM.className = \"btn btn-primary active\";
        }
      }
    }
    window.switchEvalMode = switchEvalMode;

    // Language Toggle
    function applyLanguage(lang) {
      currentLang = lang;
      localStorage.setItem("sae_lab_lang", lang);
      document.getElementById("langText").innerText = lang === "id" ? "EN" : "ID";
      document.title = translations[lang].page_title;

      // Update text nodes with data-i18n
      document.querySelectorAll("[data-i18n]").forEach(el => {
        const key = el.getAttribute("data-i18n");
        if (translations[lang] && translations[lang][key]) {
          el.innerHTML = translations[lang][key];
        }
      });

      // Update elements with data-en and data-id
      document.querySelectorAll("[data-en][data-id]").forEach(el => {
        el.innerHTML = (lang === "id") ? el.getAttribute("data-id") : el.getAttribute("data-en");
      });

      // Update Tab Names
      document.querySelectorAll(".tab-name").forEach(el => {
        el.innerText = (lang === "id") ? el.getAttribute("data-name-id") : el.getAttribute("data-name-en");
      });

      updateCharts();
    }

    document.getElementById("langToggleBtn").addEventListener("click", () => {
      applyLanguage(currentLang === "id" ? "en" : "id");
    });

    // Colors mapping for packages
    const packageColors = {
      "fastsaegpu": "#8b5cf6",
      "fastsae": "#2563eb",
      "tipsae": "#0d9488",
      "hbsae": "#4f46e5",
      "sae": "#0284c7",
      "saeRobust": "#d97706",
      "survey": "#64748b"
    };

    // Chart.js instances
    let scatterChart = null;
    let barChart = null;

    function getChartThemeColors() {
      const isDark = document.documentElement.getAttribute("data-bs-theme") === "dark";
      return {
        gridColor: isDark ? "rgba(255, 255, 255, 0.08)" : "rgba(0, 0, 0, 0.06)",
        textColor: isDark ? "#9ca3af" : "#64748b",
        baselineColor: isDark ? "rgba(239, 68, 68, 0.6)" : "rgba(239, 68, 68, 0.7)"
      };
    }

    function initCharts() {
      const tc = getChartThemeColors();

      // Scatter Chart
      const ctxScatter = document.getElementById("scatterChart").getContext("2d");
      scatterChart = new Chart(ctxScatter, {
        type: "scatter",
        data: { datasets: [] },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          animation: { duration: 400 },
          scales: {
            x: {
              type: "logarithmic",
              title: {
                display: true,
                text: currentLang === "id" ? "Waktu Komputasi (detik, skala log)" : "Runtime (seconds, log scale)",
                color: tc.textColor
              },
              grid: { color: tc.gridColor },
              ticks: {
                color: tc.textColor,
                callback: function(val) {
                  return val + "s";
                }
              }
            },
            y: {
              title: {
                display: true,
                text: currentLang === "id" ? "Efisiensi Relatif (%)" : "Relative Efficiency (%)",
                color: tc.textColor
              },
              grid: { color: tc.gridColor },
              ticks: { color: tc.textColor }
            }
          },
          plugins: {
            legend: {
              position: "top",
              labels: {
                boxWidth: 10,
                color: tc.textColor,
                font: { size: 11 }
              }
            },
            tooltip: {
              callbacks: {
                label: function(ctx) {
                  const pt = ctx.raw;
                  return [
                    pt.model,
                    (currentLang === "id" ? "Dataset: " : "Dataset: ") + pt.dsName,
                    (currentLang === "id" ? "Efisiensi: " : "Rel. Efficiency: ") + pt.y.toFixed(1) + "%",
                    "RRMSE: " + pt.rrmse.toFixed(2) + "%",
                    (currentLang === "id" ? "Waktu: " : "Time: ") + pt.x.toFixed(2) + "s",
                    "RAM: " + pt.ram.toFixed(1) + " MB"
                  ];
                }
              }
            }
          }
        }
      });

      // Bar Chart
      const ctxBar = document.getElementById("barChart").getContext("2d");
      barChart = new Chart(ctxBar, {
        type: "bar",
        data: { labels: [], datasets: [] },
        options: {
          indexAxis: "y",
          responsive: true,
          maintainAspectRatio: false,
          animation: { duration: 400 },
          scales: {
            x: {
              title: {
                display: true,
                text: currentLang === "id" ? "Efisiensi Relatif (%)" : "Relative Efficiency (%)",
                color: tc.textColor
              },
              grid: { color: tc.gridColor },
              ticks: { color: tc.textColor }
            },
            y: {
              grid: { display: false },
              ticks: { color: tc.textColor, font: { size: 10 } }
            }
          },
          plugins: {
            legend: { display: false },
            tooltip: {
              callbacks: {
                label: function(ctx) {
                  return (currentLang === "id" ? "Efisiensi: " : "Efficiency: ") + ctx.raw.toFixed(1) + "%";
                }
              }
            }
          }
        }
      });

      updateCharts();
    }

    function updateCharts() {
      if (!scatterChart || !barChart) return;
      const selectedDs = document.getElementById("chartDatasetSelect").value;
      const tc = getChartThemeColors();

      const filtered = (selectedDs === "ALL")
        ? benchmarkData
        : benchmarkData.filter(d => d.dataset_id === selectedDs);

      const pkgGroups = {};
      filtered.forEach(d => {
        if (!pkgGroups[d.package]) pkgGroups[d.package] = [];
        pkgGroups[d.package].push({
          x: Math.max(0.005, d.Runtime),
          y: d.RelEff,
          model: d.model,
          dsName: (currentLang === "id" ? d.dataset_name_id : d.dataset_name_en),
          rrmse: d.RRMSE,
          ram: d.RAM
        });
      });

      const scatterDatasets = Object.keys(pkgGroups).map(pkg => ({
        label: pkg,
        data: pkgGroups[pkg],
        backgroundColor: packageColors[pkg] || "#64748b",
        borderColor: packageColors[pkg] || "#64748b",
        pointRadius: 6,
        pointHoverRadius: 8
      }));

      scatterChart.data.datasets = scatterDatasets;
      scatterChart.options.scales.x.title.text = currentLang === "id" ? "Waktu Komputasi (detik, skala log)" : "Runtime (seconds, log scale)";
      scatterChart.options.scales.y.title.text = currentLang === "id" ? "Efisiensi Relatif (%)" : "Relative Efficiency (%)";
      scatterChart.update();

      let barItems = [];
      if (selectedDs === "ALL") {
        const modelMap = {};
        filtered.forEach(d => {
          if (!modelMap[d.model]) modelMap[d.model] = { sum: 0, count: 0, pkg: d.package };
          modelMap[d.model].sum += d.RelEff;
          modelMap[d.model].count += 1;
        });
        barItems = Object.keys(modelMap).map(m => ({
          model: m,
          eff: modelMap[m].sum / modelMap[m].count,
          pkg: modelMap[m].pkg
        })).sort((a, b) => b.eff - a.eff);
      } else {
        barItems = filtered.map(d => ({
          model: d.model,
          eff: d.RelEff,
          pkg: d.package
        })).sort((a, b) => b.eff - a.eff);
      }

      barChart.data.labels = barItems.map(d => d.model.replace(/ \\(.+\\)/, ""));
      barChart.data.datasets = [{
        label: "RelEff",
        data: barItems.map(d => d.eff),
        backgroundColor: barItems.map(d => (packageColors[d.pkg] || "#64748b") + "cc"),
        borderColor: barItems.map(d => packageColors[d.pkg] || "#64748b"),
        borderWidth: 1,
        borderRadius: 4
      }];
      barChart.options.scales.x.title.text = currentLang === "id" ? "Efisiensi Relatif (%)" : "Relative Efficiency (%)";
      barChart.update();
    }

    function updateChartsTheme() {
      if (!scatterChart || !barChart) return;
      const tc = getChartThemeColors();
      
      scatterChart.options.scales.x.grid.color = tc.gridColor;
      scatterChart.options.scales.x.ticks.color = tc.textColor;
      scatterChart.options.scales.y.grid.color = tc.gridColor;
      scatterChart.options.scales.y.ticks.color = tc.textColor;
      scatterChart.options.plugins.legend.labels.color = tc.textColor;
      scatterChart.update();

      barChart.options.scales.x.grid.color = tc.gridColor;
      barChart.options.scales.x.ticks.color = tc.textColor;
      barChart.options.scales.y.ticks.color = tc.textColor;
      barChart.update();
    }

    document.getElementById("chartDatasetSelect").addEventListener("change", updateCharts);

    // Initialize Page
    document.addEventListener("DOMContentLoaded", () => {
      applyTheme(currentTheme);
      applyLanguage(currentLang);
      initCharts();
    });
  </script>
</body>
</html>'

  # Substitute template variables
  html <- html_template
  html <- gsub("{{TOTAL_MODELS}}", as.character(total_models), html, fixed = TRUE)
  html <- gsub("{{TOTAL_DATASETS}}", as.character(total_datasets), html, fixed = TRUE)
  html <- gsub("{{TOP_EFF_VAL}}", top_eff_val, html, fixed = TRUE)
  html <- gsub("{{TOP_EFF_MODEL}}", top_eff_model, html, fixed = TRUE)
  html <- gsub("{{TOP_EFF_DS}}", top_eff_ds, html, fixed = TRUE)
  html <- gsub("{{FASTEST_MODEL}}", fastest_model_name, html, fixed = TRUE)
  html <- gsub("{{FASTEST_TIME}}", fastest_model_time, html, fixed = TRUE)
  html <- gsub("{{LOWEST_RAM_MODEL}}", lowest_ram_model, html, fixed = TRUE)
  html <- gsub("{{LOWEST_RAM_VAL}}", lowest_ram_val, html, fixed = TRUE)
  html <- gsub("{{DS_OPTIONS}}", paste(ds_options, collapse = "\n"), html, fixed = TRUE)
  html <- gsub("{{TABS_NAV}}", paste(tabs_nav, collapse = "\n"), html, fixed = TRUE)
  html <- gsub("{{TAB_PANES}}", paste(tab_panes, collapse = "\n"), html, fixed = TRUE)
  html <- gsub("{{TIMESTAMP}}", timestamp_str, html, fixed = TRUE)
  html <- gsub("{{CHART_JSON}}", chart_json_str, html, fixed = TRUE)
  
  # Write output
  docs_index_path <- file.path(docs_dir, "index.html")
  root_index_path <- file.path(root_dir, "index.html")
  
  writeLines(html, docs_index_path)
  writeLines(html, root_index_path)
  
  cat(sprintf("[+] Successfully generated: %s\n", docs_index_path))
  cat(sprintf("[+] Successfully generated: %s\n", root_index_path))
  return(invisible(TRUE))
}

# Run directly if invoked from command line
if (sys.nframe() == 0L) {
  generate_benchmark_dashboard()
}
