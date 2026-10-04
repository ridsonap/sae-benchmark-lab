#' BPS Two-Stage Stratified Cluster Sampling Engine for SAE Benchmarks
#'
#' Mimics the official Badan Pusat Statistik (BPS) Susenas sampling methodology:
#' - Stage 1: Stratified selection of Blok Sensus (BS) with PPS (Size = Number of Households)
#' - Stage 2: Systematic selection of fixed households (default 10 households per BS)
#' - Base design weights calculated from joint inclusion probabilities
#' - Area-level direct estimates and sampling variances via Taylor Series Linearization (survey package)
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(Matrix)
})

#' Generate Synthetic Population Hierarchy
#'
#' Creates a hierarchical finite population: Province -> District -> Stratum (Urban/Rural) -> Blok Sensus -> Households.
#'
#' @param n_districts Integer, number of districts (Kabupaten/Kota), default 60.
#' @param n_provinces Integer, number of provinces, default 6 (10 districts per province).
#' @param bs_per_stratum_min Minimum Blok Sensus per stratum, default 25.
#' @param bs_per_stratum_max Maximum Blok Sensus per stratum, default 40.
#' @param hh_per_bs_min Minimum households per Blok Sensus, default 80.
#' @param hh_per_bs_max Maximum households per Blok Sensus, default 150.
#' @param seed Integer random seed.
#' @return A data.frame representing the finite population frame.
generate_synthetic_hierarchy <- function(n_districts = 60,
                                         n_provinces = 6,
                                         bs_per_stratum_min = 25,
                                         bs_per_stratum_max = 40,
                                         hh_per_bs_min = 80,
                                         hh_per_bs_max = 150,
                                         seed = 42) {
  if (!is.null(seed)) set.seed(seed)
  
  districts_per_prov <- ceiling(n_districts / n_provinces)
  prov_ids <- rep(1:n_provinces, each = districts_per_prov)[1:n_districts]
  
  bs_list <- list()
  bs_counter <- 1
  
  for (d in 1:n_districts) {
    p <- prov_ids[d]
    for (s in 1:2) { # 1 = Urban (Perkotaan), 2 = Rural (Perdesaan)
      n_bs_stratum <- sample(bs_per_stratum_min:bs_per_stratum_max, 1)
      m_sizes <- sample(hh_per_bs_min:hh_per_bs_max, n_bs_stratum, replace = TRUE)
      
      for (i in 1:n_bs_stratum) {
        bs_id <- sprintf("BS_%02d_%d_%04d", d, s, bs_counter)
        bs_list[[length(bs_list) + 1]] <- data.frame(
          province_id = p,
          province_name = sprintf("Provinsi %02d", p),
          district_id = d,
          district_name = sprintf("Kabupaten %02d", d),
          stratum_id = s,
          stratum_label = ifelse(s == 1, "Urban", "Rural"),
          strata_code = sprintf("STR_%02d_%d", d, s),
          bs_id = bs_id,
          bs_size = m_sizes[i],
          stringsAsFactors = FALSE
        )
        bs_counter <- bs_counter + 1
      }
    }
  }
  
  bs_frame <- do.call(rbind, bs_list)
  
  # Calculate stratum total households for PPS base
  stratum_totals <- bs_frame %>%
    group_by(strata_code) %>%
    summarise(stratum_total_hh = sum(bs_size), .groups = "drop")
  
  bs_frame <- bs_frame %>%
    left_join(stratum_totals, by = "strata_code")
  
  # Expand to household level
  hh_records <- vector("list", nrow(bs_frame))
  for (r in seq_len(nrow(bs_frame))) {
    row_info <- bs_frame[r, ]
    m <- row_info$bs_size
    hh_records[[r]] <- data.frame(
      hh_id = sprintf("%s_HH%03d", row_info$bs_id, seq_len(m)),
      hh_nurt = seq_len(m),
      province_id = row_info$province_id,
      province_name = row_info$province_name,
      district_id = row_info$district_id,
      district_name = row_info$district_name,
      stratum_id = row_info$stratum_id,
      stratum_label = row_info$stratum_label,
      strata_code = row_info$strata_code,
      bs_id = row_info$bs_id,
      bs_size = m,
      stratum_total_hh = row_info$stratum_total_hh,
      stringsAsFactors = FALSE
    )
  }
  
  pop_df <- do.call(rbind, hh_records)
  return(pop_df)
}

#' Sample Households Using BPS Two-Stage Stratified Cluster Sampling
#'
#' Stage 1: Select Blok Sensus with PPS (Probability Proportional to Size = bs_size).
#' Stage 2: Select m = 10 households systematically from each selected BS.
#'
#' @param pop_df Finite population data.frame from generate_synthetic_hierarchy().
#' @param bs_sample_urban Number of Blok Sensus to draw in urban stratum per district (default 4).
#' @param bs_sample_rural Number of Blok Sensus to draw in rural stratum per district (default 4).
#' @param m_hh_per_bs Households sampled per Blok Sensus (default 10, official BPS Susenas design).
#' @param seed Random seed for sampling.
#' @return Sample data.frame containing drawn households with sampling weights `FWT`.
sample_two_stage_bps <- function(pop_df,
                                 bs_sample_urban = 4,
                                 bs_sample_rural = 4,
                                 m_hh_per_bs = 10,
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Extract distinct Blok Sensus
  bs_catalog <- pop_df %>%
    distinct(province_id, district_id, stratum_id, strata_code, bs_id, bs_size, stratum_total_hh)
  
  sampled_bs_list <- list()
  
  strata_codes <- unique(bs_catalog$strata_code)
  for (st in strata_codes) {
    st_bs <- bs_catalog[bs_catalog$strata_code == st, ]
    s_id <- st_bs$stratum_id[1]
    n_sample_bs <- if (s_id == 1) bs_sample_urban else bs_sample_rural
    
    # Cap if strata has fewer BS than sample requested
    n_sample_bs <- min(n_sample_bs, nrow(st_bs))
    
    # Stage 1: PPS selection without replacement
    # Inclusion probability pi_1i = n_sample_bs * (M_i / M_stratum)
    pi_1 <- pmin(1, n_sample_bs * (st_bs$bs_size / st_bs$stratum_total_hh[1]))
    
    sampled_idx <- sample(seq_len(nrow(st_bs)), size = n_sample_bs, replace = FALSE, prob = st_bs$bs_size)
    sampled_st_bs <- st_bs[sampled_idx, ]
    sampled_st_bs$pi_1 <- pi_1[sampled_idx]
    
    sampled_bs_list[[length(sampled_bs_list) + 1]] <- sampled_st_bs
  }
  
  sampled_bs_df <- do.call(rbind, sampled_bs_list)
  
  # Stage 2: Systematic sampling of m households per BS
  sampled_hh_list <- list()
  
  for (b in seq_len(nrow(sampled_bs_df))) {
    cur_bs <- sampled_bs_df[b, ]
    bs_hhs <- pop_df[pop_df$bs_id == cur_bs$bs_id, ]
    m_pop <- nrow(bs_hhs)
    m_draw <- min(m_hh_per_bs, m_pop)
    
    # Systematic sampling with random start
    k_step <- m_pop / m_draw
    r_start <- runif(1, 0, k_step)
    draw_idx <- pmin(m_pop, floor(r_start + (0:(m_draw - 1)) * k_step) + 1)
    
    drawn_hhs <- bs_hhs[draw_idx, ]
    
    # Conditional probability Stage 2: pi_2|1 = m_draw / m_pop
    pi_2_cond <- m_draw / m_pop
    
    # Joint inclusion probability: pi = pi_1 * pi_2|1
    pi_joint <- cur_bs$pi_1 * pi_2_cond
    
    # Base weight = 1 / pi_joint
    base_weight <- 1 / pi_joint
    
    # Introduce subtle realistic non-response / calibration variation (~5% CV)
    calib_factor <- exp(rnorm(m_draw, mean = 0, sd = 0.04))
    drawn_hhs$FWT <- base_weight * calib_factor
    drawn_hhs$base_weight <- base_weight
    drawn_hhs$pi_joint <- pi_joint
    
    sampled_hh_list[[length(sampled_hh_list) + 1]] <- drawn_hhs
  }
  
  sample_df <- do.call(rbind, sampled_hh_list)
  return(sample_df)
}

#' Compute Direct Estimates and Complex Sampling Variances via Taylor Linearization
#'
#' Uses the R survey package with the exact two-stage cluster specification:
#' svydesign(ids = ~bs_id, strata = ~strata_code, weights = ~FWT, nest = TRUE)
#'
#' @param sample_df Sample data.frame containing drawn households with response variable.
#' @param pop_df Full finite population data.frame (to extract exact Ground Truth).
#' @param response_col Character name of target response column (e.g. "y" or "y_binary").
#' @param is_rate Logical, whether the target is a rate/proportion in (0, 1).
#' @return Area-level data.frame with direct estimates, Taylor variances, design effects, and true parameters.
compute_bps_direct_and_ground_truth <- function(sample_df,
                                                pop_df,
                                                response_col = "y",
                                                is_rate = FALSE) {
  # 1. Exact Finite Population Ground Truth
  ground_truth <- pop_df %>%
    group_by(district_id) %>%
    summarise(
      province_id = first(province_id),
      province_name = first(province_name),
      district_name = first(district_name),
      N_pop = n(),
      y_true = mean(.data[[response_col]]),
      sd_true = sd(.data[[response_col]]),
      var_true = var(.data[[response_col]]),
      .groups = "drop"
    )
  
  # 2. Survey Complex Design Specification
  des <- survey::svydesign(
    ids = ~bs_id,
    strata = ~strata_code,
    weights = ~FWT,
    data = sample_df,
    nest = TRUE
  )
  
  # 3. Direct Estimation via Taylor Series Linearization
  formula_y <- as.formula(paste0("~", response_col))
  svy_res <- survey::svyby(
    formula_y,
    ~district_id,
    des,
    survey::svymean,
    vartype = c("se", "var"),
    deff = TRUE
  )
  
  # Clean up column names from svyby
  col_y <- response_col
  col_se <- "se"
  col_var <- "var"
  col_deff <- paste0("DEff.", response_col)
  
  var_vals <- svy_res[[col_var]]
  y_vals <- svy_res[[col_y]]
  if (is_rate) {
    var_vals <- pmax(1e-4, var_vals)
    y_vals <- pmax(0.005, pmin(0.995, y_vals))
  } else {
    var_vals <- pmax(1e-6, var_vals)
  }
  
  direct_df <- data.frame(
    district_id = svy_res$district_id,
    y_dir = y_vals,
    se_dir = sqrt(var_vals),
    psi_dir = var_vals,
    deff_dir = if (col_deff %in% names(svy_res)) svy_res[[col_deff]] else NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Sample sizes per district
  sample_counts <- sample_df %>%
    group_by(district_id) %>%
    summarise(
      n_sample_hh = n(),
      n_sample_bs = n_distinct(bs_id),
      .groups = "drop"
    )
  
  # Merge Ground Truth, Direct Estimates, and Sample Statistics
  area_df <- ground_truth %>%
    left_join(direct_df, by = "district_id") %>%
    left_join(sample_counts, by = "district_id") %>%
    mutate(
      cv_dir = se_dir / y_dir,
      direct_error = y_dir - y_true,
      direct_abs_pct_error = abs(y_dir - y_true) / abs(y_true) * 100
    )
  
  # If target is a bounded rate, ensure proper bounding checks
  if (is_rate) {
    area_df$y_dir_bounded <- pmax(0.005, pmin(0.995, area_df$y_dir))
  }
  
  return(area_df)
}
