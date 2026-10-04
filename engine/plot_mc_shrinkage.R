#!/usr/bin/env Rscript
#' Plot Monte Carlo Distribution & Shrinkage Effect (R = 30 Replications)
#'
#' Demonstrates:
#' 1. High variance of Direct estimator across independent survey draws
#' 2. Variance reduction & shrinkage of SAE models (EBLUP) towards Ground Truth
#' 3. Preservation of precision under Self-Benchmarking
#'
#' @author Antigravity Pair Programmer
#' @date 2026-10-04

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(fastsae)
})

source("/Volumes/work/_MainR/sae-benchmark-lab/generator/bps_sampling_engine.R")

set.seed(101)
pop01 <- generate_synthetic_hierarchy(n_districts = 60, n_provinces = 6, seed = 101)
dist_info01 <- pop01 %>% distinct(district_id, province_id)
dist_info01$x1 <- rnorm(60, 0, 1)
dist_info01$x2 <- runif(60, 0.4, 0.95)
dist_info01$x3 <- runif(60, 0.3, 0.85)
pop01 <- pop01 %>% left_join(dist_info01 %>% dplyr::select(district_id, x1, x2, x3), by = "district_id")
u_d <- rnorm(60, 0, 0.15)
names(u_d) <- 1:60
c_bs <- rnorm(length(unique(pop01$bs_id)), 0, 0.12)
names(c_bs) <- unique(pop01$bs_id)
pop01$u <- u_d[as.character(pop01$district_id)]
pop01$c <- c_bs[pop01$bs_id]
pop01$e <- rnorm(nrow(pop01), 0, 0.35)
pop01$y <- 13.8 + 0.35 * pop01$x1 + 0.25 * pop01$x2 + 0.20 * pop01$x3 + pop01$u + pop01$c + pop01$e

gt <- pop01 %>% group_by(district_id) %>% summarise(y_true = mean(y), .groups = "drop")
y_true <- gt$y_true

bs_catalog <- pop01 %>% distinct(province_id, district_id, stratum_id, strata_code, bs_id, bs_size, stratum_total_hh)
bs_by_strata <- split(bs_catalog, bs_catalog$strata_code)
hh_indices_by_bs <- split(seq_len(nrow(pop01)), pop01$bs_id)

sample_fn <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  sampled_bs_list <- lapply(bs_by_strata, function(st_bs) {
    n_sample_bs <- min(4, nrow(st_bs))
    idx <- sample.int(nrow(st_bs), size = n_sample_bs, prob = st_bs$bs_size)
    sub <- st_bs[idx, ]
    sub$pi_1 <- pmin(1, n_sample_bs * (sub$bs_size / sub$stratum_total_hh[1]))
    sub
  })
  sampled_bs <- do.call(rbind, sampled_bs_list)
  all_hh_idx <- unlist(lapply(seq_len(nrow(sampled_bs)), function(i) {
    b_id <- sampled_bs$bs_id[i]
    hhs <- hh_indices_by_bs[[b_id]]
    m_pop <- length(hhs)
    m_draw <- min(10, m_pop)
    k_step <- m_pop / m_draw
    r_start <- runif(1, 0, k_step)
    draw_pos <- pmin(m_pop, floor(r_start + (0:(m_draw - 1)) * k_step) + 1)
    hhs[draw_pos]
  }))
  samp <- pop01[all_hh_idx, ]
  bs_pi_map <- sampled_bs$pi_1
  names(bs_pi_map) <- sampled_bs$bs_id
  samp$pi_1 <- bs_pi_map[samp$bs_id]
  samp$pi_2 <- 10 / samp$bs_size
  samp$FWT <- (1 / (samp$pi_1 * samp$pi_2)) * exp(rnorm(nrow(samp), 0, 0.04))
  
  bs_agg <- samp %>%
    group_by(district_id, strata_code, bs_id) %>%
    summarise(w_y = sum(FWT * y), w = sum(FWT), .groups = "drop")
  dist_agg <- bs_agg %>%
    group_by(district_id) %>%
    summarise(tot_wy = sum(w_y), tot_w = sum(w), .groups = "drop") %>%
    mutate(y_hat = tot_wy / tot_w)
  bs_agg <- bs_agg %>% left_join(dist_agg %>% dplyr::select(district_id, tot_w, y_hat), by = "district_id")
  bs_agg$z <- (bs_agg$w_y - bs_agg$y_hat * bs_agg$w) / bs_agg$tot_w
  strat_v <- bs_agg %>%
    group_by(district_id, strata_code) %>%
    summarise(var_z = ifelse(n() > 1, sum((z - mean(z))^2) * n() / (n() - 1), 0), .groups = "drop")
  dist_v <- strat_v %>%
    group_by(district_id) %>%
    summarise(psi_dir = pmax(1e-6, sum(var_z)), .groups = "drop")
  res <- dist_agg %>% dplyr::select(district_id, y_dir = y_hat) %>% left_join(dist_v, by = "district_id")
  res
}

R <- 30
selected_districts <- c(6, 17, 28, 43, 56)

rep_records <- list()
for (r in 1:R) {
  samp_r <- sample_fn(seed = 1000 + r)
  ds_r <- samp_r %>% left_join(dist_info01 %>% dplyr::select(district_id, x1, x2, x3), by = "district_id")
  
  # Direct
  dir_sub <- ds_r %>% filter(district_id %in% selected_districts)
  for (i in seq_len(nrow(dir_sub))) {
    rep_records[[length(rep_records) + 1]] <- data.frame(
      rep = r,
      district_id = paste0("Kab. ", dir_sub$district_id[i]),
      model = "Direct (Survey)",
      estimate = dir_sub$y_dir[i],
      y_true = y_true[dir_sub$district_id[i]],
      stringsAsFactors = FALSE
    )
  }
  
  # fastsae EBLUP
  f1 <- fastsae::eblup_fh(y_dir ~ x1 + x2 + x3, vardir = "psi_dir", data = ds_r, method = "REML", print_result = FALSE)
  for (d in selected_districts) {
    rep_records[[length(rep_records) + 1]] <- data.frame(
      rep = r,
      district_id = paste0("Kab. ", d),
      model = "fastsae (EBLUP)",
      estimate = f1$df_eblup$eblup[d],
      y_true = y_true[d],
      stringsAsFactors = FALSE
    )
  }
  
  # fastsae EBLUP SB
  f2 <- fastsae::eblup_fh(y_dir ~ x1 + x2 + x3, vardir = "psi_dir", data = ds_r, method = "REML", self_benchmark = TRUE, print_result = FALSE)
  for (d in selected_districts) {
    rep_records[[length(rep_records) + 1]] <- data.frame(
      rep = r,
      district_id = paste0("Kab. ", d),
      model = "fastsae (Self-Benchmark)",
      estimate = f2$df_eblup$eblup[d],
      y_true = y_true[d],
      stringsAsFactors = FALSE
    )
  }
}

plot_df <- do.call(rbind, rep_records)
plot_df$model <- factor(plot_df$model, levels = c("Direct (Survey)", "fastsae (EBLUP)", "fastsae (Self-Benchmark)"))

# True values dataframe for reference dots / lines
gt_df <- plot_df %>% distinct(district_id, y_true)

p <- ggplot(plot_df, aes(x = district_id, y = estimate, fill = model)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.65, outlier.size = 1.2, alpha = 0.9) +
  geom_point(data = gt_df, aes(x = district_id, y = y_true), 
             inherit.aes = FALSE, color = "#dc2626", shape = 18, size = 4.2) +
  scale_fill_manual(
    values = c(
      "Direct (Survey)" = "#94a3b8",
      "fastsae (EBLUP)" = "#3b82f6",
      "fastsae (Self-Benchmark)" = "#10b981"
    ),
    labels = c(
      "Direct (Survey)",
      "fastsae (EBLUP, REML)",
      "fastsae (Self-Benchmark)"
    )
  ) +
  labs(
    title = "Monte Carlo Replications (R = 30) - Shrinkage & Stabilitas Estimasi",
    subtitle = "Sebaran estimasi across 30 penarikan sampel independen (BPS Two-Stage). Titik merah = Nilai Murni (Ground Truth).",
    x = "Wilayah Sampel (Kabupaten)",
    y = "Estimasi Karakteristik (Y)",
    fill = "Metode Penduga"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1e293b"),
    plot.subtitle = element_text(size = 10, color = "#64748b", margin = margin(b = 12)),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 9.5, color = "#334155"),
    axis.title = element_text(size = 10, face = "bold", color = "#1e293b"),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

p_out1 <- "/Volumes/work/_MainR/sae-benchmark-lab/results/plots/mc_replication_shrinkage.png"
p_out2 <- "/Volumes/work/_MainR/sae-benchmark-lab/docs/plots/mc_replication_shrinkage.png"

ggsave(p_out1, p, width = 8.5, height = 4.8, dpi = 300)
file.copy(p_out1, p_out2, overwrite = TRUE)
cat(sprintf("[SUCCESS] Monte Carlo shrinkage plot saved to:\n  - %s\n  - %s\n", p_out1, p_out2))
