# SAE Benchmark Lab (BPS Two-Stage Cluster Edition)

A private, standardized testbed for evaluating and benchmarking **Small Area Estimation (SAE)** models against exact finite population Ground Truth, generated via official **Badan Pusat Statistik (BPS)** two-stage stratified cluster sampling methodology.

---

## 📌 Architecture Overview

```
sae-benchmark-lab/
├── R/                            # 📦 saebenchmark package (sumber utama)
│   ├── metrics.R                 # calculate_sae_metrics()
│   ├── datasets.R                # list_datasets(), default_formula()
│   ├── benchmark.R               # rank_model(), read_leaderboard()
│   └── web.R                     # update_web()
├── datasets/                     # Standardized benchmark datasets (.rds & .csv)
│   ├── datasets_manifest.csv     # Metadata catalog of all 8 datasets
│   ├── ds01_continuous_linear.*  # Baseline Fay-Herriot (log expenditure)
│   ├── ds02_bounded_rate.*       # Poverty / bounded rate in (0, 1)
│   ├── ds03_highdim_sparse.*     # 25 covariates (3 signals, 22 Podes noise)
│   ├── ds04_nonlinear_interaction.* # Sine & interactions (MERF territory)
│   ├── ds05_spatial_correlated.* # Spatial SAR (rho=0.65) with W matrix
│   ├── ds06_spatiotemporal_panel.* # D=50 x T=5 panel with AR(1) persistence
│   ├── ds07_extreme_outliers.*   # Disaster shock / heavy-tailed outliers
│   ├── ds08_nested_subarea.*     # Province -> Kabupaten hierarchy
│   └── W_matrix.rds              # Queen spatial adjacency matrix
├── generator/
│   ├── bps_sampling_engine.R     # BPS two-stage PPS + systematic sampling engine
│   └── make_all_datasets.R       # Battery dataset generator script
├── engine/                       # Legacy runners (tetap jalan, dibungkus R/ bila perlu)
│   ├── metrics.R                 # SAE evaluation metrics (ARB, RRMSE, RelEff, etc.)
│   └── benchmark_runner.R        # Automated model runner & plot generator
├── results/
│   ├── master_leaderboard.csv    # Benchmark leaderboard table (sumber web)
│   └── plots/                    # Automated comparative visualization plots
├── docs/
│   ├── index.html                # Web minimalis (fetch leaderboard.json)
│   └── leaderboard.json          # Data leaderboard per dataset (auto-update)
├── benchmark_my_model.R          # Contoh 3-baris benchmark model sendiri
└── run_test.R                    # Plug-and-play runner script (legacy battery)
```

---

## 🎯 Official BPS Two-Stage Sampling Scheme

All benchmark datasets are constructed from a synthetic finite population of **~120,000 households** nested in **~1,200 Blok Sensus (BS)**, mimicking the official **BPS Susenas** design:

1. **Stage 1 (Blok Sensus Selection)**:
   - Stratified by Urban / Rural (`R105`) within each Kabupaten / Kota.
   - Sampled using **Probability Proportional to Size (PPS)** without replacement, where size is the household count $M_{dhi}$ of the Blok Sensus.
   - Inclusion probability: $\pi_{1,dhi} = a_{dh} \frac{M_{dhi}}{\sum_k M_{dhk}}$.

2. **Stage 2 (Household Selection)**:
   - Exactly $m = 10$ households selected per sampled Blok Sensus using **Systematic Sampling** with a random start.
   - Conditional probability: $\pi_{2|1,dhij} = \frac{10}{M_{dhi}}$.

3. **Survey Weight & Variance Estimation**:
   - Design weight: $w_{dhij} = \frac{1}{\pi_{1,dhi} \times \pi_{2|1,dhij}} = \frac{M_{dh}}{10 a_{dh}}$ (with empirical calibration adjustments matching BPS `FWT`).
   - Direct estimation and sampling variance $\hat{\psi}_d$ computed via **Taylor Series Linearization** using `survey::svydesign` with cluster nesting (`ids = ~bs_id, strata = ~strata_code, weights = ~FWT`).
   - Reflects realistic **Design Effects ($\text{Deff} > 1$)** due to intra-cluster correlation.

---

## 📊 Dataset Battery (8 Structural Archetypes)

| Dataset ID | Target Type | DGP Structure | SAE Challenge Tested |
| :--- | :--- | :--- | :--- |
| `ds01_continuous_linear` | Continuous | Log expenditure, linear covariates, normal $u_d$ | Baseline efficiency gain of EBLUP / HB over Direct |
| `ds02_bounded_rate` | Proportion $\in (0, 1)$ | Household poverty flag from latent logit | Bounded estimation; prevents rates $<0$ or $>1$ |
| `ds03_highdim_sparse` | Continuous | 25 covariates (3 true signals, 22 Podes/remote sensing noise) | Regularization, Horseshoe shrinkage, LASSO vs overfitting |
| `ds04_nonlinear_interaction`| Continuous | $2.2\sin(\pi x_1) + 1.8 x_2^2 - 2.5(x_1 x_3) + 1.4\sqrt{\|x_4\|}$ | Severe functional misspecification; tests MERF/RF-SAE |
| `ds05_spatial_correlated` | Continuous | SAR random effects: $\mathbf{u} = (I - \rho W)^{-1}\boldsymbol{\epsilon}$ ($\rho=0.65$) | Spatial borrowing of strength (SEBLUP, CAR/Besag INLA) |
| `ds06_spatiotemporal_panel`| Panel Continuous | $D=50 \times T=5$ panel with $u_{dt} = \phi u_{d,t-1} + \epsilon_{dt}$ ($\phi=0.7$) | Spatio-temporal models (Rao-Yu, tipsae panel Stan) |
| `ds07_extreme_outliers` | Continuous | 4 disaster shock districts with $\pm 8\sigma$ deviations | Heavy-tailed robust estimation (Student-$t$ HB vs Huber) |
| `ds08_nested_subarea` | Hierarchical | Province ($v_p$) + District ($u_{pd}$) nested random effects | Two-fold subarea borrowing across multiple administrative tiers |

Every dataset contains the **exact finite population Ground Truth (`y_true`)** calculated directly from all population units.

---

## 🚀 How to Test Any Model (Plug-and-Play)

### A. Cara baru (disarankan): 1 fungsi `benchmark_sae()`

Hasil otomatis masuk ke `results/master_leaderboard.csv` + web `docs/` per dataset.

```bash
Rscript benchmark_my_model.R
```

```r
source("R/metrics.R"); source("R/datasets.R")
source("R/benchmark.R"); source("R/web.R")

# 1. Fungsi model: terima (ds, formula_str), kembalikan vektor numerik nrow(ds)
my_model <- function(ds, formula_str) {
  fit <- lm(as.formula(formula_str), data = ds)
  as.numeric(predict(fit, newdata = ds))
}

# 2. Benchmark + masuk leaderboard + refresh web
rank_model(my_model, "mymodel (lm, v1)", dataset_ids = NULL)  # NULL = semua 8 dataset
# rank_model(my_model, "mymodel (lm, v1)", dataset_ids = "ds01")  # 1 dataset saja
# (nama lama benchmark_sae() tetap bisa dipakai sebagai alias)

# 3. Lihat web minimalis
# Buka docs/index.html di browser, atau serve: python3 -m http.server --directory docs 8000
```

Fungsi bantu: `list_datasets()`, `default_formula("ds01_continuous_linear")`,
`read_leaderboard()`, `update_web()` (regenerasi `docs/leaderboard.json` + `index.html`).

### B. Cara lama: full battery via `run_test.R`

```bash
# Run all 8 datasets across standard models (Direct, fastsaegpu HB, MERF, fastsae EBLUP, INLA)
Rscript run_test.R

# Quick check on the first 3 datasets
Rscript run_test.R --quick

# Run only a specific dataset archetype
Rscript run_test.R --dataset=ds04_nonlinear_interaction
```

### 2. Test a Custom New Model Inside R

You can plug any new model into the benchmark runner with just a few lines:

```r
source("/Volumes/work/_MainR/sae-benchmark-lab/engine/benchmark_runner.R")

# Define your custom model wrapper
# Must accept (ds, formula_str) and return numeric vector of area estimates
my_new_model <- function(ds, formula_str) {
  # Fit your model here:
  # fit <- my_model_func(formula = as.formula(formula_str), data = ds, ...)
  # return(fit$estimates)
}

# Run against all 8 datasets in one command
results <- run_benchmark_suite(
  models = list(
    "Direct"     = wrapper_direct,
    "MyNewModel" = my_new_model,
    "Enhanced_MERF" = wrapper_fastsaegpu_merf
  )
)
```

---

## 📈 Standard SAE Evaluation Metrics

The engine automatically computes:
- **ARB (%)**: Absolute Relative Bias $= \frac{1}{D}\sum \left|\frac{\hat{\theta}_d - \theta_d}{\theta_d}\right| \times 100$
- **RRMSE (%)**: Relative Root Mean Squared Error $= \sqrt{\frac{1}{D}\sum \left(\frac{\hat{\theta}_d - \theta_d}{\theta_d}\right)^2} \times 100$
- **RMSE & MAE**: Root Mean Squared Error and Mean Absolute Error
- **Corr**: Pearson correlation with Ground Truth $\theta_d$
- **RelEff (%)**: Relative Efficiency vs Direct $= \frac{\text{MSE}(\text{Direct})}{\text{MSE}(\text{Model})} \times 100$ ($>100\%$ indicates efficiency gain)
- **Regular vs Outlier RRMSE**: Breakdown for contaminated shock benchmarks
- **Boundary Violations**: Count of estimates outside valid domain (e.g. $[0, 1]$ for rates)
- **Runtime (s)**: Wall-clock execution time
