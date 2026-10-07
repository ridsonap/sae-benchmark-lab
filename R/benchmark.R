#' Rank one SAE model across one or more datasets
#'
#' This is the single entry point of the package. Give it any model function
#' with signature `function(ds, formula_str)` returning a numeric vector of
#' area estimates (length = nrow(ds)). Results are measured (runtime + RAM),
#' scored against ground truth, merged into
#' `results/master_leaderboard.csv`, and optionally pushed to the web
#' leaderboard (`docs/leaderboard.json` + timestamp refresh).
#'
#' `benchmark_sae()` remains available as an alias for backward compatibility.
#'
#' @param model_fn function(ds, formula_str, ...) -> numeric vector
#' @param model_name display name, e.g. "mymodel (my_fn, v1)". Used as
#'   leaderboard key together with dataset_id.
#' @param dataset_ids NULL (all) or character vector, e.g. "ds01_continuous_linear"
#'   or partial "ds01". Passed through fuzzy matching.
#' @param formulas optional named list overriding default formulas per dataset
#' @param datasets_dir path to datasets/
#' @param output_dir path to results/
#' @param update_web logical, refresh docs/leaderboard.json after run?
#' @param verbose logical
#' @return data.frame with one row per dataset (the new model's scores)
#' @export
#' @examples
#' \dontrun{
#' my_model <- function(ds, formula_str) {
#'   fit <- lm(as.formula(formula_str), data = ds)
#'   as.numeric(predict(fit, newdata = ds))
#' }
#' rank_model(my_model, "demo (lm, v1)", dataset_ids = "ds01_continuous_linear")
#' }
rank_model <- function(model_fn,
                          model_name,
                          dataset_ids = NULL,
                          formulas = NULL,
                          datasets_dir = file.path(find_root(), "datasets"),
                          output_dir = file.path(find_root(), "results"),
                          update_web = TRUE,
                          verbose = TRUE) {
  if (!is.function(model_fn)) stop("model_fn must be a function(ds, formula_str)")
  if (missing(model_name) || !nzchar(model_name)) stop("model_name must be a non-empty string")

  manifest <- list_datasets(datasets_dir)
  ids <- resolve_dataset_ids(dataset_ids, manifest)
  manifest <- manifest[match(ids, manifest$id), , drop = FALSE]

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  log <- function(...) if (isTRUE(verbose)) cat(sprintf(...))

  log("== SAE benchmark: '%s' on %d dataset(s) ==\n", model_name, nrow(manifest))

  rows <- vector("list", nrow(manifest))

  for (i in seq_len(nrow(manifest))) {
    ds_id <- manifest$id[i]
    ds_path <- file.path(datasets_dir, paste0(ds_id, ".rds"))
    if (!file.exists(ds_path)) {
      warning("Dataset file missing, skipped: ", ds_path)
      next
    }
    ds <- readRDS(ds_path)
    is_rate <- identical(manifest$response_type[i], "bounded_rate")
    is_out <- if ("is_outlier" %in% names(ds)) ds$is_outlier else NULL
    formula_str <- if (!is.null(formulas) && !is.null(formulas[[ds_id]])) {
      formulas[[ds_id]]
    } else {
      default_formula(ds_id)
    }

    log("[%d/%d] %s ... ", i, nrow(manifest), ds_id)
    gc(reset = TRUE, verbose = FALSE)
    mem_before <- sum(gc(verbose = FALSE)[, 2])
    t0 <- Sys.time()
    pred <- tryCatch(
      model_fn(ds, formula_str),
      error = function(e) {
        log("FAILED: %s\n", conditionMessage(e))
        rep(NA_real_, nrow(ds))
      }
    )
    t1 <- Sys.time()
    mem_after <- tryCatch(sum(gc(verbose = FALSE)[, 2]),
                          error = function(e) mem_before)
    runtime_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    peak_mb <- max(0, round(mem_after - mem_before, 2))

    # length safety: recycle/truncate guard
    if (length(pred) != nrow(ds)) {
      warning(sprintf("%s: model returned %d values, expected %d. Coerced with NA padding.",
                      ds_id, length(pred), nrow(ds)))
      tmp <- rep(NA_real_, nrow(ds))
      n_copy <- min(length(pred), nrow(ds))
      tmp[seq_len(n_copy)] <- as.numeric(pred)[seq_len(n_copy)]
      pred <- tmp
    }

    met <- calculate_sae_metrics(
      pred = pred,
      truth = ds$y_true,
      direct = ds$y_dir,
      is_rate = is_rate,
      is_outlier = is_out,
      runtime_sec = runtime_sec,
      memory_mb = peak_mb
    )

    log("RRMSE %.2f%% | ARB %.2f%% | Eff %.1f%% | %.2fs\n",
        met$RRMSE_pct, met$ARB_pct, met$RelEff_pct, met$Runtime_sec)

    rows[[i]] <- cbind(
      data.frame(
        dataset_id = ds_id,
        dataset_name = manifest$name[i],
        model = model_name,
        stringsAsFactors = FALSE
      ),
      met
    )
  }

  new_df <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(new_df) || nrow(new_df) == 0) stop("No benchmark rows produced.")

  merged <- merge_leaderboard(new_df, output_dir)
  log("Saved %d row(s) to %s\n", nrow(new_df),
      file.path(output_dir, "master_leaderboard.csv"))

  if (isTRUE(update_web)) {
    tryCatch(
      update_web(output_dir = output_dir, root = find_root(), verbose = verbose),
      error = function(e) warning("Web refresh failed: ", conditionMessage(e))
    )
  }

  invisible(merged[merged$model == model_name, , drop = FALSE])
}

#' Alias of [rank_model()] for backward compatibility
#' @export
benchmark_sae <- rank_model

#' Read current leaderboard
#' @param output_dir path to results/
#' @export
read_leaderboard <- function(output_dir = file.path(find_root(), "results")) {
  p <- file.path(output_dir, "master_leaderboard.csv")
  if (!file.exists(p)) stop("Leaderboard not found: ", p)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

merge_leaderboard <- function(new_df, output_dir) {
  lb_csv <- file.path(output_dir, "master_leaderboard.csv")
  lb_rds <- file.path(output_dir, "master_leaderboard.rds")

  if (file.exists(lb_csv)) {
    existing <- utils::read.csv(lb_csv, stringsAsFactors = FALSE)
    # unify columns (old files may carry extra web columns)
    for (cl in setdiff(names(new_df), names(existing))) existing[[cl]] <- NA
    for (cl in setdiff(names(existing), names(new_df))) new_df[[cl]] <- NA
    new_df <- new_df[, names(existing), drop = FALSE]
    exist_keys <- paste(existing$dataset_id, existing$model, sep = "___")
    new_keys <- paste(new_df$dataset_id, new_df$model, sep = "___")
    keep <- existing[!(exist_keys %in% new_keys), , drop = FALSE]
    merged <- rbind(keep, new_df)
  } else {
    merged <- new_df
  }

  merged <- merged[order(merged$dataset_id, -merged$RelEff_pct, merged$RRMSE_pct), , drop = FALSE]
  utils::write.csv(merged, lb_csv, row.names = FALSE)
  saveRDS(merged, lb_rds)
  merged
}
