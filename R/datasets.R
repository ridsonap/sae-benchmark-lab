#' Project-root and dataset helpers
#'
#' All paths default to the package project root (where datasets/ lives).
#' Override with explicit directory arguments if needed.

#' @return absolute path to project root
#' @export
find_root <- function() {
  candidates <- c(
    getwd(),
    dirname(getwd()),
    Sys.getenv("SAE_BENCHMARK_ROOT", unset = NA)
  )
  for (cand in candidates) {
    if (!is.na(cand) && dir.exists(file.path(cand, "datasets")) &&
        file.exists(file.path(cand, "datasets", "datasets_manifest.csv"))) {
      return(normalizePath(cand, mustWork = TRUE))
    }
  }
  # walk up from this file location
  this <- tryCatch(normalizePath(dirname(sys.frame(1)$ofile)),
                   error = function(e) NA)
  if (!is.na(this)) {
    d <- this
    for (i in 1:6) {
      if (file.exists(file.path(d, "datasets", "datasets_manifest.csv"))) return(d)
      d <- dirname(d)
    }
  }
  normalizePath(getwd(), mustWork = TRUE)
}

#' @param datasets_dir directory containing datasets_manifest.csv
#' @return manifest data.frame
#' @export
list_datasets <- function(datasets_dir = file.path(find_root(), "datasets")) {
  manifest_path <- file.path(datasets_dir, "datasets_manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("datasets_manifest.csv not found in: ", datasets_dir)
  }
  utils::read.csv(manifest_path, stringsAsFactors = FALSE)
}

#' Default model formula per dataset
#'
#' @param dataset_id e.g. "ds01_continuous_linear"
#' @return formula string using y_dir as response
#' @export
default_formula <- function(dataset_id) {
  switch(dataset_id,
    "ds01_continuous_linear" = "y_dir ~ x1 + x2 + x3",
    "ds02_bounded_rate" = "y_dir ~ x1 + x2",
    "ds03_highdim_sparse" = paste0("y_dir ~ ", paste0("x", 1:25, collapse = " + ")),
    "ds04_nonlinear_interaction" = "y_dir ~ x1 + x2 + x3 + x4",
    "ds05_spatial_correlated" = "y_dir ~ x1 + x2",
    "ds06_spatiotemporal_panel" = "y_dir ~ x1 + x2",
    "ds07_extreme_outliers" = "y_dir ~ x1 + x2",
    "ds08_nested_subarea" = "y_dir ~ x1 + x2",
    "y_dir ~ x1 + x2"
  )
}

resolve_dataset_ids <- function(dataset_ids, manifest) {
  if (is.null(dataset_ids)) return(manifest$id)
  matched <- character()
  for (did in dataset_ids) {
    if (did %in% manifest$id) {
      matched <- c(matched, did)
    } else {
      hits <- manifest$id[grepl(did, manifest$id, fixed = TRUE)]
      if (length(hits) == 0) {
        hits <- manifest$id[grepl(paste0("^", did), manifest$id)]
      }
      matched <- c(matched, hits)
    }
  }
  matched <- unique(matched)
  if (length(matched) == 0) {
    stop("No datasets matching: ", paste(dataset_ids, collapse = ", "))
  }
  matched
}
