# Run once after extracting the v1.1.0 patch over an existing project.
# This removes obsolete files that a ZIP overlay cannot delete. It does not
# remove fitted model objects, validation objects, prediction rasters or raw data.

find_project_root <- function() {
  candidate <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(candidate, "Ch2_PfPR_reproducible.Rproj")) &&
        dir.exists(file.path(candidate, "scripts")) &&
        dir.exists(file.path(candidate, "R"))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  stop("Open Ch2_PfPR_reproducible.Rproj and run this script again.", call. = FALSE)
}

root <- find_project_root()
obsolete_files <- c(
  file.path(root, "scripts", "10_external_map_comparison.R"),
  file.path(root, "outputs", "tables", "supplementary", "Table_S_candidate_multivariable_model_comparison.csv"),
  file.path(root, "outputs", "tables", "supplementary", "Table_S_candidate_pool_ranked_by_univariate_WAIC.csv"),
  file.path(root, "outputs", "logs", "external_MAP_comparison_status.csv"),
  file.path(root, "outputs", "figures", "supplementary", "Figure_S9_internal_calibration_deciles_overall.png")
)
unlink(obsolete_files[file.exists(obsolete_files)], force = TRUE)

external_dir <- file.path(root, "data", "external_map")
if (dir.exists(external_dir)) unlink(external_dir, recursive = TRUE, force = TRUE)

supp_dir <- file.path(root, "outputs", "figures", "supplementary")
if (dir.exists(supp_dir)) {
  legacy_figures <- list.files(
    supp_dir,
    pattern = "external_MAP|^Figure_S(17|19|21|23|25|27)_model_pfpr|^Figure_S29_exceedance",
    full.names = TRUE
  )
  unlink(legacy_figures, force = TRUE)
}

message("v1.1.0 patch cleanup completed. Saved model and validation objects were retained.")
