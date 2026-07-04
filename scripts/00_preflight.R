# Fast checks before a complete or resumed analysis. This does not fit INLA.
source(file.path("scripts", "_bootstrap.R"))

source(file.path("tests", "run_static_audit.R"))
source(file.path("tests", "test_validation_helpers.R"))
source(file.path("tests", "test_prediction_year_helpers.R"))
source(file.path("tests", "test_district_aggregation_helpers.R"))
source(file.path("tests", "test_figure_manifest.R"))

assert_files_exist(unlist(cfg$files, use.names = FALSE))

obs_header <- readr::read_csv(cfg$files$observations, n_max = 1, show_col_types = FALSE)
grid_header <- readr::read_csv(cfg$files$prediction_grid, n_max = 1, show_col_types = FALSE)
required_obs <- c("site_id", "longitude", "latitude", "pf_pos", "examined", "year_end")
required_grid <- c("longitude", "latitude", "year_anchor", "month_anchor")
missing_obs <- setdiff(required_obs, names(obs_header))
missing_grid <- setdiff(required_grid, names(grid_header))
if (length(missing_obs)) {
  stop("Observation input is missing columns: ", paste(missing_obs, collapse = ", "), call. = FALSE)
}
if (length(missing_grid)) {
  stop("Prediction-grid input is missing columns: ", paste(missing_grid, collapse = ", "), call. = FALSE)
}

districts <- sf::st_read(cfg$files$districts, quiet = TRUE)
if (nrow(districts) != 161L) {
  stop("The district boundary contains ", nrow(districts), " features; expected 161.", call. = FALSE)
}

message("PREFLIGHT PASSED: inputs, packages, R syntax, helper tests and the 161-district boundary are valid.")
