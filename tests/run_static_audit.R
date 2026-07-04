# Lightweight repository audit implemented entirely in base R.
# Open Ch2_PfPR_reproducible.Rproj, then run:
# source("tests/run_static_audit.R")

find_repository_root <- function() {
  starts <- getwd()

  source_file <- tryCatch(
    sys.frame(1)$ofile,
    error = function(e) NULL
  )
  if (!is.null(source_file) && nzchar(source_file)) {
    starts <- c(dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)), starts)
  }

  starts <- unique(normalizePath(starts, winslash = "/", mustWork = TRUE))

  for (start in starts) {
    candidate <- start
    repeat {
      is_root <- dir.exists(file.path(candidate, "R")) &&
        dir.exists(file.path(candidate, "scripts")) &&
        file.exists(file.path(candidate, "config", "config.R")) &&
        file.exists(file.path(candidate, "Ch2_PfPR_reproducible.Rproj"))

      if (is_root) {
        return(candidate)
      }

      parent <- dirname(candidate)
      if (identical(parent, candidate)) {
        break
      }
      candidate <- parent
    }
  }

  stop(
    "Could not locate the repository root. Open Ch2_PfPR_reproducible.Rproj and run the audit again.",
    call. = FALSE
  )
}

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

root <- find_repository_root()
r_files <- sort(c(
  list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
  list.files(file.path(root, "scripts"), pattern = "\\.R$", full.names = TRUE)
))

errors <- character()

forbidden <- c(
  "setwd(" = "absolute or implicit working-directory mutation",
  "rep(REF_YEAR" = "old prediction-grid reference-year assignment",
  "_p70_" = "old 70% decision cutoff",
  "Decision3_PfPRgt20_p70" = "old decision output naming",
  "Ntrials = dat$Ntrials" = "R-INLA can expand this expression to NULL; use an evaluated local vector",
  "return.marginals = FALSE" = "obsolete return.marginals entry from control.predictor",
  "mean_pred = stats::weighted.mean(pred_pfpr, examined)" =
    "calibration weights are overwritten inside summarise; use the shared calibration helper",
  ".data$year == year" =
    "ambiguous dplyr data masking; use filter_year_rows() or .env qualification"
)

for (path in r_files) {
  text <- read_text(path)
  relative <- substring(path, nchar(root) + 2L)

  for (token in names(forbidden)) {
    if (grepl(token, text, fixed = TRUE)) {
      errors <- c(
        errors,
        sprintf("%s contains %s: %s", relative, shQuote(token), forbidden[[token]])
      )
    }
  }

  syntax_error <- tryCatch(
    {
      parse(file = path, keep.source = FALSE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(syntax_error)) {
    errors <- c(errors, sprintf("R syntax error in %s: %s", relative, syntax_error))
  }
}

required_scripts <- file.path(
  root,
  "scripts",
  c(
    "00_preflight.R",
    "00_run_all.R",
    "01_prepare_data.R",
    "02_exploratory_outputs.R",
    "03_mesh_and_screening.R",
    "04_fit_final_model.R",
    "05_posterior_outputs.R",
    "06_validation.R",
    "06_render_validation_outputs.R",
    "07_prediction_surfaces.R",
    "08_exceedance_decision.R",
    "09_district_aggregation.R",
    "10_results_summary.R",
    "11_output_audit.R"
  )
)

missing_scripts <- required_scripts[!file.exists(required_scripts)]
if (length(missing_scripts) > 0L) {
  errors <- c(
    errors,
    sprintf(
      "Missing required script: %s",
      substring(missing_scripts, nchar(root) + 2L)
    )
  )
}

config_text <- read_text(file.path(root, "config", "config.R"))
for (token in c("LST_day_lag0", "decision_probability = 0.80")) {
  if (!grepl(token, config_text, fixed = TRUE)) {
    errors <- c(errors, sprintf("Configuration is missing required token: %s", token))
  }
}

data_text <- read_text(file.path(root, "R", "01_data.R"))
if (!grepl("year_anchor", data_text, fixed = TRUE)) {
  errors <- c(errors, "Prediction-year construction does not reference year_anchor")
}

model_text <- read_text(file.path(root, "R", "02_spatial_model.R"))
for (token in c(
  "Ntrials <- as.numeric(dat[[\"Ntrials\"]])",
  "return.marginals.predictor = FALSE"
)) {
  if (!grepl(token, model_text, fixed = TRUE)) {
    errors <- c(errors, sprintf("INLA compatibility code is missing required token: %s", token))
  }
}


validation_text <- read_text(file.path(root, "R", "03_validation.R"))
for (token in c(
  "filter_calibration_rows <- function",
  "summarise_calibration_groups <- function",
  "examined_total = sum(.data$examined"
)) {
  if (!grepl(token, validation_text, fixed = TRUE)) {
    errors <- c(errors, sprintf("Validation compatibility code is missing required token: %s", token))
  }
}

validation_test <- file.path(root, "tests", "test_validation_helpers.R")
if (!file.exists(validation_test)) {
  errors <- c(errors, "Missing calibration regression test: tests/test_validation_helpers.R")
}

prediction_text <- read_text(file.path(root, "R", "04_prediction.R"))
if (!grepl("filter_year_rows <- function", prediction_text, fixed = TRUE)) {
  errors <- c(errors, "Prediction code is missing the explicit year-filter helper.")
}

prediction_test <- file.path(root, "tests", "test_prediction_year_helpers.R")
if (!file.exists(prediction_test)) {
  errors <- c(errors, "Missing prediction-year regression test: tests/test_prediction_year_helpers.R")
}


district_tokens <- c(
  "extract_polygon_means_with_fallback <- function",
  "nearest_valid_cell_fallback",
  "continuous_fallback_distance_km"
)
for (token in district_tokens) {
  if (!grepl(token, prediction_text, fixed = TRUE)) {
    errors <- c(errors, sprintf("District aggregation correction is missing required token: %s", token))
  }
}

district_test <- file.path(root, "tests", "test_district_aggregation_helpers.R")
if (!file.exists(district_test)) {
  errors <- c(errors, "Missing district aggregation regression test: tests/test_district_aggregation_helpers.R")
}

figure_tokens <- c(
  "Figure_3_district_mean_pfpr_by_year.png",
  "Figure_4_spatial_hyperparameter_densities.png",
  "Figure_5_decision_maps_pfpr20_p80.png",
  "Figure_S7_fixed_effect_posterior_densities.png",
  "Figure_S10_spatial_block_cv_scatter_by_fold.png",
  "Figure_S12_spatial_block_cv_calibration_fixed_bins_by_year.png",
  "Figure_S9_internal_calibration_deciles_by_year.png",
  "Figure_S16_latent_spatial_field_mean_and_sd.png",
  "Figure_S23_exceedance_probabilities_all_years_thresholds.png"
)
combined_text <- paste(vapply(r_files, read_text, character(1)), collapse = "
")
for (token in figure_tokens) {
  if (!grepl(token, combined_text, fixed = TRUE)) {
    errors <- c(errors, sprintf("Manuscript figure output is not configured: %s", token))
  }
}

if (file.exists(file.path(root, "scripts", "10_external_map_comparison.R"))) {
  errors <- c(errors, "Obsolete external MAP manuscript script is still present.")
}

screening_script <- read_text(file.path(root, "scripts", "03_mesh_and_screening.R"))
if (grepl("Table_S_candidate_multivariable_model_comparison.csv", screening_script, fixed = TRUE)) {
  errors <- c(errors, "Misleading one-row multivariable comparison table is still generated.")
}
if (!grepl("Table_S_final_model_selection_evidence.csv", screening_script, fixed = TRUE)) {
  errors <- c(errors, "Final-model selection evidence table is not configured.")
}

rmd_files <- list.files(
  root,
  pattern = "\\.[Rr](md|markdown)$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(rmd_files) > 0L) {
  errors <- c(
    errors,
    sprintf(
      "R Markdown file found although the repository is script-based: %s",
      substring(rmd_files, nchar(root) + 2L)
    )
  )
}

if (length(errors) > 0L) {
  cat("STATIC AUDIT FAILED\n")
  cat(paste0("- ", errors, collapse = "\n"), "\n")
  stop("Static audit failed.", call. = FALSE)
}

cat(sprintf("STATIC AUDIT PASSED: %d R files checked\n", length(r_files)))
cat(sprintf("Repository root: %s\n", root))
