source(file.path("scripts", "_bootstrap.R"))

prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
screening <- read_rds_required(file.path(cfg$dirs$model, "03_covariate_screening.rds"))

# Refresh transparent missingness and model-selection documentation, including
# projects that were prepared under an earlier repository version.
write_csv_checked(
  missingness_table_for_output(prep, cfg),
  file.path(cfg$dirs$tables_supp, "Table_S_missing_covariate_values.csv")
)
write_csv_checked(
  final_model_selection_evidence(screening, cfg),
  file.path(cfg$dirs$tables_supp, "Table_S_final_model_selection_evidence.csv")
)

fit_info_path <- file.path(cfg$dirs$model, "final_model_information_criteria.csv")
if (file.exists(fit_info_path)) {
  fit_info <- readr::read_csv(fit_info_path, show_col_types = FALSE)
  write_csv_checked(
    fit_info,
    file.path(cfg$dirs$tables_supp, "Table_S_final_model_information_criteria.csv")
  )
}

district_all <- readr::read_csv(
  file.path(cfg$dirs$district, "district_results_all_by_year.csv"),
  show_col_types = FALSE
)

district_summary <- district_all |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    n_districts = dplyr::n(),
    n_area_weighted_exact = sum(continuous_aggregation_method == "area_weighted_exact"),
    n_nearest_cell_fallback = sum(continuous_aggregation_method == "nearest_valid_cell_fallback"),
    mean_district_pfpr = mean(district_pfpr_mean),
    median_district_pfpr = stats::median(district_pfpr_mean),
    q1_district_pfpr = stats::quantile(district_pfpr_mean, 0.25, names = FALSE),
    q3_district_pfpr = stats::quantile(district_pfpr_mean, 0.75, names = FALSE),
    minimum_district_pfpr = min(district_pfpr_mean),
    maximum_district_pfpr = max(district_pfpr_mean),
    districts_ge_20pct = sum(district_pfpr_mean >= cfg$decision_threshold),
    districts_ge_20pct_pct = 100 * mean(district_pfpr_mean >= cfg$decision_threshold),
    .groups = "drop"
  )
write_csv_checked(
  district_summary,
  file.path(cfg$dirs$tables_main, "District_summary_by_year.csv")
)

district_extremes <- district_all |>
  dplyr::group_by(year) |>
  dplyr::arrange(district_pfpr_mean, .by_group = TRUE) |>
  dplyr::summarise(
    lowest_district = dplyr::first(District),
    lowest_province = dplyr::first(Province),
    lowest_pfpr = dplyr::first(district_pfpr_mean),
    highest_district = dplyr::last(District),
    highest_province = dplyr::last(Province),
    highest_pfpr = dplyr::last(district_pfpr_mean),
    .groups = "drop"
  )
write_csv_checked(
  district_extremes,
  file.path(cfg$dirs$tables_main, "District_extremes_by_year.csv")
)

observed <- readr::read_csv(
  file.path(cfg$dirs$tables_main, "observed_data_summary.csv"),
  show_col_types = FALSE
)
validation <- readr::read_csv(
  file.path(cfg$dirs$tables_main, "Table_2_validation_comparison.csv"),
  show_col_types = FALSE
)
decision <- readr::read_csv(
  file.path(cfg$dirs$tables_main, "Table_3_decision_summary_pfpr20_p80.csv"),
  show_col_types = FALSE
)
exceedance <- readr::read_csv(
  file.path(cfg$dirs$rasters, "exceedance_raster_index.csv"),
  show_col_types = FALSE
) |>
  dplyr::filter(abs(threshold - cfg$decision_threshold) < 1e-8)

key_results <- dplyr::bind_rows(
  tibble::tibble(
    section = "Observed data",
    year = NA_integer_,
    metric = c("clusters", "examined", "positive", "aggregated_observed_pfpr"),
    value = c(observed$clusters, observed$examined, observed$positive, observed$aggregated_observed_pfpr),
    source_file = "observed_data_summary.csv"
  ),
  validation |>
    tidyr::pivot_longer(c(r, RMSE, MAE, Bias), names_to = "metric", values_to = "value") |>
    dplyr::transmute(
      section = paste0("Validation: ", .data$validation),
      year = NA_integer_, metric, value,
      source_file = "Table_2_validation_comparison.csv"
    ),
  decision |>
    tidyr::pivot_longer(
      c(Yes_pct, Uncertain_pct, No_pct, mean_exceedance_probability),
      names_to = "metric", values_to = "value"
    ) |>
    dplyr::transmute(
      section = "Decision mapping",
      year = as.integer(year), metric, value,
      source_file = "Table_3_decision_summary_pfpr20_p80.csv"
    ),
  exceedance |>
    dplyr::select(year, mean_exceedance_probability, proportion_probability_ge_0_5) |>
    tidyr::pivot_longer(-year, names_to = "metric", values_to = "value") |>
    dplyr::transmute(
      section = "Exceedance at 20% PfPR",
      year = as.integer(year), metric, value,
      source_file = "exceedance_raster_index.csv"
    ),
  district_summary |>
    dplyr::select(
      year, median_district_pfpr, q1_district_pfpr, q3_district_pfpr,
      districts_ge_20pct, districts_ge_20pct_pct, n_nearest_cell_fallback
    ) |>
    tidyr::pivot_longer(-year, names_to = "metric", values_to = "value") |>
    dplyr::transmute(
      section = "District aggregation",
      year = as.integer(year), metric, value,
      source_file = "District_summary_by_year.csv"
    )
)
write_csv_checked(
  key_results,
  file.path(cfg$dirs$tables_main, "Key_results_for_manuscript.csv")
)

# Remove misleading or unreproducible legacy deliverables from prior versions.
unlink(c(
  file.path(cfg$dirs$tables_supp, "Table_S_candidate_multivariable_model_comparison.csv"),
  file.path(cfg$dirs$tables_supp, "Table_S_candidate_pool_ranked_by_univariate_WAIC.csv"),
  file.path(cfg$dirs$logs, "external_MAP_comparison_status.csv"),
  list.files(cfg$dirs$figures_supp, pattern = "external_MAP", full.names = TRUE)
), force = TRUE)

message("Manuscript-ready model-selection, missingness, district and key-result tables written.")
