source(file.path("scripts", "_bootstrap.R"))

figure_manifest <- manuscript_figure_manifest(cfg) |>
  dplyr::mutate(
    exists = file.exists(.data$path),
    bytes = ifelse(.data$exists, file.info(.data$path)$size, NA_real_),
    valid_file = .data$exists & is.finite(.data$bytes) & .data$bytes > 1000,
    status = ifelse(.data$valid_file, "created", "missing")
  )
write_csv_checked(
  figure_manifest |>
    dplyr::select(figure, section, description, relative_path, required, status, bytes),
  file.path(cfg$dirs$logs, "manuscript_figure_audit.csv")
)

mandatory_nonfigure <- c(
  file.path(cfg$dirs$tables_main, "Table_1_fixed_effect_posterior_summaries.csv"),
  file.path(cfg$dirs$tables_main, "Table_2_validation_comparison.csv"),
  file.path(cfg$dirs$tables_main, "Table_3_decision_summary_pfpr20_p80.csv"),
  file.path(cfg$dirs$tables_main, "District_summary_by_year.csv"),
  file.path(cfg$dirs$tables_main, "Key_results_for_manuscript.csv"),
  file.path(cfg$dirs$tables_supp, "Table_S_final_model_selection_evidence.csv"),
  file.path(cfg$dirs$tables_supp, "Table_S_missing_covariate_values.csv"),
  file.path(cfg$dirs$district, "district_results_all_by_year.csv"),
  file.path(cfg$dirs$district, "district_results_all_by_year.gpkg"),
  file.path(cfg$dirs$district, "district_aggregation_audit.csv")
)
nonfigure_audit <- tibble::tibble(file = mandatory_nonfigure) |>
  dplyr::mutate(
    relative_path = substring(.data$file, nchar(cfg$root) + 2L),
    required = TRUE,
    exists = file.exists(.data$file),
    bytes = ifelse(.data$exists, file.info(.data$file)$size, NA_real_),
    valid_file = .data$exists & is.finite(.data$bytes) & .data$bytes > 0
  )
write_csv_checked(nonfigure_audit, file.path(cfg$dirs$logs, "mandatory_nonfigure_output_audit.csv"))

missing_figures <- figure_manifest |>
  dplyr::filter(.data$required, !.data$valid_file)
missing_nonfigure <- nonfigure_audit |>
  dplyr::filter(.data$required, !.data$valid_file)

if (nrow(missing_figures) || nrow(missing_nonfigure)) {
  details <- c(
    paste0(missing_figures$figure, ": ", missing_figures$relative_path),
    missing_nonfigure$relative_path
  )
  stop(
    "Mandatory manuscript outputs are missing or empty:\n",
    paste(details, collapse = "\n"),
    call. = FALSE
  )
}

# Content-level district audit: 161 rows and complete numerical results in each year.
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
expected_districts <- nrow(prep$dist_map)
if (expected_districts != 161L) {
  stop("The supplied administrative boundary does not contain the expected 161 districts.", call. = FALSE)
}
district <- readr::read_csv(
  file.path(cfg$dirs$district, "district_results_all_by_year.csv"),
  show_col_types = FALSE
)
required_numeric <- c(
  "district_pfpr_mean", "mean_cellwise_lower_95", "mean_cellwise_upper_95",
  "mean_exceedance_probability", "area_prop_yes", "area_prop_uncertain", "area_prop_no"
)
district_missing_matrix <- as.matrix(district[, required_numeric, drop = FALSE])
district$.missing_numeric_values <- rowSums(!is.finite(district_missing_matrix))

district <- district |>
  dplyr::mutate(
    .decision_area_total = area_prop_yes + area_prop_uncertain + area_prop_no,
    .invalid_probability =
      district_pfpr_mean < 0 | district_pfpr_mean > 1 |
      mean_cellwise_lower_95 < 0 | mean_cellwise_lower_95 > 1 |
      mean_cellwise_upper_95 < 0 | mean_cellwise_upper_95 > 1 |
      mean_exceedance_probability < 0 | mean_exceedance_probability > 1 |
      area_prop_yes < 0 | area_prop_yes > 1 |
      area_prop_uncertain < 0 | area_prop_uncertain > 1 |
      area_prop_no < 0 | area_prop_no > 1,
    .invalid_interval_order =
      mean_cellwise_lower_95 > district_pfpr_mean |
      district_pfpr_mean > mean_cellwise_upper_95,
    .invalid_decision_total = abs(.decision_area_total - 1) > 1e-6
  )

district_content_audit <- district |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    districts = dplyr::n(),
    unique_districts = dplyr::n_distinct(paste(Province, District, sep = "::")),
    missing_numeric_values = sum(.missing_numeric_values),
    fallback_districts = sum(continuous_aggregation_method == "nearest_valid_cell_fallback"),
    invalid_probability_values = sum(.invalid_probability, na.rm = TRUE),
    invalid_interval_order = sum(.invalid_interval_order, na.rm = TRUE),
    invalid_decision_area_totals = sum(.invalid_decision_total, na.rm = TRUE),
    .groups = "drop"
  )
write_csv_checked(
  district_content_audit,
  file.path(cfg$dirs$logs, "district_content_audit.csv")
)
if (any(district_content_audit$districts != expected_districts) ||
    any(district_content_audit$unique_districts != expected_districts) ||
    any(district_content_audit$missing_numeric_values != 0L) ||
    any(district_content_audit$invalid_probability_values != 0L) ||
    any(district_content_audit$invalid_interval_order != 0L) ||
    any(district_content_audit$invalid_decision_area_totals != 0L)) {
  stop(
    "District content audit failed: the final output must contain complete results for all 161 districts in every year.",
    call. = FALSE
  )
}

all_outputs <- list.files(file.path(cfg$root, "outputs"), recursive = TRUE, full.names = TRUE)
all_outputs <- all_outputs[file.info(all_outputs)$isdir %in% FALSE]
write_csv_checked(
  file_md5_table(all_outputs, root = cfg$root),
  file.path(cfg$dirs$logs, "all_output_checksums.csv")
)
write_session_information(file.path(cfg$dirs$logs, "sessionInfo.txt"))

message(
  "Output audit passed for all 5 main figures, all 23 supplementary figures, ",
  "manuscript tables and complete 161-district deliverables."
)
