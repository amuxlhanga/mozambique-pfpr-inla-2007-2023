source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
raster_index <- readr::read_csv(
  file.path(cfg$dirs$rasters, "prediction_raster_index.csv"),
  show_col_types = FALSE
)
exceedance <- read_rds_required(
  file.path(cfg$dirs$model, "08_exceedance_and_decision_outputs.rds")
)

district_continuous <- extract_district_continuous(raster_index, prep, cfg) |>
  dplyr::mutate(
    district_pfpr_mean_pct = 100 * district_pfpr_mean,
    mean_cellwise_lower_95_pct = 100 * mean_cellwise_lower_95,
    mean_cellwise_upper_95_pct = 100 * mean_cellwise_upper_95
  )

district_decisions <- extract_district_decisions(exceedance, prep, cfg) |>
  dplyr::mutate(
    mean_exceedance_probability_pct = 100 * mean_exceedance_probability,
    area_pct_yes = 100 * area_prop_yes,
    area_pct_uncertain = 100 * area_prop_uncertain,
    area_pct_no = 100 * area_prop_no
  )

district_all <- dplyr::left_join(
  district_continuous,
  district_decisions,
  by = c("year", "Province", "District")
)

expected_districts <- nrow(prep$dist_map)
required_numeric <- c(
  "district_pfpr_mean", "mean_cellwise_lower_95", "mean_cellwise_upper_95",
  "mean_exceedance_probability", "area_prop_yes", "area_prop_uncertain", "area_prop_no"
)

missing_numeric_matrix <- as.matrix(district_all[, required_numeric, drop = FALSE])
district_all$.missing_numeric_values <- rowSums(!is.finite(missing_numeric_matrix))

district_all <- district_all |>
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

district_audit <- district_all |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    districts = dplyr::n(),
    complete_continuous = sum(is.finite(district_pfpr_mean)),
    complete_decision = sum(is.finite(mean_exceedance_probability)),
    continuous_fallbacks = sum(continuous_aggregation_method == "nearest_valid_cell_fallback"),
    decision_fallbacks = sum(decision_aggregation_method == "nearest_valid_cell_fallback"),
    remaining_missing_values = sum(.missing_numeric_values),
    invalid_probability_values = sum(.invalid_probability, na.rm = TRUE),
    invalid_interval_order = sum(.invalid_interval_order, na.rm = TRUE),
    invalid_decision_area_totals = sum(.invalid_decision_total, na.rm = TRUE),
    .groups = "drop"
  )

district_all <- district_all |>
  dplyr::select(
    -.missing_numeric_values,
    -.decision_area_total,
    -.invalid_probability,
    -.invalid_interval_order,
    -.invalid_decision_total
  )

if (any(district_audit$districts != expected_districts)) {
  stop(
    "District aggregation has the wrong number of rows. Expected ", expected_districts,
    " districts in every year.",
    call. = FALSE
  )
}
if (isTRUE(cfg$district_aggregation$require_complete) &&
    any(district_audit$remaining_missing_values > 0L)) {
  stop(
    "District aggregation still contains missing values after the documented nearest-cell fallback.",
    call. = FALSE
  )
}
if (any(district_audit$invalid_probability_values > 0L) ||
    any(district_audit$invalid_interval_order > 0L) ||
    any(district_audit$invalid_decision_area_totals > 0L)) {
  stop(
    "District aggregation failed range, interval-order or decision-proportion checks.",
    call. = FALSE
  )
}

district_dictionary <- tibble::tribble(
  ~column, ~description,
  "year", "Survey-aligned prediction year.",
  "Province", "Province name from the 161-district boundary file.",
  "District", "District name from the 161-district boundary file.",
  "district_pfpr_mean", "Mean grid-cell posterior PfPR using exact polygon-cell overlap fractions and geodesic cell-area weights; nearest valid land cell used only when the district has no valid raster support.",
  "mean_cellwise_lower_95", "Area-weighted mean of grid-cell posterior 2.5% quantiles; not a district-level credible limit.",
  "mean_cellwise_upper_95", "Area-weighted mean of grid-cell posterior 97.5% quantiles; not a district-level credible limit.",
  "continuous_valid_raster_area_km2", "District raster area with valid values used for continuous-result aggregation; zero indicates nearest-cell fallback.",
  "continuous_aggregation_method", "area_weighted_exact or nearest_valid_cell_fallback.",
  "continuous_fallback_distance_km", "Distance from the district point-on-surface to the nearest valid prediction cell when fallback was required.",
  "continuous_fallback_cell_longitude", "Longitude of the fallback prediction cell.",
  "continuous_fallback_cell_latitude", "Latitude of the fallback prediction cell.",
  "mean_exceedance_probability", "Area-weighted mean probability that grid-cell PfPR exceeds the configured 20% threshold.",
  "decision_valid_raster_area_km2", "District raster area with valid values used for exceedance and decision aggregation; zero indicates nearest-cell fallback.",
  "area_prop_yes", "Proportion of district raster area classified Yes at p*=0.80.",
  "area_prop_uncertain", "Proportion of district raster area classified Uncertain.",
  "area_prop_no", "Proportion of district raster area classified No at p*=0.80.",
  "decision_aggregation_method", "area_weighted_exact or nearest_valid_cell_fallback for decision outputs.",
  "mean_cell_exceedance_class", "Class obtained by applying p*=0.80 to the district mean of grid-cell exceedance probabilities.",
  "dominant_gridcell_decision", "Most common grid-cell decision class by district raster area; ties are resolved in favour of Yes, then No, then Uncertain."
)

fallback_records <- district_all |>
  dplyr::filter(
    continuous_aggregation_method == "nearest_valid_cell_fallback" |
      decision_aggregation_method == "nearest_valid_cell_fallback"
  ) |>
  dplyr::select(
    year, Province, District,
    continuous_aggregation_method,
    continuous_fallback_distance_km,
    continuous_fallback_cell_longitude,
    continuous_fallback_cell_latitude,
    decision_aggregation_method,
    decision_fallback_distance_km,
    decision_fallback_cell_longitude,
    decision_fallback_cell_latitude
  )

write_csv_checked(district_continuous, file.path(cfg$dirs$district, "district_pfpr_by_year.csv"))
write_csv_checked(district_decisions, file.path(cfg$dirs$district, "district_exceedance_decisions_by_year.csv"))
write_csv_checked(district_all, file.path(cfg$dirs$district, "district_results_all_by_year.csv"))
write_csv_checked(district_audit, file.path(cfg$dirs$district, "district_aggregation_audit.csv"))
write_csv_checked(fallback_records, file.path(cfg$dirs$district, "district_fallback_records.csv"))
write_csv_checked(district_dictionary, file.path(cfg$dirs$district, "district_results_data_dictionary.csv"))

sf_long <- prep$dist_map |>
  dplyr::select(Province, District, geometry) |>
  dplyr::left_join(district_all, by = c("Province", "District"))
sf::st_write(
  sf_long,
  file.path(cfg$dirs$district, "district_results_all_by_year.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

unlink(c(
  file.path(cfg$dirs$district, "Figure_district_mean_pfpr_by_year.png"),
  file.path(cfg$dirs$figures_main, "Figure_3_district_mean_pfpr_by_year.png")
), force = TRUE)
save_plot(
  plot_district_pfpr(district_continuous, prep, cfg),
  file.path(cfg$dirs$figures_main, "Figure_3_district_mean_pfpr_by_year.png"),
  width = 10.5, height = 8.0
)
save_plot(
  plot_district_decisions(district_decisions, prep, cfg),
  file.path(cfg$dirs$district, "Figure_district_decision_by_year.png"),
  width = 10.5, height = 8.0
)
message(
  "Main Figure 3 and complete district results written for all ",
  expected_districts, " districts in every year."
)
