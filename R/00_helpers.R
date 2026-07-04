`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

required_packages <- function() {
  c(
    "INLA", "car", "dplyr", "ggplot2", "grid", "patchwork", "purrr", "readr",
    "rlang", "scales", "sf", "sp", "stringr", "terra", "tibble", "tidyr"
  )
}

check_packages <- function(packages = required_packages()) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Install them before running the pipeline.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

create_output_dirs <- function(cfg) {
  dirs <- unlist(cfg$dirs, use.names = FALSE)
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

assert_files_exist <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Required input files are missing:\n", paste0("  - ", missing, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L) return(NA_real_)
  stats::cor(x[ok], y[ok])
}

inv_logit <- function(x) stats::plogis(x)

clean_term_label <- function(x) {
  x |>
    stringr::str_replace("^z_", "") |>
    stringr::str_replace("_lag([0-3])$", " (lag \\1)") |>
    stringr::str_replace("Access_Cities_log1p", "Access to cities (log1p)") |>
    stringr::str_replace("Access_Health_log1p", "Access to health facilities (log1p)") |>
    stringr::str_replace("DistToWater_m", "Distance to water") |>
    stringr::str_replace("Aridity_Index", "Aridity index") |>
    stringr::str_replace("Slope_deg", "Slope") |>
    stringr::str_replace_all("_", " ")
}

write_csv_checked <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path)
  if (!file.exists(path)) stop("Failed to write: ", path, call. = FALSE)
  invisible(path)
}

save_plot <- function(plot, path, width, height, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  if (!file.exists(path)) stop("Failed to write: ", path, call. = FALSE)
  invisible(path)
}

find_first_existing <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit)) hit[[1L]] else NULL
}

file_md5_table <- function(paths, root = NULL) {
  paths <- paths[file.exists(paths)]
  absolute <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  file_label <- absolute
  if (!is.null(root)) {
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    prefix <- paste0(root, "/")
    file_label <- ifelse(startsWith(absolute, prefix), substring(absolute, nchar(prefix) + 1L), absolute)
  }
  tibble::tibble(
    file = file_label,
    bytes = unname(file.info(paths)$size),
    md5 = unname(tools::md5sum(paths))
  )
}

write_session_information <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- capture.output({
    print(sessionInfo())
    if (requireNamespace("INLA", quietly = TRUE)) {
      cat("\nINLA version:\n")
      print(utils::packageVersion("INLA"))
    }
  })
  writeLines(lines, con = path, useBytes = TRUE)
  invisible(path)
}

read_rds_required <- function(path) {
  if (!file.exists(path)) stop("Required stage output is missing: ", path, call. = FALSE)
  readRDS(path)
}

add_malawi_label <- function(cfg, size = 2.8) {
  ggplot2::annotate(
    "text",
    x = cfg$map_context$longitude,
    y = cfg$map_context$latitude,
    label = cfg$map_context$label,
    colour = "grey45",
    size = size,
    angle = 90
  )
}

pfpr_fill_scale <- function(name = "PfPR") {
  ggplot2::scale_fill_gradientn(
    colours = c("#08306B", "#2171B5", "#6BAED6", "#FEE08B", "#F46D43", "#A50026"),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = scales::percent_format(accuracy = 1),
    oob = scales::squish,
    na.value = "grey95",
    name = name
  )
}

pfpr_colour_scale <- function(name = "Observed PfPR") {
  ggplot2::scale_colour_gradientn(
    colours = c("#08306B", "#6BAED6", "#FEE08B", "#F46D43", "#A50026"),
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    oob = scales::squish,
    name = name
  )
}


supplementary_model_figure_number <- function(year) {
  numbers <- c(`2007` = 17L, `2011` = 18L, `2015` = 19L, `2018` = 20L, `2022` = 21L, `2023` = 22L)
  value <- unname(numbers[as.character(year)])
  if (length(value) != 1L || is.na(value)) stop("Unsupported survey year: ", year, call. = FALSE)
  value
}

manuscript_figure_manifest <- function(cfg) {
  main <- tibble::tribble(
    ~figure, ~section, ~description, ~path,
    "Figure 1", "Main", "Observed cluster-level PfPR by survey year", file.path(cfg$dirs$figures_main, "Figure_1_observed_cluster_pfpr_by_year.png"),
    "Figure 2", "Main", "Year-specific posterior mean PfPR surfaces", file.path(cfg$dirs$figures_main, "Figure_2_year_specific_posterior_mean_pfpr.png"),
    "Figure 3", "Main", "Area-weighted district mean PfPR by survey year", file.path(cfg$dirs$figures_main, "Figure_3_district_mean_pfpr_by_year.png"),
    "Figure 4", "Main", "Spatial hyperparameter posterior densities", file.path(cfg$dirs$figures_main, "Figure_4_spatial_hyperparameter_densities.png"),
    "Figure 5", "Main", "Decision maps at PfPR 20% and posterior probability 0.80", file.path(cfg$dirs$figures_main, "Figure_5_decision_maps_pfpr20_p80.png")
  )

  supplementary <- tibble::tribble(
    ~figure, ~section, ~description, ~path,
    "Figure S1", "Supplementary", "Number of survey clusters by year", file.path(cfg$dirs$figures_supp, "Figure_S1_clusters_by_year.png"),
    "Figure S2", "Supplementary", "Survey clusters by month and year", file.path(cfg$dirs$figures_supp, "Figure_S2_clusters_by_month_and_year.png"),
    "Figure S3", "Supplementary", "Spatial distribution of survey clusters", file.path(cfg$dirs$figures_supp, "Figure_S3_spatial_distribution_all_clusters.png"),
    "Figure S4", "Supplementary", "Distribution of observed cluster-level PfPR", file.path(cfg$dirs$figures_supp, "Figure_S4_observed_cluster_pfpr_distribution.png"),
    "Figure S5", "Supplementary", "Candidate SPDE meshes", file.path(cfg$dirs$figures_supp, "Figure_S5_candidate_spde_meshes.png"),
    "Figure S6", "Supplementary", "Correlation matrix of retained covariates", file.path(cfg$dirs$figures_supp, "Figure_S6_correlation_matrix_retained_covariates.png"),
    "Figure S7", "Supplementary", "Posterior density summaries of fixed effects", file.path(cfg$dirs$figures_supp, "Figure_S7_fixed_effect_posterior_densities.png"),
    "Figure S8", "Supplementary", "Internal validation by survey year", file.path(cfg$dirs$figures_supp, "Figure_S8_internal_validation_by_year.png"),
    "Figure S9", "Supplementary", "Internal calibration by survey year using fitted-risk deciles", file.path(cfg$dirs$figures_supp, "Figure_S9_internal_calibration_deciles_by_year.png"),
    "Figure S10", "Supplementary", "Spatial block cross-validation by fold", file.path(cfg$dirs$figures_supp, "Figure_S10_spatial_block_cv_scatter_by_fold.png"),
    "Figure S11", "Supplementary", "Spatial block cross-validation by survey year", file.path(cfg$dirs$figures_supp, "Figure_S11_spatial_block_cv_scatter_by_year.png"),
    "Figure S12", "Supplementary", "Spatial block calibration by year using fixed-width bins", file.path(cfg$dirs$figures_supp, "Figure_S12_spatial_block_cv_calibration_fixed_bins_by_year.png"),
    "Figure S13", "Supplementary", "Overall spatial block calibration using fixed-width bins", file.path(cfg$dirs$figures_supp, "Figure_S13_spatial_block_cv_calibration_fixed_bins_overall.png"),
    "Figure S14", "Supplementary", "Spatial block calibration by year using prediction deciles", file.path(cfg$dirs$figures_supp, "Figure_S14_spatial_block_cv_calibration_deciles_by_year.png"),
    "Figure S15", "Supplementary", "Overall spatial block calibration using prediction deciles", file.path(cfg$dirs$figures_supp, "Figure_S15_spatial_block_cv_calibration_deciles_overall.png"),
    "Figure S16", "Supplementary", "Posterior mean and SD of the latent spatial field", file.path(cfg$dirs$figures_supp, "Figure_S16_latent_spatial_field_mean_and_sd.png"),
    "Figure S17", "Supplementary", "Model lower, mean and upper PfPR surfaces for 2007", file.path(cfg$dirs$figures_supp, "Figure_S17_model_pfpr_lower_mean_upper_2007.png"),
    "Figure S18", "Supplementary", "Model lower, mean and upper PfPR surfaces for 2011", file.path(cfg$dirs$figures_supp, "Figure_S18_model_pfpr_lower_mean_upper_2011.png"),
    "Figure S19", "Supplementary", "Model lower, mean and upper PfPR surfaces for 2015", file.path(cfg$dirs$figures_supp, "Figure_S19_model_pfpr_lower_mean_upper_2015.png"),
    "Figure S20", "Supplementary", "Model lower, mean and upper PfPR surfaces for 2018", file.path(cfg$dirs$figures_supp, "Figure_S20_model_pfpr_lower_mean_upper_2018.png"),
    "Figure S21", "Supplementary", "Model lower, mean and upper PfPR surfaces for 2022", file.path(cfg$dirs$figures_supp, "Figure_S21_model_pfpr_lower_mean_upper_2022.png"),
    "Figure S22", "Supplementary", "Model lower, mean and upper PfPR surfaces for 2023", file.path(cfg$dirs$figures_supp, "Figure_S22_model_pfpr_lower_mean_upper_2023.png"),
    "Figure S23", "Supplementary", "Exceedance probabilities at 10%, 20%, 30% and 40%", file.path(cfg$dirs$figures_supp, "Figure_S23_exceedance_probabilities_all_years_thresholds.png")
  )

  dplyr::bind_rows(main, supplementary) |>
    dplyr::mutate(
      required = TRUE,
      relative_path = substring(.data$path, nchar(cfg$root) + 2L)
    )
}
