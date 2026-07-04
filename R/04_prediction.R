filter_year_rows <- function(data, year_value, year_column = "year") {
  if (!year_column %in% names(data)) {
    stop("Year column not found: ", year_column, call. = FALSE)
  }
  year_value <- as.integer(year_value)
  year_values <- suppressWarnings(as.integer(as.character(data[[year_column]])))
  keep <- !is.na(year_values) & year_values == year_value
  data[keep, , drop = FALSE]
}

extract_prediction_outputs <- function(model_bundle, prep) {
  idx_est <- INLA::inla.stack.index(model_bundle$stack, "est")$data
  idx_pred <- INLA::inla.stack.index(model_bundle$stack, "pred")$data
  fitted <- model_bundle$fit$summary.fitted.values
  linear <- model_bundle$fit$summary.linear.predictor

  if (length(idx_pred) != nrow(prep$grid)) {
    stop("Prediction stack length does not match the prediction grid.")
  }

  obs_predictions <- prep$obs |>
    dplyr::transmute(
      site_id,
      year = year_end,
      longitude,
      latitude,
      examined,
      pf_pos,
      observed_pfpr = pf_pos / examined,
      fitted_pfpr = pmin(pmax(fitted[idx_est, "mean"], 0), 1),
      fitted_sd = fitted[idx_est, "sd"],
      fitted_lower = pmin(pmax(fitted[idx_est, "0.025quant"], 0), 1),
      fitted_upper = pmin(pmax(fitted[idx_est, "0.975quant"], 0), 1)
    )

  grid_predictions <- prep$grid |>
    dplyr::transmute(
      year = year_anchor,
      month = month_anchor,
      longitude,
      latitude,
      inside_moz,
      pfpr_mean = pmin(pmax(fitted[idx_pred, "mean"], 0), 1),
      pfpr_sd = fitted[idx_pred, "sd"],
      pfpr_lower = pmin(pmax(fitted[idx_pred, "0.025quant"], 0), 1),
      pfpr_upper = pmin(pmax(fitted[idx_pred, "0.975quant"], 0), 1),
      eta_mean = linear[idx_pred, "mean"],
      eta_sd = linear[idx_pred, "sd"]
    ) |>
    dplyr::mutate(pfpr_cv = pfpr_sd / pmax(pfpr_mean, 1e-6))

  list(observations = obs_predictions, grid = grid_predictions)
}

points_to_masked_raster <- function(df, value_column, prep, cfg) {
  df <- df |>
    dplyr::filter(inside_moz, is.finite(.data[[value_column]]))
  extent <- terra::ext(
    min(df$longitude), max(df$longitude),
    min(df$latitude), max(df$latitude)
  )
  template <- terra::rast(
    extent,
    resolution = cfg$raster_resolution_deg,
    crs = "EPSG:4326"
  )
  points <- terra::vect(
    data.frame(
      longitude = df$longitude,
      latitude = df$latitude,
      value = df[[value_column]]
    ),
    geom = c("longitude", "latitude"),
    crs = "EPSG:4326"
  )
  raster <- terra::rasterize(points, template, field = "value", fun = "mean", background = NA_real_)
  terra::mask(raster, terra::vect(prep$moz_map))
}

write_prediction_rasters <- function(grid_predictions, prep, cfg) {
  variables <- c(
    pfpr_mean = "mean",
    pfpr_lower = "lower",
    pfpr_upper = "upper",
    pfpr_sd = "sd",
    pfpr_cv = "cv"
  )
  index <- list()

  for (year in cfg$survey_years) {
    year_data <- filter_year_rows(grid_predictions, year)
    expected <- sum(prep$grid$year_anchor == year)
    if (nrow(year_data) != expected) {
      stop(
        "Year-specific prediction rows are incomplete for ", year,
        ": expected ", expected, ", found ", nrow(year_data), ".",
        call. = FALSE
      )
    }

    paths <- list(year = year)
    for (value_column in names(variables)) {
      raster <- points_to_masked_raster(year_data, value_column, prep, cfg)
      path <- file.path(
        cfg$dirs$rasters,
        paste0("PfPR_", variables[[value_column]], "_year_", year, ".tif")
      )
      terra::writeRaster(raster, path, overwrite = TRUE)
      paths[[value_column]] <- path
    }
    index[[as.character(year)]] <- tibble::as_tibble(paths)
  }

  dplyr::bind_rows(index)
}

project_latent_spatial_field <- function(model_bundle, prep, cfg) {
  spatial_summary <- model_bundle$fit$summary.random$s
  if (is.null(spatial_summary)) stop("Spatial random field summary not found.")

  bbox <- sf::st_bbox(prep$moz_map)
  nx <- max(200L, ceiling((bbox[["xmax"]] - bbox[["xmin"]]) / cfg$raster_resolution_deg))
  ny <- max(300L, ceiling((bbox[["ymax"]] - bbox[["ymin"]]) / cfg$raster_resolution_deg))
  projector <- INLA::inla.mesh.projector(
    model_bundle$spatial$mesh,
    xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
    ylim = c(bbox[["ymin"]], bbox[["ymax"]]),
    dims = c(nx, ny)
  )
  mean_matrix <- INLA::inla.mesh.project(projector, spatial_summary$mean)
  sd_matrix <- INLA::inla.mesh.project(projector, spatial_summary$sd)

  matrix_to_raster <- function(z) {
    r <- terra::rast(
      nrows = nrow(z),
      ncols = ncol(z),
      xmin = bbox[["xmin"]],
      xmax = bbox[["xmax"]],
      ymin = bbox[["ymin"]],
      ymax = bbox[["ymax"]],
      crs = "EPSG:4326"
    )
    terra::values(r) <- as.vector(t(z[nrow(z):1, , drop = FALSE]))
    terra::mask(r, terra::vect(prep$moz_map))
  }

  mean_raster <- matrix_to_raster(mean_matrix)
  sd_raster <- matrix_to_raster(sd_matrix)
  mean_path <- file.path(cfg$dirs$rasters, "latent_spatial_field_mean_logit.tif")
  sd_path <- file.path(cfg$dirs$rasters, "latent_spatial_field_sd.tif")
  terra::writeRaster(mean_raster, mean_path, overwrite = TRUE)
  terra::writeRaster(sd_raster, sd_path, overwrite = TRUE)

  list(mean = mean_path, sd = sd_path)
}

compute_exceedance_outputs <- function(grid_predictions, prep, cfg) {
  threshold_results <- list()
  decision_results <- list()
  decision_summary <- list()

  for (year in cfg$survey_years) {
    year_data <- filter_year_rows(grid_predictions, year) |>
      dplyr::filter(.data$inside_moz)

    for (threshold in cfg$thresholds) {
      probability <- 1 - stats::pnorm(
        (stats::qlogis(threshold) - year_data$eta_mean) / pmax(year_data$eta_sd, 1e-10)
      )
      field_name <- paste0("exceed_", sprintf("%02d", round(100 * threshold)))
      year_data[[field_name]] <- pmin(pmax(probability, 0), 1)
      raster <- points_to_masked_raster(year_data, field_name, prep, cfg)
      path <- file.path(
        cfg$dirs$rasters,
        paste0("Exceed_PfPRgt", round(100 * threshold), "_year_", year, ".tif")
      )
      terra::writeRaster(raster, path, overwrite = TRUE)

      values <- terra::values(raster, mat = FALSE)
      values <- values[is.finite(values)]
      threshold_results[[paste(year, threshold, sep = "_")]] <- tibble::tibble(
        year = year,
        threshold = threshold,
        n_cells = length(values),
        mean_exceedance_probability = mean(values),
        median_exceedance_probability = stats::median(values),
        proportion_probability_ge_0_5 = mean(values >= 0.5),
        raster_file = path
      )
    }

    decision_threshold <- cfg$decision_threshold
    exceed_path <- file.path(
      cfg$dirs$rasters,
      paste0("Exceed_PfPRgt", round(100 * decision_threshold), "_year_", year, ".tif")
    )
    exceed_raster <- terra::rast(exceed_path)
    # Classify the already-rasterised exceedance probability. This avoids
    # averaging categorical decision codes when more than one 5 km grid point
    # falls in the same output raster cell.
    decision_raster <- terra::ifel(
      exceed_raster >= cfg$decision_probability,
      3,
      terra::ifel(exceed_raster <= 1 - cfg$decision_probability, 1, 2)
    )
    decision_path <- file.path(
      cfg$dirs$rasters,
      paste0(
        "Decision3_PfPRgt", round(100 * cfg$decision_threshold),
        "_p", round(100 * cfg$decision_probability), "_year_", year, ".tif"
      )
    )
    terra::writeRaster(decision_raster, decision_path, overwrite = TRUE)

    values <- terra::values(decision_raster, mat = FALSE)
    values <- values[is.finite(values)]
    counts <- table(factor(values, levels = 1:3))
    exceed_values <- terra::values(exceed_raster, mat = FALSE)
    exceed_values <- exceed_values[is.finite(exceed_values)]

    decision_summary[[as.character(year)]] <- tibble::tibble(
      year = year,
      Yes_pct = 100 * unname(counts[["3"]]) / sum(counts),
      Uncertain_pct = 100 * unname(counts[["2"]]) / sum(counts),
      No_pct = 100 * unname(counts[["1"]]) / sum(counts),
      mean_exceedance_probability = mean(exceed_values),
      decision_threshold = cfg$decision_threshold,
      probability_cutoff = cfg$decision_probability
    )
    decision_results[[as.character(year)]] <- tibble::tibble(
      year = year,
      raster_file = decision_path
    )
  }

  list(
    exceedance_index = dplyr::bind_rows(threshold_results),
    decision_index = dplyr::bind_rows(decision_results),
    decision_summary = dplyr::bind_rows(decision_summary)
  )
}

extract_polygon_means_with_fallback <- function(stack, districts) {
  value_names <- names(stack)
  if (!length(value_names) || any(!nzchar(value_names))) {
    stop("All raster layers must have names before district extraction.", call. = FALSE)
  }

  # Use exact polygon-cell overlap fractions together with geodesic cell areas.
  # This yields true area-weighted district means even though the rasters use a
  # longitude-latitude grid whose cell area changes with latitude.
  cell_area_km2 <- terra::cellSize(stack[[1]], unit = "km")
  valid_support <- !is.na(stack[[1]])
  if (terra::nlyr(stack) > 1L) {
    for (i in 2:terra::nlyr(stack)) {
      valid_support <- valid_support & !is.na(stack[[i]])
    }
  }
  valid_area_km2 <- terra::ifel(valid_support, cell_area_km2, NA_real_)
  numerators <- stack * valid_area_km2
  names(numerators) <- paste0("numerator__", value_names)
  extraction_stack <- c(numerators, valid_area_km2)
  names(extraction_stack)[terra::nlyr(extraction_stack)] <- "valid_area_km2"

  district_vector <- terra::vect(districts)
  extracted <- terra::extract(
    extraction_stack,
    district_vector,
    fun = "sum",
    na.rm = TRUE,
    exact = TRUE,
    ID = TRUE
  ) |>
    tibble::as_tibble()

  area_values <- extracted$valid_area_km2
  mean_values <- lapply(value_names, function(value_name) {
    numerator <- extracted[[paste0("numerator__", value_name)]]
    dplyr::if_else(is.finite(area_values) & area_values > 0, numerator / area_values, NA_real_)
  }) |>
    stats::setNames(value_names) |>
    tibble::as_tibble()

  out <- dplyr::bind_cols(
    sf::st_drop_geometry(districts) |> dplyr::select(Province, District),
    mean_values
  ) |>
    dplyr::mutate(
      extraction_area_km2 = area_values,
      aggregation_method = "area_weighted_exact",
      fallback_distance_km = NA_real_,
      fallback_cell_longitude = NA_real_,
      fallback_cell_latitude = NA_real_
    )

  finite_matrix <- as.matrix(out[, value_names, drop = FALSE])
  missing_ids <- which(rowSums(is.finite(finite_matrix)) < length(value_names))
  if (!length(missing_ids)) return(out)

  cell_values <- terra::as.data.frame(stack, xy = TRUE, cells = TRUE, na.rm = FALSE)
  valid_matrix <- as.matrix(cell_values[, value_names, drop = FALSE])
  valid <- rowSums(is.finite(valid_matrix)) == length(value_names)
  cell_values <- cell_values[valid, , drop = FALSE]
  if (!nrow(cell_values)) {
    stop("No valid raster cells are available for district fallback extraction.", call. = FALSE)
  }

  raster_crs <- terra::crs(stack, proj = TRUE)
  valid_points <- sf::st_as_sf(
    cell_values,
    coords = c("x", "y"),
    crs = raster_crs,
    remove = FALSE
  )
  district_points <- suppressWarnings(
    sf::st_point_on_surface(sf::st_make_valid(districts[missing_ids, , drop = FALSE]))
  )
  district_points <- sf::st_transform(district_points, sf::st_crs(valid_points))
  nearest <- sf::st_nearest_feature(district_points, valid_points)
  distances <- as.numeric(
    sf::st_distance(district_points, valid_points[nearest, , drop = FALSE], by_element = TRUE)
  ) / 1000

  for (value_name in value_names) {
    out[[value_name]][missing_ids] <- cell_values[[value_name]][nearest]
  }
  out$extraction_area_km2[missing_ids] <- 0
  out$aggregation_method[missing_ids] <- "nearest_valid_cell_fallback"
  out$fallback_distance_km[missing_ids] <- distances
  out$fallback_cell_longitude[missing_ids] <- cell_values$x[nearest]
  out$fallback_cell_latitude[missing_ids] <- cell_values$y[nearest]
  out
}

extract_district_continuous <- function(raster_index, prep, cfg) {
  districts <- prep$dist_map |>
    dplyr::select(Province, District, geometry)

  purrr::map_dfr(cfg$survey_years, function(year) {
    row <- filter_year_rows(raster_index, year)
    if (nrow(row) != 1L) stop("Raster index is incomplete for year ", year)
    stack <- c(
      terra::rast(row$pfpr_mean),
      terra::rast(row$pfpr_lower),
      terra::rast(row$pfpr_upper)
    )
    names(stack) <- c("pfpr_mean", "pfpr_lower", "pfpr_upper")

    extract_polygon_means_with_fallback(stack, districts) |>
      dplyr::rename(
        district_pfpr_mean = pfpr_mean,
        mean_cellwise_lower_95 = pfpr_lower,
        mean_cellwise_upper_95 = pfpr_upper,
        continuous_valid_raster_area_km2 = extraction_area_km2,
        continuous_aggregation_method = aggregation_method,
        continuous_fallback_distance_km = fallback_distance_km,
        continuous_fallback_cell_longitude = fallback_cell_longitude,
        continuous_fallback_cell_latitude = fallback_cell_latitude
      ) |>
      dplyr::mutate(year = .env$year, .before = 1)
  })
}

extract_district_decisions <- function(exceedance_outputs, prep, cfg) {
  districts <- prep$dist_map |>
    dplyr::select(Province, District, geometry)

  purrr::map_dfr(cfg$survey_years, function(year) {
    decision_file <- filter_year_rows(exceedance_outputs$decision_index, year) |>
      dplyr::pull(raster_file)
    exceed_file <- filter_year_rows(exceedance_outputs$exceedance_index, year) |>
      dplyr::filter(abs(.data$threshold - cfg$decision_threshold) < 1e-8) |>
      dplyr::pull(raster_file)
    if (length(decision_file) != 1L || length(exceed_file) != 1L) {
      stop("Decision or exceedance raster missing for year ", year)
    }

    decision <- terra::rast(decision_file)
    exceed <- terra::rast(exceed_file)
    stack <- c(exceed, decision == 3, decision == 2, decision == 1)
    names(stack) <- c(
      "mean_exceedance_probability",
      "area_prop_yes",
      "area_prop_uncertain",
      "area_prop_no"
    )

    extract_polygon_means_with_fallback(stack, districts) |>
      dplyr::rename(
        decision_valid_raster_area_km2 = extraction_area_km2,
        decision_aggregation_method = aggregation_method,
        decision_fallback_distance_km = fallback_distance_km,
        decision_fallback_cell_longitude = fallback_cell_longitude,
        decision_fallback_cell_latitude = fallback_cell_latitude
      ) |>
      dplyr::mutate(
        year = .env$year,
        mean_cell_exceedance_class = dplyr::case_when(
          mean_exceedance_probability >= cfg$decision_probability ~ "Yes",
          mean_exceedance_probability <= 1 - cfg$decision_probability ~ "No",
          TRUE ~ "Uncertain"
        ),
        dominant_gridcell_decision = dplyr::case_when(
          area_prop_yes >= pmax(area_prop_uncertain, area_prop_no) ~ "Yes",
          area_prop_no >= pmax(area_prop_uncertain, area_prop_yes) ~ "No",
          TRUE ~ "Uncertain"
        ),
        .before = 1
      )
  })
}
