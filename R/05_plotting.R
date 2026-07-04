map_theme <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "right",
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1)
    )
}

validation_theme <- function(base_size = 9) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

raster_to_data_frame <- function(path, value_name, ...) {
  r <- terra::rast(path)
  out <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  if (ncol(out) != 3L) stop("Expected a single-layer raster: ", path)
  names(out) <- c("longitude", "latitude", value_name)
  tibble::as_tibble(out) |>
    dplyr::mutate(...)
}

plot_observed_pfpr_by_year <- function(prep, cfg) {
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = prep$obs,
      ggplot2::aes(
        x = longitude,
        y = latitude,
        colour = PfPR,
        size = examined
      ),
      alpha = 0.82
    ) +
    ggplot2::geom_sf(
      data = prep$dist_map,
      fill = NA,
      colour = "grey45",
      linewidth = 0.18,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~year_factor, ncol = 3) +
    pfpr_colour_scale("Observed PfPR") +
    ggplot2::scale_size_continuous(range = c(0.4, 3.6), name = "Examined") +
    add_malawi_label(cfg, size = 2.4) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    map_theme()
}

plot_clusters_by_year <- function(prep) {
  counts <- prep$obs |>
    dplyr::count(year_end, name = "clusters")
  ggplot2::ggplot(counts, ggplot2::aes(x = factor(year_end), y = clusters)) +
    ggplot2::geom_col(width = 0.72, fill = "grey35") +
    ggplot2::labs(x = "Year", y = "Number of clusters") +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

plot_clusters_by_month_year <- function(prep) {
  dat <- prep$obs |>
    dplyr::filter(!is.na(month_end)) |>
    dplyr::count(year_factor, month_end, name = "clusters")
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = factor(month_end, levels = 1:12), y = clusters, fill = year_factor)
  ) +
    ggplot2::geom_col(position = "dodge", width = 0.8) +
    ggplot2::labs(x = "Month", y = "Number of clusters", fill = "Year") +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "right")
}

plot_all_cluster_locations <- function(prep) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = prep$dist_map,
      fill = NA,
      colour = "grey65",
      linewidth = 0.17
    ) +
    ggplot2::geom_point(
      data = prep$obs,
      ggplot2::aes(x = longitude, y = latitude, colour = year_factor),
      size = 0.7,
      alpha = 0.72
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude", colour = "Year") +
    map_theme()
}

plot_observed_pfpr_distribution <- function(prep) {
  ggplot2::ggplot(prep$obs, ggplot2::aes(x = PfPR)) +
    ggplot2::geom_histogram(binwidth = 0.025, boundary = 0, colour = "white", fill = "grey35") +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(x = "Observed cluster-level PfPR", y = "Number of clusters") +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

save_candidate_mesh_figure <- function(spatial, prep, path, width = 11, height = 4.2, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(path, width = width, height = height, units = "in", res = dpi, bg = "white")
  old <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mfrow = c(1, length(spatial$meshes)), mar = c(2, 2, 2, 1))
  for (nm in names(spatial$meshes)) {
    plot(spatial$meshes[[nm]], asp = 1, main = nm, col = "grey70", lwd = 0.35)
    plot(sf::st_geometry(prep$moz_map), add = TRUE, border = "#2166AC", lwd = 1)
    graphics::points(prep$coords_obs[, 1], prep$coords_obs[, 2], pch = 16, cex = 0.25, col = "#B2182B")
  }
  invisible(path)
}

covariate_dictionary <- function(cfg) {
  tibble::tribble(
    ~covariate, ~definition, ~units, ~type, ~lags_evaluated, ~source_class, ~final_status,
    "Rainfall (CHIRPS)", "Monthly rainfall representing creation and persistence of mosquito breeding sites.", "mm/month", "Dynamic", "0–3 months", "CHIRPS rainfall surface", "Screened, not retained",
    "Enhanced Vegetation Index (EVI)", "Satellite measure of vegetation greenness and density.", "Index", "Dynamic", "0–3 months", "MODIS-derived vegetation surface", "Retained (lag 2)",
    "LST day", "Daytime land surface temperature.", "Degrees C", "Dynamic", "0–3 months", "MODIS-derived temperature surface", "Retained (lag 0)",
    "LST night", "Night-time land surface temperature.", "Degrees C", "Dynamic", "0–3 months", "MODIS-derived temperature surface", "Screened, not retained",
    "LST delta", "Diurnal difference between daytime and night-time land surface temperature.", "Degrees C", "Dynamic", "0–3 months", "Derived from LST day and night", "Screened, not retained",
    "Tasselled Cap Brightness (TCB)", "Reflectance-based index of surface brightness and bare or dry land cover.", "Index", "Dynamic", "0–3 months", "MODIS-derived reflectance surface", "Screened, not retained",
    "Tasselled Cap Wetness (TCW)", "Reflectance-based index of surface wetness and moisture availability.", "Index", "Dynamic", "0–3 months", "MODIS-derived reflectance surface", "Screened, not retained",
    "Temperature Suitability Index (TSI)", "Mechanistic index of thermal suitability for malaria transmission.", "Index", "Dynamic", "0–3 months", "Suitability surface", "Retained (lag 3)",
    "Habitat Suitability Index (HSI)", "Mechanistic index of persistence and suitability of larval habitat.", "Index", "Dynamic", "0–3 months", "Suitability surface", "Retained (lag 2)",
    "Potential evapotranspiration (PET)", "Atmospheric evaporative demand.", "mm/month", "Static", "Not applicable", "Climatic PET surface", "Screened, not retained",
    "Aridity index", "Ratio describing climatic dryness or wetness.", "Ratio", "Static", "Not applicable", "Derived climatic surface", "Retained",
    "Night-time lights", "Proxy for settlement intensity and human development.", "Radiance", "Static", "Not applicable", "VIIRS/stable night-light surface", "Screened, not retained",
    "Elevation", "Height above sea level.", "m", "Static", "Not applicable", "SRTM", "Screened, not retained",
    "Slope", "Terrain steepness.", "Degrees", "Static", "Not applicable", "Derived from SRTM", "Retained",
    "Distance to water", "Distance to the nearest mapped river, lake or water body.", "m", "Static", "Not applicable", "Surface-water hydrology layer", "Retained",
    "Topographic Wetness Index (TWI)", "Terrain-based tendency for local water accumulation.", "Index", "Static", "Not applicable", "SRTM/HydroSHEDS-derived", "Screened, not retained",
    "Access to cities", "Travel-time accessibility to urban centres.", "Minutes travel", "Static", "Not applicable", "Accessibility surface", "Retained after log1p transformation",
    "Population density", "Number of people per square kilometre.", "Persons/km2", "Static", "Not applicable", "WorldPop", "Screened, not retained",
    "Access to health facilities", "Travel time to the nearest mapped health facility.", "Minutes travel", "Static", "Not applicable", "Accessibility surface", "Screened, not retained"
  )
}

forced_suitability_lags <- function(cfg) {
  tibble::tibble(
    covariate = c("Temperature Suitability Index (TSI)", "Habitat Suitability Index (HSI)"),
    lags_evaluated = c("0–3 months", "0–3 months"),
    retained_lag = c(3L, 2L)
  )
}

thematic_group_table <- function(cfg) {
  purrr::imap_dfr(
    cfg$thematic_groups,
    ~ tibble::tibble(group = .y, covariates = paste(.x, collapse = ", "))
  )
}

compute_vif_table <- function(prep, cfg) {
  z <- paste0("z_", cfg$final_covariates_raw)
  dat <- as.data.frame(prep$X_obs[, z, drop = FALSE])
  dat$PfPR <- prep$obs$PfPR
  fit <- stats::lm(PfPR ~ ., data = dat)
  vif <- car::vif(fit)
  tibble::tibble(
    covariate = names(vif),
    VIF = as.numeric(vif)
  ) |>
    dplyr::mutate(covariate = sub("^z_", "", covariate)) |>
    dplyr::arrange(dplyr::desc(VIF))
}

retained_correlation_matrix <- function(prep, cfg) {
  z <- paste0("z_", cfg$final_covariates_raw)
  out <- stats::cor(prep$X_obs[, z, drop = FALSE], use = "pairwise.complete.obs")
  rownames(out) <- clean_term_label(rownames(out))
  colnames(out) <- clean_term_label(colnames(out))
  out
}

plot_correlation_matrix <- function(cor_matrix) {
  dat <- as.data.frame(as.table(cor_matrix), stringsAsFactors = FALSE)
  names(dat) <- c("row", "column", "correlation")
  ggplot2::ggplot(dat, ggplot2::aes(x = column, y = row, fill = correlation)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 2.5) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), name = "Correlation"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


plot_fixed_effect_forest <- function(fixed) {
  required <- c("covariate", "posterior_mean", "lower_95", "upper_95")
  missing <- setdiff(required, names(fixed))
  if (length(missing)) stop("Fixed-effect table is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  dat <- fixed |>
    dplyr::mutate(
      covariate = factor(.data$covariate, levels = rev(.data$covariate))
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$posterior_mean, y = .data$covariate)) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, linewidth = 0.45) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$lower_95, xend = .data$upper_95, yend = .data$covariate),
      linewidth = 0.55
    ) +
    ggplot2::geom_point(size = 2.1) +
    ggplot2::labs(
      x = "Posterior mean and 95% credible interval\n(logit scale)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8.5),
      axis.text.x = ggplot2::element_text(size = 8.5),
      axis.title.x = ggplot2::element_text(size = 9.5, margin = ggplot2::margin(t = 8)),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 6, r = 8, b = 8, l = 6)
    )
}

plot_fixed_effect_densities <- function(model_bundle) {
  marginals <- model_bundle$fit$marginals.fixed
  if (is.null(marginals) || !length(marginals)) stop("Fixed-effect marginals are unavailable.")
  dat <- purrr::imap_dfr(marginals, function(marginal, term) {
    sm <- as.data.frame(INLA::inla.smarginal(marginal))
    names(sm) <- c("value", "density")
    tibble::as_tibble(sm) |>
      dplyr::mutate(term = clean_term_label(term))
  })
  ggplot2::ggplot(dat, ggplot2::aes(x = value, y = density)) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::facet_wrap(~term, scales = "free", ncol = 3) +
    ggplot2::labs(x = "Coefficient on logit scale", y = "Posterior density") +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank())
}

plot_spatial_hyperparameter_densities <- function(hyper) {
  range_km <- INLA::inla.tmarginal(function(x) x * 111.32, hyper$transformed$marginals.range[[1]])
  spatial_sd <- INLA::inla.tmarginal(sqrt, hyper$transformed$marginals.var[[1]])
  kappa <- hyper$transformed$marginals.kap[[1]]
  marginals <- list(
    "Practical spatial range (km)" = range_km,
    "Marginal spatial SD" = spatial_sd,
    "Kappa" = kappa
  )
  dat <- purrr::imap_dfr(marginals, function(marginal, parameter) {
    sm <- as.data.frame(INLA::inla.smarginal(marginal))
    names(sm) <- c("value", "density")
    tibble::as_tibble(sm) |>
      dplyr::mutate(parameter = parameter)
  })
  ggplot2::ggplot(dat, ggplot2::aes(x = value, y = density)) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::facet_wrap(~parameter, scales = "free", nrow = 1) +
    ggplot2::labs(x = NULL, y = "Posterior density") +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank())
}

plot_validation_scatter <- function(predictions, facet = c("year", "fold"), title = NULL) {
  facet <- match.arg(facet)
  group_var <- if (facet == "year") "year" else "fold"
  labels <- predictions |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarise(r = safe_cor(obs_pfpr, pred_pfpr), .groups = "drop") |>
    dplyr::mutate(label = ifelse(is.finite(r), sprintf("r = %.2f", r), "r = NA"))
  p <- ggplot2::ggplot(predictions, ggplot2::aes(x = obs_pfpr, y = pred_pfpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.4) +
    ggplot2::geom_point(size = 0.75, alpha = 0.45) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = 0.04, y = 0.96, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 2.7
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Observed PfPR", y = "Predicted PfPR", title = title) +
    validation_theme()
  if (facet == "year") p + ggplot2::facet_wrap(~year, ncol = 3) else p + ggplot2::facet_wrap(~fold, ncol = 3)
}

plot_internal_validation <- function(internal) {
  pred <- internal$predictions
  labels <- internal$by_year |>
    dplyr::mutate(label = ifelse(is.finite(r), sprintf("r = %.2f", r), "r = NA"))
  ggplot2::ggplot(pred, ggplot2::aes(x = obs_pfpr, y = pred_pfpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.4) +
    ggplot2::geom_point(size = 0.75, alpha = 0.35) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = 0.04, y = 0.96, label = label),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1, size = 2.7
    ) +
    ggplot2::facet_wrap(~year, ncol = 3) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Observed PfPR", y = "Fitted PfPR") +
    validation_theme()
}

plot_calibration <- function(calibration, facet_year = TRUE, title = NULL) {
  p <- ggplot2::ggplot(calibration, ggplot2::aes(x = mean_pred, y = mean_obs)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.4) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.18,
      colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Mean predicted PfPR", y = "Mean observed PfPR", title = title) +
    validation_theme()
  if (facet_year && "year" %in% names(calibration)) p + ggplot2::facet_wrap(~year, ncol = 3) else p
}

plot_validation_method_comparison <- function(overall_table) {
  dat <- overall_table |>
    dplyr::filter(validation != "Internal fitted values") |>
    tidyr::pivot_longer(c(r, RMSE, MAE, Bias), names_to = "metric", values_to = "value")
  ggplot2::ggplot(dat, ggplot2::aes(x = validation, y = value, fill = validation)) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::facet_wrap(~metric, scales = "free_y", nrow = 1) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      strip.background = ggplot2::element_blank()
    )
}

plot_prediction_mean_by_year <- function(raster_index, prep, cfg) {
  dat <- purrr::map_dfr(seq_len(nrow(raster_index)), function(i) {
    raster_to_data_frame(
      raster_index$pfpr_mean[[i]],
      "pfpr_mean",
      year = raster_index$year[[i]]
    )
  })
  ggplot2::ggplot(dat, ggplot2::aes(x = longitude, y = latitude, fill = pfpr_mean)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(
      data = prep$dist_map,
      fill = NA,
      colour = "grey30",
      linewidth = 0.16,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~year, ncol = 3) +
    pfpr_fill_scale("PfPR") +
    add_malawi_label(cfg, size = 2.5) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    map_theme()
}

plot_uncertainty_surfaces_for_year <- function(raster_index, prep, cfg, year) {
  row <- filter_year_rows(raster_index, year)
  if (nrow(row) != 1L) stop("Prediction raster index is incomplete for year ", year)
  dat <- dplyr::bind_rows(
    raster_to_data_frame(row$pfpr_lower[[1]], "pfpr", summary = "Lower 95%"),
    raster_to_data_frame(row$pfpr_mean[[1]], "pfpr", summary = "Mean"),
    raster_to_data_frame(row$pfpr_upper[[1]], "pfpr", summary = "Upper 95%")
  ) |>
    dplyr::mutate(summary = factor(summary, levels = c("Lower 95%", "Mean", "Upper 95%")))
  ggplot2::ggplot(dat, ggplot2::aes(x = longitude, y = latitude, fill = pfpr)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(
      data = prep$dist_map,
      fill = NA,
      colour = "grey35",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~summary, nrow = 1) +
    pfpr_fill_scale("PfPR") +
    add_malawi_label(cfg, size = 2.3) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude", title = paste("PfPR", year)) +
    map_theme()
}

plot_latent_spatial_fields <- function(latent_index, prep, cfg) {
  mean_df <- raster_to_data_frame(latent_index$mean, "value")
  sd_df <- raster_to_data_frame(latent_index$sd, "value")
  p_mean <- ggplot2::ggplot(mean_df, ggplot2::aes(x = longitude, y = latitude, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(data = prep$dist_map, fill = NA, colour = "white", linewidth = 0.12, inherit.aes = FALSE) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, name = "Mean") +
    add_malawi_label(cfg, size = 2.2) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Latent field mean (logit scale)") +
    map_theme(8)
  p_sd <- ggplot2::ggplot(sd_df, ggplot2::aes(x = longitude, y = latitude, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(data = prep$dist_map, fill = NA, colour = "white", linewidth = 0.12, inherit.aes = FALSE) +
    ggplot2::scale_fill_gradient(low = "white", high = "#54278F", name = "SD") +
    add_malawi_label(cfg, size = 2.2) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Latent field posterior SD") +
    map_theme(8)
  patchwork::wrap_plots(p_mean, p_sd, guides = "collect")
}

plot_decision_maps <- function(decision_index, prep, cfg) {
  dat <- purrr::map_dfr(seq_len(nrow(decision_index)), function(i) {
    raster_to_data_frame(
      decision_index$raster_file[[i]],
      "decision_code",
      year = decision_index$year[[i]]
    )
  }) |>
    dplyr::mutate(
      decision = factor(
        decision_code,
        levels = c(1, 2, 3),
        labels = c("No", "Uncertain", "Yes")
      )
    )
  ggplot2::ggplot(dat, ggplot2::aes(x = longitude, y = latitude, fill = decision)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(
      data = prep$dist_map,
      fill = NA,
      colour = "grey35",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~year, ncol = 3) +
    ggplot2::scale_fill_manual(
      values = c("No" = "#08306B", "Uncertain" = "#FFF7BC", "Yes" = "#FE9929"),
      drop = FALSE,
      name = "Decision"
    ) +
    add_malawi_label(cfg, size = 2.4) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    map_theme()
}

plot_exceedance_all_years <- function(exceedance_index, prep, cfg) {
  dat <- purrr::map_dfr(seq_len(nrow(exceedance_index)), function(i) {
    row <- exceedance_index[i, ]
    raster_to_data_frame(
      row$raster_file[[1]],
      "probability",
      year = row$year[[1]],
      threshold = paste0(round(100 * row$threshold[[1]]), "%")
    )
  }) |>
    dplyr::mutate(
      threshold = factor(threshold, levels = paste0(round(100 * cfg$thresholds), "%"))
    )
  ggplot2::ggplot(dat, ggplot2::aes(x = longitude, y = latitude, fill = probability)) +
    ggplot2::geom_raster() +
    ggplot2::geom_sf(
      data = prep$dist_map,
      fill = NA,
      colour = "grey50",
      linewidth = 0.09,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(year ~ threshold) +
    ggplot2::scale_fill_gradientn(
      colours = c("#08306B", "#4292C6", "#F7FCB9", "#FDAE6B", "#A50F15"),
      limits = c(0, 1),
      labels = scales::percent_format(),
      name = "Exceedance\nprobability"
    ) +
    add_malawi_label(cfg, size = 1.7) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    map_theme(7) +
    ggplot2::theme(legend.position = "right")
}

plot_district_pfpr <- function(district_continuous, prep, cfg) {
  sf_data <- prep$dist_map |>
    dplyr::select(Province, District, geometry) |>
    dplyr::left_join(district_continuous, by = c("Province", "District"))
  ggplot2::ggplot(sf_data, ggplot2::aes(fill = district_pfpr_mean)) +
    ggplot2::geom_sf(colour = "grey30", linewidth = 0.12) +
    ggplot2::facet_wrap(~year, ncol = 3) +
    pfpr_fill_scale("District mean PfPR") +
    add_malawi_label(cfg, size = 2.5) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    map_theme()
}

plot_district_decisions <- function(district_decisions, prep, cfg) {
  sf_data <- prep$dist_map |>
    dplyr::select(Province, District, geometry) |>
    dplyr::left_join(district_decisions, by = c("Province", "District")) |>
    dplyr::mutate(mean_cell_exceedance_class = factor(mean_cell_exceedance_class, levels = c("No", "Uncertain", "Yes")))
  ggplot2::ggplot(sf_data, ggplot2::aes(fill = mean_cell_exceedance_class)) +
    ggplot2::geom_sf(colour = "grey30", linewidth = 0.12) +
    ggplot2::facet_wrap(~year, ncol = 3) +
    ggplot2::scale_fill_manual(
      values = c("No" = "#08306B", "Uncertain" = "#FFF7BC", "Yes" = "#FE9929"),
      drop = FALSE,
      name = "Mean-cell class"
    ) +
    add_malawi_label(cfg, size = 2.5) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    map_theme()
}
