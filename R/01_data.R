prepare_analysis_data <- function(cfg) {
  assert_files_exist(unlist(cfg$files, use.names = FALSE))

  obs <- readr::read_csv(cfg$files$observations, show_col_types = FALSE)
  grid <- readr::read_csv(cfg$files$prediction_grid, show_col_types = FALSE)

  required_obs <- c("site_id", "longitude", "latitude", "pf_pos", "examined", "year_end")
  required_grid <- c("longitude", "latitude", "year_anchor", "month_anchor")
  missing_obs <- setdiff(required_obs, names(obs))
  missing_grid <- setdiff(required_grid, names(grid))
  if (length(missing_obs)) stop("Observation data missing: ", paste(missing_obs, collapse = ", "))
  if (length(missing_grid)) stop("Prediction grid missing: ", paste(missing_grid, collapse = ", "))

  obs <- obs |>
    dplyr::mutate(
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      pf_pos = as.integer(pf_pos),
      examined = as.integer(examined),
      year_end = as.integer(year_end),
      month_end = if ("month_end" %in% names(obs)) as.integer(month_end) else NA_integer_
    ) |>
    dplyr::filter(
      is.finite(longitude), is.finite(latitude),
      !is.na(year_end), examined > 0L,
      pf_pos >= 0L, pf_pos <= examined
    )

  grid <- grid |>
    dplyr::mutate(
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      year_anchor = as.integer(year_anchor),
      month_anchor = as.integer(month_anchor)
    ) |>
    dplyr::filter(is.finite(longitude), is.finite(latitude))

  missing_years_obs <- setdiff(cfg$survey_years, sort(unique(obs$year_end)))
  missing_years_grid <- setdiff(cfg$survey_years, sort(unique(grid$year_anchor)))
  if (length(missing_years_obs)) {
    stop("Observation data do not contain configured years: ", paste(missing_years_obs, collapse = ", "))
  }
  if (length(missing_years_grid)) {
    stop("Prediction grid does not contain configured years: ", paste(missing_years_grid, collapse = ", "))
  }

  obs <- obs |> dplyr::filter(year_end %in% cfg$survey_years)
  grid <- grid |> dplyr::filter(year_anchor %in% cfg$survey_years)

  # Explicit transformations used by the manuscript model.
  if ("Access_Cities_min" %in% names(obs)) obs$Access_Cities_log1p <- log1p(obs$Access_Cities_min)
  if ("Access_Health_min" %in% names(obs)) obs$Access_Health_log1p <- log1p(obs$Access_Health_min)
  if ("Access_Cities_min" %in% names(grid)) grid$Access_Cities_log1p <- log1p(grid$Access_Cities_min)
  if ("Access_Health_min" %in% names(grid)) grid$Access_Health_log1p <- log1p(grid$Access_Health_min)

  # Population rasters differ in vintage between the supplied observation and
  # prediction files. The harmonised name is retained only for screening and is
  # not part of the frozen final model.
  if ("WorldPop_2020" %in% names(obs)) obs$WorldPop_static <- obs$WorldPop_2020
  if ("WorldPop_2018" %in% names(grid)) grid$WorldPop_static <- grid$WorldPop_2018

  year_levels <- as.character(cfg$survey_years)
  obs <- obs |>
    dplyr::mutate(
      year_factor = factor(as.character(year_end), levels = year_levels),
      year_id = match(as.character(year_end), year_levels),
      PfPR = pf_pos / examined
    )
  grid <- grid |>
    dplyr::mutate(
      year_factor = factor(as.character(year_anchor), levels = year_levels),
      year_id = match(as.character(year_anchor), year_levels)
    )

  if (anyNA(obs$year_id) || anyNA(grid$year_id)) {
    stop("Could not map all observation or grid years to year_id.")
  }

  dist_map <- sf::st_read(cfg$files$districts, quiet = TRUE) |>
    sf::st_make_valid()
  if (is.na(sf::st_crs(dist_map))) sf::st_crs(dist_map) <- 4326
  dist_map <- sf::st_transform(dist_map, 4326)

  province_col <- find_first_existing(
    dist_map,
    c("Province", "PROVINCE", "province", "ADM1_PT", "ADM1_NAME", "NAME_1")
  )
  district_col <- find_first_existing(
    dist_map,
    c("District", "DISTRICT", "district", "ADM2_PT", "ADM2_NAME", "NAME_2")
  )
  if (is.null(province_col) || is.null(district_col)) {
    stop("District shapefile must contain province and district name fields.")
  }
  dist_map$Province <- as.character(dist_map[[province_col]])
  dist_map$District <- as.character(dist_map[[district_col]])

  moz_map <- dist_map |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_make_valid()

  dynamic <- unlist(
    lapply(
      cfg$dynamic_families,
      function(v) paste0(v, "_lag", cfg$dynamic_lags)
    ),
    use.names = FALSE
  )
  candidates <- unique(c(dynamic, cfg$static_candidates))
  covariates_all <- candidates[candidates %in% names(obs) & candidates %in% names(grid)]

  missing_final <- setdiff(cfg$final_covariates_raw, covariates_all)
  if (length(missing_final)) {
    stop("Final manuscript covariates missing from inputs: ", paste(missing_final, collapse = ", "))
  }

  mu <- vapply(obs[covariates_all], mean, numeric(1), na.rm = TRUE)
  sigma <- vapply(obs[covariates_all], stats::sd, numeric(1), na.rm = TRUE)
  sigma[!is.finite(sigma) | sigma == 0] <- 1

  missingness <- tibble::tibble(
    covariate = covariates_all,
    retained_in_final_model = covariates_all %in% cfg$final_covariates_raw,
    n_missing_obs = vapply(obs[covariates_all], function(x) sum(!is.finite(x)), integer(1)),
    pct_missing_obs = 100 * n_missing_obs / nrow(obs),
    n_missing_grid = vapply(grid[covariates_all], function(x) sum(!is.finite(x)), integer(1)),
    pct_missing_grid = 100 * n_missing_grid / nrow(grid),
    standardisation_mean = unname(mu[covariates_all]),
    standardisation_sd = unname(sigma[covariates_all]),
    imputation_observations = "Observation-data mean (zero after standardisation)",
    imputation_prediction_grid = "Observation-data mean (zero after standardisation)"
  ) |>
    dplyr::arrange(dplyr::desc(retained_in_final_model), covariate)

  scale_matrix <- function(df) {
    z <- sweep(sweep(as.matrix(df[covariates_all]), 2, mu, "-"), 2, sigma, "/")
    # Mean imputation on the standardised scale. This reproduces the previous
    # handling while making the policy explicit and auditable.
    z[!is.finite(z)] <- 0
    z <- as.data.frame(z)
    names(z) <- paste0("z_", covariates_all)
    z
  }

  X_obs <- cbind(Intercept = 1, scale_matrix(obs))
  X_grid <- cbind(Intercept = 1, scale_matrix(grid))
  X_obs$year_id <- obs$year_id
  X_grid$year_id <- grid$year_id

  coords_obs <- as.matrix(obs[, c("longitude", "latitude")])
  coords_grid <- as.matrix(grid[, c("longitude", "latitude")])

  obs_sf <- sf::st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  grid_sf <- sf::st_as_sf(grid, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  inside_grid <- lengths(sf::st_within(grid_sf, moz_map)) > 0L
  grid$inside_moz <- inside_grid

  list(
    obs = obs,
    grid = grid,
    dist_map = dist_map,
    moz_map = moz_map,
    coords_obs = coords_obs,
    coords_grid = coords_grid,
    X_obs = X_obs,
    X_grid = X_grid,
    covariates_all = covariates_all,
    scaling = list(mean = mu, sd = sigma),
    missingness = missingness,
    year_levels = year_levels
  )
}

missingness_table_for_output <- function(prep, cfg) {
  out <- prep$missingness
  if (!"retained_in_final_model" %in% names(out)) {
    out$retained_in_final_model <- out$covariate %in% cfg$final_covariates_raw
  }
  if (!"standardisation_mean" %in% names(out)) {
    out$standardisation_mean <- unname(prep$scaling$mean[out$covariate])
  }
  if (!"standardisation_sd" %in% names(out)) {
    out$standardisation_sd <- unname(prep$scaling$sd[out$covariate])
  }
  out$imputation_observations <- "Observation-data mean (zero after standardisation)"
  out$imputation_prediction_grid <- "Observation-data mean (zero after standardisation)"
  out |>
    dplyr::arrange(dplyr::desc(retained_in_final_model), covariate)
}

write_data_preparation_outputs <- function(prep, cfg) {
  prep$missingness <- missingness_table_for_output(prep, cfg)
  saveRDS(prep, file.path(cfg$dirs$model, "01_prepared_data.rds"), compress = "xz")
  write_csv_checked(prep$missingness, file.path(cfg$dirs$tables_supp, "Table_S_missing_covariate_values.csv"))

  summary_tbl <- prep$obs |>
    dplyr::summarise(
      clusters = dplyr::n(),
      examined = sum(examined),
      positive = sum(pf_pos),
      aggregated_observed_pfpr = positive / examined
    )
  write_csv_checked(summary_tbl, file.path(cfg$dirs$tables_main, "observed_data_summary.csv"))
  invisible(prep)
}
