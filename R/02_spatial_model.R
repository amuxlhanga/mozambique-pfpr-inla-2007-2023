km_to_degrees <- function(km) km / 111.32

build_mesh_candidate <- function(coords, boundary, spec) {
  INLA::inla.mesh.2d(
    loc = coords,
    boundary = boundary,
    max.edge = km_to_degrees(c(spec$inner_km, spec$outer_km)),
    offset = km_to_degrees(spec$offset_km),
    cutoff = km_to_degrees(spec$cutoff_km)
  )
}

build_spatial_objects <- function(prep, cfg) {
  moz_sp <- methods::as(prep$moz_map, "Spatial")
  boundary <- INLA::inla.sp2segment(moz_sp)

  meshes <- lapply(
    cfg$mesh$candidates,
    function(spec) build_mesh_candidate(prep$coords_obs, boundary, spec)
  )
  mesh <- meshes[[cfg$mesh$selected]]
  if (is.null(mesh)) stop("Selected mesh not found: ", cfg$mesh$selected)

  spde <- INLA::inla.spde2.matern(mesh, alpha = 2)
  sidx <- INLA::inla.spde.make.index("s", n.spde = spde$n.spde)
  A_obs <- INLA::inla.spde.make.A(mesh, loc = prep$coords_obs)
  A_grid <- INLA::inla.spde.make.A(mesh, loc = prep$coords_grid)

  list(
    boundary = boundary,
    meshes = meshes,
    mesh = mesh,
    spde = spde,
    sidx = sidx,
    A_obs = A_obs,
    A_grid = A_grid
  )
}

make_model_formula <- function(z_covariates, spde) {
  rhs <- paste(
    c("Intercept", z_covariates, "f(year_id, model = 'iid')", "f(s, model = spde)"),
    collapse = " + "
  )
  fml <- stats::as.formula(paste0("y ~ -1 + ", rhs))
  environment(fml) <- environment()
  fml
}

fit_inla_stack <- function(stack, formula, compute_predictions = FALSE) {
  dat <- INLA::inla.stack.data(stack)
  A <- INLA::inla.stack.A(stack)

  # Current R-INLA versions require Ntrials to be supplied as an evaluated
  # vector or a simple symbol. Passing dat$Ntrials directly can be expanded to
  # NULL by INLA's argument parser, which silently reverts the binomial size to
  # one and makes count responses greater than one invalid.
  if (!"Ntrials" %in% names(dat)) {
    stop("The INLA stack does not contain Ntrials.", call. = FALSE)
  }
  Ntrials <- as.numeric(dat[["Ntrials"]])
  y <- as.numeric(dat[["y"]])

  if (length(Ntrials) != length(y)) {
    stop("Ntrials and y have different lengths in the INLA stack.", call. = FALSE)
  }
  observed <- is.finite(y)
  invalid <- observed & (
    !is.finite(Ntrials) | Ntrials <= 0 | y < 0 | y > Ntrials
  )
  if (any(invalid)) {
    stop(
      "Invalid binomial stack: observed y must satisfy 0 <= y <= Ntrials and Ntrials > 0.",
      call. = FALSE
    )
  }
  # Prediction rows have y = NA. A dummy positive number of trials is required
  # for those rows even though it does not enter the likelihood.
  if (any(!observed & (!is.finite(Ntrials) | Ntrials <= 0))) {
    stop("Prediction rows must have a positive dummy Ntrials value.", call. = FALSE)
  }

  INLA::inla(
    formula,
    family = "binomial",
    Ntrials = Ntrials,
    data = dat,
    control.predictor = list(
      A = A,
      compute = compute_predictions,
      link = 1
    ),
    control.compute = list(
      waic = TRUE,
      dic = TRUE,
      cpo = FALSE,
      return.marginals = TRUE,
      return.marginals.predictor = FALSE
    ),
    control.inla = list(strategy = "gaussian", int.strategy = "eb"),
    verbose = FALSE
  )
}

fit_observation_model <- function(prep, spatial, z_covariates) {
  effects <- prep$X_obs[, c("Intercept", z_covariates, "year_id"), drop = FALSE]
  stack <- INLA::inla.stack(
    tag = "est",
    data = list(y = prep$obs$pf_pos, Ntrials = prep$obs$examined),
    A = list(spatial$A_obs, 1),
    effects = list(spatial$sidx, effects)
  )
  formula <- make_model_formula(z_covariates, spatial$spde)
  fit <- fit_inla_stack(stack, formula, compute_predictions = FALSE)
  list(fit = fit, stack = stack, formula = formula)
}

base_covariate_name <- function(z_name) {
  sub("_lag[0-3]$", "", sub("^z_", "", z_name))
}

group_for_covariate <- function(z_name, groups) {
  base <- base_covariate_name(z_name)
  hit <- names(groups)[vapply(groups, function(x) base %in% x, logical(1))]
  if (length(hit)) hit[[1L]] else NA_character_
}

screen_one_covariate <- function(z_name, prep, spatial, min_rows = 200L) {
  if (!z_name %in% names(prep$X_obs)) {
    return(tibble::tibble(var = z_name, WAIC = NA_real_, DIC = NA_real_, n = 0L, status = "missing", error_message = NA_character_))
  }
  keep <- is.finite(prep$X_obs[[z_name]]) &
    prep$obs$examined > 0L &
    prep$obs$pf_pos >= 0L &
    prep$obs$pf_pos <= prep$obs$examined &
    !is.na(prep$obs$year_id)

  if (sum(keep) < min_rows) {
    return(tibble::tibble(var = z_name, WAIC = NA_real_, DIC = NA_real_, n = sum(keep), status = "too_few_rows", error_message = NA_character_))
  }

  effects <- prep$X_obs[keep, c("Intercept", z_name, "year_id"), drop = FALSE]
  stack <- INLA::inla.stack(
    tag = "est",
    data = list(
      y = prep$obs$pf_pos[keep],
      Ntrials = prep$obs$examined[keep]
    ),
    A = list(spatial$A_obs[keep, , drop = FALSE], 1),
    effects = list(spatial$sidx, effects)
  )
  formula <- make_model_formula(z_name, spatial$spde)

  fit <- tryCatch(
    fit_inla_stack(stack, formula, compute_predictions = FALSE),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(tibble::tibble(
      var = z_name,
      WAIC = NA_real_,
      DIC = NA_real_,
      n = sum(keep),
      status = "inla_error",
      error_message = conditionMessage(fit)
    ))
  }

  tibble::tibble(
    var = z_name,
    WAIC = fit$waic$waic,
    DIC = fit$dic$dic,
    n = sum(keep),
    status = "ok",
    error_message = NA_character_
  )
}

run_covariate_screening <- function(prep, spatial, cfg) {
  z_all <- paste0("z_", prep$covariates_all)
  results <- purrr::map_dfr(z_all, screen_one_covariate, prep = prep, spatial = spatial) |>
    dplyr::mutate(
      raw_covariate = sub("^z_", "", var),
      base = base_covariate_name(var),
      lag = dplyr::if_else(
        stringr::str_detect(var, "_lag[0-3]$"),
        as.integer(stringr::str_extract(var, "[0-3]$")),
        NA_integer_
      ),
      group = vapply(var, group_for_covariate, character(1), groups = cfg$thematic_groups),
      retained_in_manuscript = raw_covariate %in% cfg$final_covariates_raw
    ) |>
    dplyr::arrange(WAIC)

  best_lags <- results |>
    dplyr::filter(base %in% c("TSI", "HSI"), is.finite(WAIC)) |>
    dplyr::group_by(base) |>
    dplyr::slice_min(WAIC, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  list(all = results, best_lags = best_lags)
}

fit_final_manuscript_model <- function(prep, spatial, cfg) {
  z_final <- paste0("z_", cfg$final_covariates_raw)
  missing <- setdiff(z_final, names(prep$X_obs))
  if (length(missing)) stop("Final model design variables missing: ", paste(missing, collapse = ", "))

  X_est <- prep$X_obs[, c("Intercept", z_final, "year_id"), drop = FALSE]
  X_pred <- prep$X_grid[, c("Intercept", z_final, "year_id"), drop = FALSE]

  stack_est <- INLA::inla.stack(
    tag = "est",
    data = list(y = prep$obs$pf_pos, Ntrials = prep$obs$examined),
    A = list(spatial$A_obs, 1),
    effects = list(spatial$sidx, X_est)
  )
  stack_pred <- INLA::inla.stack(
    tag = "pred",
    data = list(
      y = rep(NA_integer_, nrow(prep$grid)),
      Ntrials = rep(1L, nrow(prep$grid))
    ),
    A = list(spatial$A_grid, 1),
    effects = list(spatial$sidx, X_pred)
  )
  stack_all <- INLA::inla.stack(stack_est, stack_pred)
  formula <- make_model_formula(z_final, spatial$spde)
  fit <- fit_inla_stack(stack_all, formula, compute_predictions = TRUE)

  list(
    fit = fit,
    stack = stack_all,
    formula = formula,
    z_covariates = z_final,
    raw_covariates = cfg$final_covariates_raw,
    spatial = spatial
  )
}

extract_fixed_effects <- function(model_bundle) {
  model_bundle$fit$summary.fixed |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      term,
      covariate = clean_term_label(term),
      posterior_mean = mean,
      posterior_sd = sd,
      lower_95 = `0.025quant`,
      upper_95 = `0.975quant`,
      credible_interval = sprintf("(%.3f, %.3f)", lower_95, upper_95)
    )
}

extract_year_effects <- function(model_bundle, prep, cfg) {
  yr <- model_bundle$fit$summary.random$year_id
  if (is.null(yr)) stop("Year random effect was not found in fitted model.")
  out <- yr |>
    as.data.frame() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      year_id = dplyr::row_number(),
      year = cfg$survey_years[year_id]
    ) |>
    dplyr::select(year, year_id, dplyr::everything())

  base_id <- match(cfg$reference_year, cfg$survey_years)
  base_mean <- out$mean[out$year_id == base_id]
  out |>
    dplyr::mutate(delta_from_reference = mean - base_mean)
}

extract_spatial_hyperparameters <- function(model_bundle) {
  transformed <- INLA::inla.spde2.result(
    model_bundle$fit,
    "s",
    model_bundle$spatial$spde,
    do.transf = TRUE
  )
  variance_mean <- INLA::inla.emarginal(function(x) x, transformed$marginals.var[[1]])
  marginal_sd_mean <- INLA::inla.emarginal(sqrt, transformed$marginals.var[[1]])
  kappa_mean <- INLA::inla.emarginal(function(x) x, transformed$marginals.kap[[1]])
  range_degrees_mean <- INLA::inla.emarginal(function(x) x, transformed$marginals.range[[1]])
  summary <- tibble::tibble(
    parameter = c("variance", "marginal_sd", "kappa", "range_degrees", "range_km_approx"),
    posterior_mean = c(
      variance_mean, marginal_sd_mean, kappa_mean,
      range_degrees_mean, 111.32 * range_degrees_mean
    )
  )
  list(transformed = transformed, summary = summary)
}

final_model_selection_evidence <- function(screening, cfg) {
  retained <- screening$all |>
    dplyr::filter(.data$raw_covariate %in% cfg$final_covariates_raw)

  missing <- setdiff(cfg$final_covariates_raw, retained$raw_covariate)
  if (length(missing)) {
    stop(
      "Final-model covariates are missing from the screening results: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  family_best <- screening$all |>
    dplyr::filter(.data$status == "ok", is.finite(.data$WAIC)) |>
    dplyr::group_by(.data$base) |>
    dplyr::summarise(best_family_WAIC = min(.data$WAIC), .groups = "drop")

  retained |>
    dplyr::left_join(family_best, by = "base") |>
    dplyr::mutate(
      order = match(.data$raw_covariate, cfg$final_covariates_raw),
      covariate = clean_term_label(paste0("z_", .data$raw_covariate)),
      thematic_domain = dplyr::case_when(
        .data$base %in% c("TSI", "HSI") ~ "Mechanistic suitability index",
        !is.na(.data$group) ~ stringr::str_to_sentence(stringr::str_replace_all(.data$group, "_", " ")),
        TRUE ~ "Other"
      ),
      best_supported_lag_within_family = dplyr::if_else(
        is.na(.data$lag),
        NA,
        abs(.data$WAIC - .data$best_family_WAIC) < 1e-8
      ),
      selection_basis = dplyr::case_when(
        .data$base %in% c("TSI", "HSI") ~
          "Mechanistically motivated index retained after lag screening",
        TRUE ~
          "Retained after univariate spatial screening, thematic grouping, correlation/VIF assessment and biological interpretation"
      )
    ) |>
    dplyr::arrange(.data$order) |>
    dplyr::transmute(
      order,
      covariate,
      raw_covariate,
      thematic_domain,
      lag_months = lag,
      univariate_WAIC = WAIC,
      univariate_DIC = DIC,
      screening_status = status,
      best_supported_lag_within_family,
      selection_basis
    )
}
