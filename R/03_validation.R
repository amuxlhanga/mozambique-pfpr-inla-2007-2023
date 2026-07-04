validation_metrics <- function(df, observed = "obs_pfpr", predicted = "pred_pfpr") {
  obs <- df[[observed]]
  pred <- df[[predicted]]
  err <- pred - obs
  tibble::tibble(
    N = sum(is.finite(obs) & is.finite(pred)),
    r = safe_cor(obs, pred),
    RMSE = sqrt(mean(err^2, na.rm = TRUE)),
    MAE = mean(abs(err), na.rm = TRUE),
    Bias = mean(err, na.rm = TRUE)
  )
}

filter_calibration_rows <- function(df) {
  required <- c("pred_pfpr", "examined", "pf_pos")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(
      "Calibration data are missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  df |>
    dplyr::filter(
      is.finite(.data$pred_pfpr),
      .data$pred_pfpr >= 0,
      .data$pred_pfpr <= 1,
      is.finite(.data$examined),
      .data$examined > 0,
      is.finite(.data$pf_pos),
      .data$pf_pos >= 0,
      .data$pf_pos <= .data$examined
    )
}

summarise_calibration_groups <- function(df, grouping_vars) {
  grouping_vars <- as.character(grouping_vars)
  missing_groups <- setdiff(grouping_vars, names(df))
  if (length(missing_groups)) {
    stop(
      "Calibration grouping columns are missing: ",
      paste(missing_groups, collapse = ", "),
      call. = FALSE
    )
  }

  clean <- filter_calibration_rows(df)
  if (!nrow(clean)) {
    stop("No valid observations remain for calibration summaries.", call. = FALSE)
  }

  clean |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
    dplyr::summarise(
      n_clusters = dplyr::n(),
      mean_pred = stats::weighted.mean(
        x = .data$pred_pfpr,
        w = .data$examined,
        na.rm = TRUE
      ),
      examined_total = sum(.data$examined, na.rm = TRUE),
      positive_total = sum(.data$pf_pos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      examined = .data$examined_total,
      positive = .data$positive_total,
      mean_obs = .data$positive / .data$examined,
      se_obs = sqrt(pmax(.data$mean_obs * (1 - .data$mean_obs) / pmax(.data$examined, 1), 0)),
      lower = pmax(.data$mean_obs - 1.96 * .data$se_obs, 0),
      upper = pmin(.data$mean_obs + 1.96 * .data$se_obs, 1)
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        grouping_vars,
        "n_clusters", "examined", "positive", "mean_pred",
        "mean_obs", "se_obs", "lower", "upper"
      ))
    )
}


make_decile_calibration <- function(predictions, by_year = FALSE) {
  clean <- filter_calibration_rows(predictions)
  if (isTRUE(by_year)) {
    clean |>
      dplyr::group_by(.data$year) |>
      dplyr::mutate(decile = dplyr::ntile(.data$pred_pfpr, 10)) |>
      dplyr::ungroup() |>
      summarise_calibration_groups(c("year", "decile"))
  } else {
    clean |>
      dplyr::mutate(decile = dplyr::ntile(.data$pred_pfpr, 10)) |>
      summarise_calibration_groups("decile")
  }
}

make_fixed_bin_calibration <- function(predictions, by_year = FALSE) {
  clean <- filter_calibration_rows(predictions) |>
    dplyr::mutate(
      probability_bin = cut(
        .data$pred_pfpr,
        breaks = seq(0, 1, by = 0.10),
        include.lowest = TRUE,
        right = TRUE
      )
    ) |>
    dplyr::filter(!is.na(.data$probability_bin))

  if (isTRUE(by_year)) {
    summarise_calibration_groups(clean, c("year", "probability_bin"))
  } else {
    summarise_calibration_groups(clean, "probability_bin")
  }
}

augment_validation_bundle <- function(validation) {
  validation$internal$calibration_by_year <- make_decile_calibration(
    validation$internal$predictions,
    by_year = TRUE
  )
  validation$internal$calibration_overall <- make_decile_calibration(
    validation$internal$predictions,
    by_year = FALSE
  )
  validation$internal$calibration <- validation$internal$calibration_by_year

  for (name in intersect(c("random", "spatial"), names(validation))) {
    method <- validation[[name]]$overall$validation[[1]]
    predictions <- validation[[name]]$predictions
    validation[[name]]$calibration_deciles <- make_decile_calibration(predictions, by_year = TRUE) |>
      dplyr::mutate(validation = method, .before = 1)
    validation[[name]]$calibration_overall <- make_decile_calibration(predictions, by_year = FALSE) |>
      dplyr::mutate(validation = method, .before = 1)
    validation[[name]]$calibration_fixed_bins_by_year <- make_fixed_bin_calibration(predictions, by_year = TRUE) |>
      dplyr::mutate(validation = method, .before = 1)
    validation[[name]]$calibration_fixed_bins <- make_fixed_bin_calibration(predictions, by_year = FALSE) |>
      dplyr::mutate(validation = method, .before = 1)
  }
  validation
}

extract_internal_validation <- function(model_bundle, prep) {
  idx <- INLA::inla.stack.index(model_bundle$stack, "est")$data
  fitted <- model_bundle$fit$summary.fitted.values[idx, , drop = FALSE]
  predictions <- prep$obs |>
    dplyr::transmute(
      site_id,
      year = year_end,
      year_factor,
      longitude,
      latitude,
      examined,
      pf_pos,
      obs_pfpr = pf_pos / examined,
      pred_pfpr = pmin(pmax(fitted[, "mean"], 0), 1),
      pred_sd = fitted[, "sd"],
      pred_lower = pmin(pmax(fitted[, "0.025quant"], 0), 1),
      pred_upper = pmin(pmax(fitted[, "0.975quant"], 0), 1)
    )

  overall <- validation_metrics(predictions) |>
    dplyr::mutate(validation = "Internal fitted values", .before = 1)
  by_year <- predictions |>
    dplyr::group_by(year) |>
    dplyr::group_modify(~ validation_metrics(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(validation = "Internal fitted values", .before = 1)

  calibration_by_year <- make_decile_calibration(predictions, by_year = TRUE)
  calibration_overall <- make_decile_calibration(predictions, by_year = FALSE)

  list(
    predictions = predictions,
    overall = overall,
    by_year = by_year,
    calibration = calibration_by_year,
    calibration_by_year = calibration_by_year,
    calibration_overall = calibration_overall
  )
}

make_random_folds <- function(obs, k, seed) {
  set.seed(seed)
  obs |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::group_by(year_end) |>
    dplyr::mutate(fold = sample(rep(seq_len(k), length.out = dplyr::n()))) |>
    dplyr::ungroup() |>
    dplyr::arrange(row_id) |>
    dplyr::pull(fold)
}

make_spatial_block_folds <- function(obs, k, block_km, seed) {
  points <- sf::st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    sf::st_transform(32736)
  block_m <- block_km * 1000
  bbox_poly <- sf::st_as_sfc(sf::st_bbox(points)) |>
    sf::st_buffer(dist = block_m * 0.25)
  polygons <- sf::st_make_grid(
    bbox_poly,
    cellsize = c(block_m, block_m),
    what = "polygons",
    square = TRUE
  )
  blocks_sf <- sf::st_sf(block_id = seq_along(polygons), geometry = polygons)
  joined <- sf::st_join(points, blocks_sf, join = sf::st_within, left = TRUE)

  missing <- which(is.na(joined$block_id))
  if (length(missing)) {
    nearest <- sf::st_nearest_feature(joined[missing, ], blocks_sf)
    joined$block_id[missing] <- blocks_sf$block_id[nearest]
  }

  counts <- as.data.frame(table(joined$block_id), stringsAsFactors = FALSE)
  names(counts) <- c("block_id", "n")
  counts$block_id <- as.integer(as.character(counts$block_id))
  counts$n <- as.integer(counts$n)
  set.seed(seed)
  counts$tie <- stats::runif(nrow(counts))
  counts <- counts[order(-counts$n, counts$tie), ]

  fold_load <- rep(0L, k)
  assignment <- integer(nrow(counts))
  for (i in seq_len(nrow(counts))) {
    eligible <- which(fold_load == min(fold_load))
    chosen <- sample(eligible, 1L)
    assignment[i] <- chosen
    fold_load[chosen] <- fold_load[chosen] + counts$n[i]
  }
  names(assignment) <- counts$block_id
  fold <- unname(assignment[as.character(joined$block_id)])

  list(
    fold = fold,
    block_id = joined$block_id,
    blocks = blocks_sf,
    fold_load = fold_load
  )
}

scale_fold_data <- function(train, test, raw_covariates) {
  mu <- vapply(train[raw_covariates], mean, numeric(1), na.rm = TRUE)
  sigma <- vapply(train[raw_covariates], stats::sd, numeric(1), na.rm = TRUE)
  sigma[!is.finite(sigma) | sigma == 0] <- 1

  transform <- function(df) {
    z <- sweep(sweep(as.matrix(df[raw_covariates]), 2, mu, "-"), 2, sigma, "/")
    z[!is.finite(z)] <- 0
    z <- as.data.frame(z)
    names(z) <- paste0("z_", raw_covariates)
    z
  }
  list(train = transform(train), test = transform(test))
}

run_cross_validation <- function(prep, spatial, raw_covariates, folds, method, cfg) {
  k <- max(folds)
  z_covariates <- paste0("z_", raw_covariates)
  formula <- make_model_formula(z_covariates, spatial$spde)
  fold_predictions <- vector("list", k)

  for (fold_id in seq_len(k)) {
    train_index <- folds != fold_id
    test_index <- folds == fold_id
    train <- prep$obs[train_index, , drop = FALSE]
    test <- prep$obs[test_index, , drop = FALSE]

    scaled <- scale_fold_data(train, test, raw_covariates)
    X_train <- cbind(Intercept = 1, scaled$train)
    X_test <- cbind(Intercept = 1, scaled$test)
    X_train$year_id <- train$year_id
    X_test$year_id <- test$year_id
    X_train <- X_train[, c("Intercept", z_covariates, "year_id"), drop = FALSE]
    X_test <- X_test[, c("Intercept", z_covariates, "year_id"), drop = FALSE]

    A_train <- INLA::inla.spde.make.A(
      spatial$mesh,
      loc = as.matrix(train[, c("longitude", "latitude")])
    )
    A_test <- INLA::inla.spde.make.A(
      spatial$mesh,
      loc = as.matrix(test[, c("longitude", "latitude")])
    )

    stack_est <- INLA::inla.stack(
      tag = "est",
      data = list(y = train$pf_pos, Ntrials = train$examined),
      A = list(A_train, 1),
      effects = list(spatial$sidx, X_train)
    )
    stack_pred <- INLA::inla.stack(
      tag = "pred",
      data = list(y = rep(NA_integer_, nrow(test)), Ntrials = rep(1L, nrow(test))),
      A = list(A_test, 1),
      effects = list(spatial$sidx, X_test)
    )
    stack <- INLA::inla.stack(stack_est, stack_pred)
    fit <- fit_inla_stack(stack, formula, compute_predictions = TRUE)
    idx <- INLA::inla.stack.index(stack, "pred")$data
    fitted <- fit$summary.fitted.values[idx, , drop = FALSE]

    fold_predictions[[fold_id]] <- test |>
      dplyr::transmute(
        validation = method,
        fold = fold_id,
        site_id,
        year = year_end,
        year_factor,
        longitude,
        latitude,
        examined,
        pf_pos,
        obs_pfpr = pf_pos / examined,
        pred_pfpr = pmin(pmax(fitted[, "mean"], 0), 1),
        pred_sd = fitted[, "sd"],
        pred_lower = pmin(pmax(fitted[, "0.025quant"], 0), 1),
        pred_upper = pmin(pmax(fitted[, "0.975quant"], 0), 1)
      )
  }

  predictions <- dplyr::bind_rows(fold_predictions)
  overall <- validation_metrics(predictions) |>
    dplyr::mutate(validation = method, .before = 1)
  by_fold <- predictions |>
    dplyr::group_by(fold) |>
    dplyr::group_modify(~ validation_metrics(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(validation = method, .before = 1)
  by_year <- predictions |>
    dplyr::group_by(year) |>
    dplyr::group_modify(~ validation_metrics(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(validation = method, .before = 1)

  calibration_deciles <- make_decile_calibration(predictions, by_year = TRUE) |>
    dplyr::mutate(validation = method, .before = 1)

  calibration_overall <- make_decile_calibration(predictions, by_year = FALSE) |>
    dplyr::mutate(validation = method, .before = 1)

  calibration_fixed_bins_by_year <- make_fixed_bin_calibration(predictions, by_year = TRUE) |>
    dplyr::mutate(validation = method, .before = 1)

  calibration_fixed_bins <- make_fixed_bin_calibration(predictions, by_year = FALSE) |>
    dplyr::mutate(validation = method, .before = 1)

  list(
    predictions = predictions,
    overall = overall,
    by_fold = by_fold,
    by_year = by_year,
    calibration_deciles = calibration_deciles,
    calibration_overall = calibration_overall,
    calibration_fixed_bins_by_year = calibration_fixed_bins_by_year,
    calibration_fixed_bins = calibration_fixed_bins
  )
}
