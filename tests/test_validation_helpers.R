# Runtime regression test for the calibration aggregation used in stage 06.
# Open the R project and run:
# source("tests/test_validation_helpers.R")

source(file.path("scripts", "_bootstrap.R"))

example <- tibble::tibble(
  year = c(2007L, 2007L, 2011L, 2011L),
  decile = c(1L, 1L, 1L, 1L),
  pred_pfpr = c(0.10, 0.30, 0.20, 0.40),
  examined = c(10L, 30L, 20L, 20L),
  pf_pos = c(1L, 9L, 4L, 8L)
)

result <- summarise_calibration_groups(example, c("year", "decile"))

stopifnot(
  nrow(result) == 2L,
  isTRUE(all.equal(result$mean_pred[result$year == 2007L], 0.25)),
  isTRUE(all.equal(result$mean_obs[result$year == 2007L], 0.25)),
  isTRUE(all.equal(result$mean_pred[result$year == 2011L], 0.30)),
  isTRUE(all.equal(result$mean_obs[result$year == 2011L], 0.30))
)

decile_overall <- make_decile_calibration(example, by_year = FALSE)
fixed_by_year <- make_fixed_bin_calibration(example, by_year = TRUE)
stopifnot(
  nrow(decile_overall) >= 1L,
  all(c("year", "probability_bin", "mean_pred", "mean_obs") %in% names(fixed_by_year))
)

message("VALIDATION HELPER TEST PASSED")
