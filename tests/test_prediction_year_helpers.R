# Regression test for year-specific prediction filtering.
source(file.path("R", "04_prediction.R"))

example <- data.frame(
  year = c(2007L, 2007L, 2011L, 2015L, 2015L, 2023L),
  value = seq_len(6)
)

selected_2007 <- filter_year_rows(example, 2007L)
selected_2015 <- filter_year_rows(example, 2015L)

stopifnot(
  nrow(selected_2007) == 2L,
  identical(selected_2007$value, c(1L, 2L)),
  nrow(selected_2015) == 2L,
  identical(selected_2015$value, c(4L, 5L)),
  nrow(filter_year_rows(example, 2022L)) == 0L
)

factor_example <- transform(example, year = factor(year))
stopifnot(nrow(filter_year_rows(factor_example, 2011L)) == 1L)

cat("PREDICTION YEAR FILTER TEST PASSED\n")
