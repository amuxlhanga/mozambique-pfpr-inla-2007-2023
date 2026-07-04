cran_packages <- c(
  "car", "dplyr", "ggplot2", "patchwork", "purrr",
  "readr", "rlang", "scales", "sf", "sp", "stringr", "terra", "tibble", "tidyr"
)
missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable")
  )
}
message("Package installation check complete.")
