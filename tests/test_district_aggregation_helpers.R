# Synthetic regression test for area-weighted district extraction and the
# documented nearest-valid-cell fallback used for small island districts.
source(file.path("scripts", "_bootstrap.R"))

r <- terra::rast(
  nrows = 1, ncols = 3,
  xmin = 0, xmax = 3, ymin = 0, ymax = 1,
  crs = "EPSG:4326"
)
terra::values(r) <- c(0.10, NA_real_, 0.30)
names(r) <- "pfpr_mean"

make_polygon <- function(xmin, xmax, ymin = 0.1, ymax = 0.9) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2, byrow = TRUE
  )))
}

districts <- sf::st_sf(
  Province = c("P", "P"),
  District = c("AreaWeighted", "Fallback"),
  geometry = sf::st_sfc(
    make_polygon(0.05, 0.95),
    make_polygon(1.15, 1.35),
    crs = 4326
  )
)

result <- extract_polygon_means_with_fallback(r, districts)
stopifnot(
  nrow(result) == 2L,
  isTRUE(all.equal(result$pfpr_mean[result$District == "AreaWeighted"], 0.10)),
  result$aggregation_method[result$District == "AreaWeighted"] == "area_weighted_exact",
  is.finite(result$pfpr_mean[result$District == "Fallback"]),
  result$aggregation_method[result$District == "Fallback"] == "nearest_valid_cell_fallback",
  is.finite(result$fallback_distance_km[result$District == "Fallback"])
)
message("DISTRICT AGGREGATION HELPER TEST PASSED")
