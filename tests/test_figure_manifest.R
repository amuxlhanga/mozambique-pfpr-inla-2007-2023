# Verify the exact manuscript figure inventory without fitting models.
source(file.path("scripts", "_bootstrap.R"))
manifest <- manuscript_figure_manifest(cfg)
stopifnot(
  nrow(manifest) == 28L,
  sum(manifest$section == "Main") == 5L,
  sum(manifest$section == "Supplementary") == 23L,
  all(manifest$required),
  !anyDuplicated(manifest$figure),
  !anyDuplicated(manifest$relative_path),
  any(manifest$figure == "Figure S9" & grepl("by_year", manifest$relative_path, fixed = TRUE)),
  any(manifest$figure == "Figure S23" & grepl("exceedance", manifest$relative_path, fixed = TRUE))
)
message("FIGURE MANIFEST TEST PASSED: 5 main and 23 supplementary figures configured")
