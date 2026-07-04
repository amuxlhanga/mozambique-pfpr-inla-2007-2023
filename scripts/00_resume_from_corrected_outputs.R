# Recommended after applying the v1.1.0 patch to a project that already has
# completed model fitting and validation objects. This regenerates every
# corrected manuscript figure, table, raster index and district result without
# rerunning INLA model fitting or cross-validation.
required <- c(
  "outputs/model/01_prepared_data.rds",
  "outputs/model/03_covariate_screening.rds",
  "outputs/model/04_final_pooled_inla_spde_model.rds",
  "outputs/model/06_validation_results.rds"
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(
    "Cannot resume because these saved objects are missing:\n",
    paste0("  - ", missing, collapse = "\n"),
    "\nRun source(\"scripts/00_run_all.R\") instead.",
    call. = FALSE
  )
}
source("scripts/00_resume_from_figures.R")
