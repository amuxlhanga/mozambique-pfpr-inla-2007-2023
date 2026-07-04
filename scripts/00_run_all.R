# Run from the repository root after opening Ch2_PfPR_reproducible.Rproj.
stages <- c(
  "scripts/01_prepare_data.R",
  "scripts/02_exploratory_outputs.R",
  "scripts/03_mesh_and_screening.R",
  "scripts/04_fit_final_model.R",
  "scripts/05_posterior_outputs.R",
  "scripts/06_validation.R",
  "scripts/07_prediction_surfaces.R",
  "scripts/08_exceedance_decision.R",
  "scripts/09_district_aggregation.R",
  "scripts/10_results_summary.R",
  "scripts/11_output_audit.R"
)

for (stage in stages) {
  message("\n===== Running ", stage, " =====")
  sys.source(stage, envir = new.env(parent = globalenv()))
}
message("\nAll configured stages completed.")
