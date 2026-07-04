# Resume from stage 06 after stages 01-05 have completed.
stages <- c(
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
message("\nStages 06-11 completed.")
