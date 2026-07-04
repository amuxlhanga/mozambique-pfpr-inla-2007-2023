# Resume after stages 01-06 have completed successfully.
scripts <- c(
  "scripts/07_prediction_surfaces.R",
  "scripts/08_exceedance_decision.R",
  "scripts/09_district_aggregation.R",
  "scripts/10_results_summary.R",
  "scripts/11_output_audit.R"
)
for (script in scripts) {
  message("\n===== Running ", script, " =====")
  source(script, local = new.env(parent = globalenv()))
}
