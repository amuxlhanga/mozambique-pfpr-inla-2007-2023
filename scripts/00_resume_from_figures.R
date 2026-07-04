# Re-render manuscript figures and all derived tables without refitting the
# final model or rerunning cross-validation. Requires completed stages 01-06.
source(file.path("scripts", "_bootstrap.R"))

unlink(list.files(cfg$dirs$figures_main, pattern = "^Figure_[3-9]_.*\\.png$", full.names = TRUE), force = TRUE)
unlink(list.files(cfg$dirs$figures_supp, pattern = "^Figure_S([7-9]|[12][0-9])_.*\\.png$", full.names = TRUE), force = TRUE)

scripts <- c(
  "scripts/05_posterior_outputs.R",
  "scripts/06_render_validation_outputs.R",
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
