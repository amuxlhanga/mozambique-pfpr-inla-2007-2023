source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
grid_predictions <- read_rds_required(file.path(cfg$dirs$model, "07_grid_predictions.rds"))

exceedance <- compute_exceedance_outputs(grid_predictions, prep, cfg)
saveRDS(exceedance, file.path(cfg$dirs$model, "08_exceedance_and_decision_outputs.rds"), compress = "gzip")
write_csv_checked(exceedance$exceedance_index, file.path(cfg$dirs$rasters, "exceedance_raster_index.csv"))
write_csv_checked(exceedance$decision_index, file.path(cfg$dirs$rasters, "decision_raster_index.csv"))
write_csv_checked(exceedance$decision_summary, file.path(cfg$dirs$tables_main, "Table_3_decision_summary_pfpr20_p80.csv"))

unlink(c(
  file.path(cfg$dirs$figures_main, "Figure_4_decision_maps_pfpr20_p80.png"),
  file.path(cfg$dirs$figures_main, "Figure_6_decision_maps_pfpr20_p80.png"),
  file.path(cfg$dirs$figures_supp, "Figure_S23_exceedance_probabilities_all_years_thresholds.png"),
  file.path(cfg$dirs$figures_supp, "Figure_S29_exceedance_probabilities_all_years_thresholds.png"),
  file.path(cfg$dirs$figures_supp, "Figure_S_exceedance_probabilities_all_years_thresholds.png")
), force = TRUE)

save_plot(
  plot_decision_maps(exceedance$decision_index, prep, cfg),
  file.path(cfg$dirs$figures_main, "Figure_5_decision_maps_pfpr20_p80.png"),
  width = 10.5, height = 8.0
)
save_plot(
  plot_exceedance_all_years(exceedance$exceedance_index, prep, cfg),
  file.path(cfg$dirs$figures_supp, "Figure_S23_exceedance_probabilities_all_years_thresholds.png"),
  width = 11.5, height = 15.0
)
message("Main Figure 5 and Supplementary Figure S23 written using p*=0.80.")
