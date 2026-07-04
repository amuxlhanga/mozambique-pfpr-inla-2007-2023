source(file.path("scripts", "_bootstrap.R"))
validation <- read_rds_required(file.path(cfg$dirs$model, "06_validation_results.rds"))
validation <- augment_validation_bundle(validation)
saveRDS(validation, file.path(cfg$dirs$model, "06_validation_results.rds"), compress = "gzip")

internal <- validation$internal
random_cv <- validation$random
spatial_cv <- validation$spatial

write_csv_checked(internal$predictions, file.path(cfg$dirs$validation, "internal_fitted_predictions.csv"))
write_csv_checked(random_cv$predictions, file.path(cfg$dirs$validation, "random_cv_predictions.csv"))
write_csv_checked(spatial_cv$predictions, file.path(cfg$dirs$validation, "spatial_block_cv_predictions.csv"))

comparison <- dplyr::bind_rows(internal$overall, random_cv$overall, spatial_cv$overall)
write_csv_checked(comparison, file.path(cfg$dirs$tables_main, "Table_2_validation_comparison.csv"))
write_csv_checked(random_cv$by_fold, file.path(cfg$dirs$tables_supp, "random_cv_metrics_by_fold.csv"))
write_csv_checked(random_cv$by_year, file.path(cfg$dirs$tables_supp, "random_cv_metrics_by_year.csv"))
write_csv_checked(spatial_cv$by_fold, file.path(cfg$dirs$tables_supp, "spatial_block_cv_metrics_by_fold.csv"))
write_csv_checked(spatial_cv$by_year, file.path(cfg$dirs$tables_supp, "spatial_block_cv_metrics_by_year.csv"))
write_csv_checked(random_cv$calibration_deciles, file.path(cfg$dirs$tables_supp, "random_cv_calibration_deciles_by_year.csv"))
write_csv_checked(spatial_cv$calibration_deciles, file.path(cfg$dirs$tables_supp, "spatial_block_cv_calibration_deciles_by_year.csv"))
write_csv_checked(random_cv$calibration_overall, file.path(cfg$dirs$tables_supp, "random_cv_calibration_deciles_overall.csv"))
write_csv_checked(spatial_cv$calibration_overall, file.path(cfg$dirs$tables_supp, "spatial_block_cv_calibration_deciles_overall.csv"))
write_csv_checked(spatial_cv$calibration_fixed_bins_by_year, file.path(cfg$dirs$tables_supp, "spatial_block_cv_calibration_fixed_width_bins_by_year.csv"))
write_csv_checked(spatial_cv$calibration_fixed_bins, file.path(cfg$dirs$tables_supp, "spatial_block_cv_calibration_fixed_width_bins_overall.csv"))
write_csv_checked(internal$calibration_by_year, file.path(cfg$dirs$tables_supp, "internal_calibration_deciles_by_year.csv"))
write_csv_checked(internal$calibration_overall, file.path(cfg$dirs$tables_supp, "internal_calibration_deciles_overall.csv"))

legacy <- c(
  "Figure_S_internal_validation_by_year.png",
  "Figure_S_internal_calibration_by_year.png",
  "Figure_S9_internal_calibration_deciles_overall.png",
  "Figure_S_random_cv_scatter_by_fold.png",
  "Figure_S_spatial_block_cv_scatter_by_fold.png",
  "Figure_S_spatial_block_cv_scatter_by_year.png",
  "Figure_S_spatial_block_cv_calibration_by_year.png",
  "Figure_S_spatial_block_cv_calibration_fixed_bins.png",
  "Figure_S_spatial_block_cv_calibration_deciles_overall.png",
  "Figure_S_random_vs_spatial_cv_comparison.png",
  "Figure_5_spatial_block_cv_scatter_by_fold.png",
  "Figure_S7_internal_validation_by_year.png",
  "Figure_S8_internal_calibration_deciles_overall.png",
  "Figure_S9_spatial_block_cv_scatter_by_year.png",
  "Figure_S10_spatial_block_cv_calibration_fixed_bins_by_year.png",
  "Figure_S11_spatial_block_cv_calibration_fixed_bins_overall.png",
  "Figure_S12_spatial_block_cv_calibration_deciles_by_year.png",
  "Figure_S13_spatial_block_cv_calibration_deciles_overall.png"
)
unlink(file.path(cfg$dirs$figures_supp, legacy), force = TRUE)
unlink(file.path(cfg$dirs$figures_main, "Figure_5_spatial_block_cv_scatter_by_fold.png"), force = TRUE)

save_plot(
  plot_internal_validation(internal),
  file.path(cfg$dirs$figures_supp, "Figure_S8_internal_validation_by_year.png"),
  width = 10.5, height = 7.0
)
save_plot(
  plot_calibration(internal$calibration_by_year, facet_year = TRUE),
  file.path(cfg$dirs$figures_supp, "Figure_S9_internal_calibration_deciles_by_year.png"),
  width = 10.5, height = 7.0
)
save_plot(
  plot_calibration(internal$calibration_overall, facet_year = FALSE),
  file.path(cfg$dirs$figures_diagnostics, "Diagnostic_internal_calibration_deciles_overall.png"),
  width = 6.5, height = 5.2
)
# The supervisor requested that the fold-level spatial block panels move to the supplement.
save_plot(
  plot_validation_scatter(spatial_cv$predictions, facet = "fold", title = "Spatial block cross-validation"),
  file.path(cfg$dirs$figures_supp, "Figure_S10_spatial_block_cv_scatter_by_fold.png"),
  width = 11.0, height = 7.0
)
save_plot(
  plot_validation_scatter(spatial_cv$predictions, facet = "year", title = "Spatial block cross-validation by survey year"),
  file.path(cfg$dirs$figures_supp, "Figure_S11_spatial_block_cv_scatter_by_year.png"),
  width = 10.5, height = 7.0
)
save_plot(
  plot_calibration(spatial_cv$calibration_fixed_bins_by_year, facet_year = TRUE),
  file.path(cfg$dirs$figures_supp, "Figure_S12_spatial_block_cv_calibration_fixed_bins_by_year.png"),
  width = 10.5, height = 7.0
)
save_plot(
  plot_calibration(spatial_cv$calibration_fixed_bins, facet_year = FALSE),
  file.path(cfg$dirs$figures_supp, "Figure_S13_spatial_block_cv_calibration_fixed_bins_overall.png"),
  width = 6.5, height = 5.2
)
save_plot(
  plot_calibration(spatial_cv$calibration_deciles, facet_year = TRUE),
  file.path(cfg$dirs$figures_supp, "Figure_S14_spatial_block_cv_calibration_deciles_by_year.png"),
  width = 10.5, height = 7.0
)
save_plot(
  plot_calibration(spatial_cv$calibration_overall, facet_year = FALSE),
  file.path(cfg$dirs$figures_supp, "Figure_S15_spatial_block_cv_calibration_deciles_overall.png"),
  width = 6.5, height = 5.2
)

save_plot(
  plot_validation_scatter(random_cv$predictions, facet = "fold", title = "Random cross-validation"),
  file.path(cfg$dirs$figures_diagnostics, "Diagnostic_random_cv_scatter_by_fold.png"),
  width = 10.5, height = 7.0
)
save_plot(
  plot_validation_method_comparison(comparison),
  file.path(cfg$dirs$figures_diagnostics, "Diagnostic_random_vs_spatial_cv_comparison.png"),
  width = 10.0, height = 4.5
)
message("Supplementary Figures S8-S15 written from the saved validation results.")
