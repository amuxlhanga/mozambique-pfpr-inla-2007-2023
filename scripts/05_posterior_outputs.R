source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
model_bundle <- read_rds_required(file.path(cfg$dirs$model, "04_final_pooled_inla_spde_model.rds"))

fixed <- extract_fixed_effects(model_bundle)
year_effects <- extract_year_effects(model_bundle, prep, cfg)
hyper <- extract_spatial_hyperparameters(model_bundle)

write_csv_checked(
  fixed |>
    dplyr::select(covariate, posterior_mean, lower_95, upper_95, credible_interval),
  file.path(cfg$dirs$tables_main, "Table_1_fixed_effect_posterior_summaries.csv")
)
write_csv_checked(year_effects, file.path(cfg$dirs$tables_supp, "year_random_effects_and_reference_shifts.csv"))
write_csv_checked(hyper$summary, file.path(cfg$dirs$tables_supp, "spatial_hyperparameter_posterior_means.csv"))
saveRDS(hyper, file.path(cfg$dirs$model, "05_spatial_hyperparameter_marginals.rds"), compress = "gzip")

# Remove obsolete placements from versions before supervisor-comment alignment.
unlink(c(
  file.path(cfg$dirs$figures_main, "Figure_3_fixed_effect_forest_plot.png"),
  file.path(cfg$dirs$figures_main, "Figure_3_spatial_hyperparameter_densities.png"),
  file.path(cfg$dirs$figures_main, "Figure_4_spatial_hyperparameter_densities.png"),
  file.path(cfg$dirs$figures_supp, "Figure_S_fixed_effect_posterior_densities.png"),
  file.path(cfg$dirs$figures_diagnostics, "Diagnostic_fixed_effect_posterior_densities.png")
), force = TRUE)

# The supervisor requested that the fixed-effect density panels move to the supplement;
# Table 1 remains the concise main-text summary.
save_plot(
  plot_fixed_effect_densities(model_bundle),
  file.path(cfg$dirs$figures_supp, "Figure_S7_fixed_effect_posterior_densities.png"),
  width = 10.5, height = 8.0
)
save_plot(
  plot_spatial_hyperparameter_densities(hyper),
  file.path(cfg$dirs$figures_main, "Figure_4_spatial_hyperparameter_densities.png"),
  width = 9.5, height = 3.8
)
message("Main Figure 4, Supplementary Figure S7 and posterior summary tables written.")
