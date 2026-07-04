source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
model_bundle <- read_rds_required(file.path(cfg$dirs$model, "04_final_pooled_inla_spde_model.rds"))

predictions <- extract_prediction_outputs(model_bundle, prep)
saveRDS(predictions$grid, file.path(cfg$dirs$model, "07_grid_predictions.rds"), compress = "gzip")
write_csv_checked(predictions$observations, file.path(cfg$dirs$model, "observation_posterior_predictions.csv"))

raster_index <- write_prediction_rasters(predictions$grid, prep, cfg)
write_csv_checked(raster_index, file.path(cfg$dirs$rasters, "prediction_raster_index.csv"))
latent_index <- project_latent_spatial_field(model_bundle, prep, cfg)
write_csv_checked(
  tibble::tibble(component = names(latent_index), raster_file = unlist(latent_index, use.names = FALSE)),
  file.path(cfg$dirs$rasters, "latent_field_raster_index.csv")
)

save_plot(
  plot_prediction_mean_by_year(raster_index, prep, cfg),
  file.path(cfg$dirs$figures_main, "Figure_2_year_specific_posterior_mean_pfpr.png"),
  width = 10.5, height = 8.0
)
unlink(c(
  file.path(cfg$dirs$figures_supp, "Figure_S_latent_spatial_field_mean_and_sd.png"),
  file.path(cfg$dirs$figures_supp, paste0("Figure_S_model_pfpr_lower_mean_upper_", cfg$survey_years, ".png")),
  list.files(
    cfg$dirs$figures_supp,
    pattern = "^Figure_S(1[7-9]|2[0-9])_.*(model_pfpr|external_MAP|exceedance).*\\.png$",
    full.names = TRUE
  )
), force = TRUE)

for (year in cfg$survey_years) {
  figure_number <- supplementary_model_figure_number(year)
  save_plot(
    plot_uncertainty_surfaces_for_year(raster_index, prep, cfg, year),
    file.path(
      cfg$dirs$figures_supp,
      paste0("Figure_S", figure_number, "_model_pfpr_lower_mean_upper_", year, ".png")
    ),
    width = 11.0, height = 5.4
  )
}
save_plot(
  plot_latent_spatial_fields(latent_index, prep, cfg),
  file.path(cfg$dirs$figures_supp, "Figure_S16_latent_spatial_field_mean_and_sd.png"),
  width = 10.0, height = 6.5
)
message("Year-specific prediction surfaces written without averaging repeated survey-year grids.")
