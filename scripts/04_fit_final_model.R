source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
spatial <- read_rds_required(file.path(cfg$dirs$model, "03_spatial_objects.rds"))

model_bundle <- fit_final_manuscript_model(prep, spatial, cfg)
saveRDS(model_bundle, file.path(cfg$dirs$model, "04_final_pooled_inla_spde_model.rds"), compress = "gzip")

fit_summary <- tibble::tibble(
  WAIC = model_bundle$fit$waic$waic,
  DIC = model_bundle$fit$dic$dic,
  effective_parameters_WAIC = model_bundle$fit$waic$p.eff,
  effective_parameters_DIC = model_bundle$fit$dic$p.eff,
  n_observations = nrow(prep$obs),
  n_prediction_rows = nrow(prep$grid),
  prediction_years = paste(sort(unique(prep$grid$year_anchor)), collapse = ",")
)
write_csv_checked(fit_summary, file.path(cfg$dirs$model, "final_model_information_criteria.csv"))
write_csv_checked(
  fit_summary,
  file.path(cfg$dirs$tables_supp, "Table_S_final_model_information_criteria.csv")
)
message("Final frozen manuscript model fitted with year-aligned prediction rows.")
