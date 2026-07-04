source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))
spatial <- read_rds_required(file.path(cfg$dirs$model, "03_spatial_objects.rds"))
model_bundle <- read_rds_required(file.path(cfg$dirs$model, "04_final_pooled_inla_spde_model.rds"))

internal <- extract_internal_validation(model_bundle, prep)
random_folds <- make_random_folds(prep$obs, cfg$cv$k, cfg$cv$random_seed)
spatial_folds <- make_spatial_block_folds(
  prep$obs, cfg$cv$k, cfg$cv$spatial_block_km, cfg$cv$spatial_seed
)
random_cv <- run_cross_validation(
  prep, spatial, cfg$final_covariates_raw, random_folds,
  method = "Random 5-fold cross-validation", cfg = cfg
)
spatial_cv <- run_cross_validation(
  prep, spatial, cfg$final_covariates_raw, spatial_folds$fold,
  method = "Spatial block 5-fold cross-validation", cfg = cfg
)

validation <- augment_validation_bundle(list(
  internal = internal,
  random = random_cv,
  spatial = spatial_cv,
  random_folds = random_folds,
  spatial_fold_definition = spatial_folds
))
saveRDS(validation, file.path(cfg$dirs$model, "06_validation_results.rds"), compress = "gzip")

source(file.path("scripts", "06_render_validation_outputs.R"), local = TRUE)
