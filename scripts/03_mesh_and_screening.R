source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))

spatial <- build_spatial_objects(prep, cfg)
saveRDS(spatial, file.path(cfg$dirs$model, "03_spatial_objects.rds"), compress = "gzip")
save_candidate_mesh_figure(
  spatial, prep,
  file.path(cfg$dirs$figures_supp, "Figure_S5_candidate_spde_meshes.png")
)

# Fit one baseline model before launching many screening models. This catches
# any INLA runtime or binomial Ntrials problem immediately instead of producing
# the same warning for every candidate covariate.
baseline <- tryCatch(
  fit_observation_model(prep, spatial, character(0)),
  error = function(e) {
    stop(
      "Baseline INLA binomial stack check failed: ", conditionMessage(e),
      call. = FALSE
    )
  }
)
saveRDS(baseline, file.path(cfg$dirs$model, "03_baseline_inla_spde_model.rds"), compress = "gzip")
write_csv_checked(
  tibble::tibble(
    INLA_version = as.character(utils::packageVersion("INLA")),
    WAIC = baseline$fit$waic$waic,
    DIC = baseline$fit$dic$dic,
    n_observations = nrow(prep$obs)
  ),
  file.path(cfg$dirs$logs, "inla_baseline_runtime_check.csv")
)
message("Baseline INLA binomial stack check passed.")

screening <- run_covariate_screening(prep, spatial, cfg)
saveRDS(screening, file.path(cfg$dirs$model, "03_covariate_screening.rds"), compress = "gzip")
write_csv_checked(
  screening$all,
  file.path(cfg$dirs$tables_supp, "Table_S_covariate_univariate_screening.csv")
)
write_csv_checked(
  screening$best_lags,
  file.path(cfg$dirs$tables_supp, "Table_S_best_univariate_TSI_HSI_lags.csv")
)

# The submitted model is frozen in config/config.R. Rather than presenting a
# one-row table as if it were a multivariable model comparison, document the
# screening evidence and selection basis for each retained covariate.
unlink(file.path(cfg$dirs$model, "03_candidate_model_sets.rds"), force = TRUE)
write_csv_checked(
  final_model_selection_evidence(screening, cfg),
  file.path(cfg$dirs$tables_supp, "Table_S_final_model_selection_evidence.csv")
)

write_csv_checked(
  covariate_dictionary(cfg),
  file.path(cfg$dirs$tables_supp, "Table_S1_candidate_covariate_dictionary.csv")
)
write_csv_checked(
  forced_suitability_lags(cfg),
  file.path(cfg$dirs$tables_supp, "Table_S2_forced_suitability_indices_retained_lags.csv")
)
write_csv_checked(
  thematic_group_table(cfg),
  file.path(cfg$dirs$tables_supp, "Table_S3_thematic_covariate_groups.csv")
)

vif <- compute_vif_table(prep, cfg)
write_csv_checked(vif, file.path(cfg$dirs$tables_supp, "Table_S4_retained_covariate_VIF.csv"))
cor_mat <- retained_correlation_matrix(prep, cfg)
cor_long <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
names(cor_long) <- c("covariate_1", "covariate_2", "correlation")
write_csv_checked(
  tibble::as_tibble(cor_long),
  file.path(cfg$dirs$tables_supp, "Table_S_retained_covariate_correlations_long.csv")
)
save_plot(
  plot_correlation_matrix(cor_mat),
  file.path(cfg$dirs$figures_supp, "Figure_S6_correlation_matrix_retained_covariates.png"),
  width = 8.2, height = 7.2
)
message("Mesh construction, univariate screening, final-model selection evidence and collinearity diagnostics completed.")
