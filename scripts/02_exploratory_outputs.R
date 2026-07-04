source(file.path("scripts", "_bootstrap.R"))
prep <- read_rds_required(file.path(cfg$dirs$model, "01_prepared_data.rds"))

save_plot(
  plot_observed_pfpr_by_year(prep, cfg),
  file.path(cfg$dirs$figures_main, "Figure_1_observed_cluster_pfpr_by_year.png"),
  width = 10.5, height = 8.0
)
save_plot(
  plot_clusters_by_year(prep),
  file.path(cfg$dirs$figures_supp, "Figure_S1_clusters_by_year.png"),
  width = 6.5, height = 3.8
)
save_plot(
  plot_clusters_by_month_year(prep),
  file.path(cfg$dirs$figures_supp, "Figure_S2_clusters_by_month_and_year.png"),
  width = 8.5, height = 4.2
)
save_plot(
  plot_all_cluster_locations(prep),
  file.path(cfg$dirs$figures_supp, "Figure_S3_spatial_distribution_all_clusters.png"),
  width = 6.5, height = 7.5
)
save_plot(
  plot_observed_pfpr_distribution(prep),
  file.path(cfg$dirs$figures_supp, "Figure_S4_observed_cluster_pfpr_distribution.png"),
  width = 6.5, height = 3.8
)

clusters_year <- prep$obs |>
  dplyr::count(year_end, name = "clusters") |>
  dplyr::rename(year = year_end)
clusters_month_year <- prep$obs |>
  dplyr::filter(!is.na(month_end)) |>
  dplyr::count(year_end, month_end, name = "clusters") |>
  dplyr::rename(year = year_end, month = month_end)
write_csv_checked(clusters_year, file.path(cfg$dirs$tables_supp, "Table_S_clusters_by_year.csv"))
write_csv_checked(clusters_month_year, file.path(cfg$dirs$tables_supp, "Table_S_clusters_by_month_and_year.csv"))
message("Exploratory main and supplementary outputs written.")
