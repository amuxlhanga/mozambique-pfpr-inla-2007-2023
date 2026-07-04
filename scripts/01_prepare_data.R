source(file.path("scripts", "_bootstrap.R"))

prep <- prepare_analysis_data(cfg)
write_data_preparation_outputs(prep, cfg)

input_files <- unlist(cfg$files, use.names = FALSE)
write_csv_checked(
  file_md5_table(input_files, root = cfg$root),
  file.path(cfg$dirs$logs, "input_file_checksums.csv")
)
message("Prepared observations, prediction grid, covariates and administrative boundaries.")
