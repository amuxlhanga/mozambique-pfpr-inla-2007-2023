# Run only after the final successful analysis to create a reproducible package lockfile.
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE, restart = FALSE)
}
renv::snapshot(prompt = FALSE)
message("renv.lock created or refreshed.")
