get_config <- function(root = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  years <- c(2007L, 2011L, 2015L, 2018L, 2022L, 2023L)

  list(
    root = root,
    seed = 20260630L,
    reference_year = 2007L,
    survey_years = years,

    files = list(
      observations = file.path(root, "data", "raw", "Moz_ObsCov_data_5km_ALLsurveys.csv"),
      prediction_grid = file.path(root, "data", "raw", "grid5km_covariates_allSurveys_anchorDomMonth_lag0to3.csv"),
      districts = file.path(root, "data", "boundaries", "MOZ_dist161.shp")
    ),

    dirs = list(
      model = file.path(root, "outputs", "model"),
      rasters = file.path(root, "outputs", "rasters"),
      validation = file.path(root, "outputs", "validation"),
      logs = file.path(root, "outputs", "logs"),
      district = file.path(root, "outputs", "district"),
      figures_main = file.path(root, "outputs", "figures", "main"),
      figures_supp = file.path(root, "outputs", "figures", "supplementary"),
      figures_diagnostics = file.path(root, "outputs", "figures", "diagnostics"),
      tables_main = file.path(root, "outputs", "tables", "main"),
      tables_supp = file.path(root, "outputs", "tables", "supplementary")
    ),

    dynamic_families = c(
      "Rain", "EVI", "LST_day", "LST_night", "LST_delta",
      "TCB", "TCW", "TSI", "HSI"
    ),
    dynamic_lags = 0:3,
    static_candidates = c(
      "PET_mm", "NTL_stable", "Aridity_Index", "Elevation_m",
      "Slope_deg", "DistToWater_m", "TWI", "WorldPop_static",
      "Access_Cities_log1p", "Access_Health_log1p"
    ),

    # Frozen final model used for all manuscript results. Screening remains
    # reproducible, but the submitted model cannot silently change.
    final_covariates_raw = c(
      "TSI_lag3", "HSI_lag2", "LST_day_lag0", "Aridity_Index",
      "EVI_lag2", "DistToWater_m", "Access_Cities_log1p", "Slope_deg"
    ),

    thematic_groups = list(
      thermal = c("LST_day", "LST_night", "LST_delta"),
      rainfall = c("Rain"),
      atmospheric = c("Aridity_Index", "PET_mm"),
      greenwet = c("EVI", "TCW", "TCB"),
      hydrology = c("DistToWater_m", "TWI"),
      terrain = c("Elevation_m", "Slope_deg"),
      human_accessibility = c(
        "WorldPop_static", "NTL_stable", "Access_Health_log1p",
        "Access_Cities_log1p"
      )
    ),

    mesh = list(
      candidates = list(
        mesh1 = list(inner_km = 70, outer_km = 280, cutoff_km = 15, offset_km = c(20, 100)),
        mesh2 = list(inner_km = 40, outer_km = 150, cutoff_km = 12, offset_km = c(20, 80)),
        mesh3 = list(inner_km = 10, outer_km = 40, cutoff_km = 9, offset_km = c(20, 80))
      ),
      selected = "mesh3"
    ),

    cv = list(
      k = 5L,
      spatial_block_km = 100,
      random_seed = 20260630L,
      spatial_seed = 20260631L
    ),

    thresholds = c(0.10, 0.20, 0.30, 0.40),
    decision_threshold = 0.20,
    decision_probability = 0.80,
    raster_resolution_deg = 0.05,

    district_aggregation = list(
      method = "area_weighted_exact",
      fallback = "nearest_valid_cell",
      require_complete = TRUE
    ),

    map_context = list(
      label = "Malawi",
      longitude = 34.25,
      latitude = -13.55
    )
  )
}
