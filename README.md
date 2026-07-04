# Mozambique PfPR mapping with pooled INLA–SPDE models, 2007–2023

This repository contains the R code, documentation and reviewed derived outputs supporting **Subnational mapping of Plasmodium falciparum parasite prevalence in Mozambique using pooled national survey data from 2007 to 2023**.

The workflow is fully script based. It generates the main manuscript figures and tables, supplementary diagnostics, internal and cross-validation results, year-specific PfPR rasters, exceedance and decision maps, and district-level outputs.

## Public repository scope

This GitHub release contains the complete analysis code, documentation, main and supplementary figures, manuscript tables, and reviewed district-level aggregate outputs. The cluster-level analytical inputs, covariate prediction grid and district boundary files are **not redistributed** because they are subject to provider conditions or must be obtained from their original sources. Their required locations and SHA-256 checksums are documented under `data/`.

The repository does not contain individual identifiers. District-level outputs are area-level posterior summaries. Large prediction rasters, validation prediction files and fitted model objects are excluded from GitHub and may be archived separately with the versioned Zenodo release where permitted.

Repository: <https://github.com/amuxlhanga/mozambique-pfpr-inla-2007-2023>

## Version 1.1.0 corrections

This version resolves the inconsistencies identified during the output-to-manuscript audit:

1. **Supplementary Figure S9** is now the six-panel internal calibration figure by survey year. The pooled internal calibration plot is retained only as a diagnostic output.
2. **District aggregation** uses exact polygon-cell overlap with geodesic cell-area weighting. When a small district has no valid raster support, the nearest valid land prediction cell is used as a documented fallback. The method, distance and source cell coordinates are recorded. The output audit requires complete results for all 161 districts in every year.
3. **Main Figure 3** is the district mean PfPR map and includes the Malawi context label.
4. **External MAP comparison figures were removed** from the manuscript output inventory because the exact source rasters were not supplied. The supplementary inventory is now sequential from S1 to S23.
5. The misleading one-row multivariable comparison table was removed. It is replaced by `Table_S_final_model_selection_evidence.csv`, which documents the univariate screening evidence and selection basis for each of the eight retained covariates.
6. Missing-covariate handling is explicitly documented in `Table_S_missing_covariate_values.csv`, including missingness, observation-based standardisation and mean imputation.
7. Manuscript-ready district summaries, extremes and a consolidated key-results table are generated automatically.
8. Input and output checksum tables use repository-relative paths.

## Required inputs

Place the following locally acquired files in the indicated paths before running the workflow:

```text
data/raw/Moz_ObsCov_data_5km_ALLsurveys.csv
data/raw/grid5km_covariates_allSurveys_anchorDomMonth_lag0to3.csv
data/boundaries/MOZ_dist161.shp
data/boundaries/MOZ_dist161.dbf
data/boundaries/MOZ_dist161.shx
data/boundaries/MOZ_dist161.prj
data/boundaries/MOZ_dist161.cpg
```

## Start the project

Extract the archive into a new folder and open:

```text
Ch2_PfPR_reproducible.Rproj
```

Install required packages once:

```r
source("scripts/99_install_packages.R")
```

Run the preflight checks before a long analysis:

```r
source("scripts/00_preflight.R")
```

This checks packages, inputs, R syntax, helper functions, figure configuration and the 161-district boundary without fitting INLA.

Run the complete analysis:

```r
source("scripts/00_run_all.R")
```

## Update an existing completed project

After extracting the v1.1.0 patch over a project that already contains the fitted model and validation RDS objects, run:

```r
source("APPLY_PATCH_v1.1.0.R")
source("scripts/00_preflight.R")
source("scripts/00_resume_from_corrected_outputs.R")
```

The patch cleanup removes obsolete external MAP and legacy figure files, but retains the fitted model, validation objects, prediction rasters and raw inputs.

This regenerates corrected figures, tables, rasters, district results and audits without refitting the final INLA model or repeating cross-validation.

## Manuscript figure inventory

### Main text

1. Observed cluster-level PfPR by survey year
2. Year-specific posterior mean PfPR surfaces
3. Area-weighted district mean PfPR by survey year
4. Spatial hyperparameter posterior densities
5. Decision maps at PfPR 20% with posterior probability cutoff 0.80

### Supplementary material

Figures S1-S23 are generated automatically. Fixed-effect density panels remain Figure S7 and spatial block cross-validation by fold remains Figure S10, following the supervisor annotations. Figure S9 is the corrected internal calibration figure by year. Figures S17-S22 are the six model lower/mean/upper surface panels, and Figure S23 contains the extended exceedance maps.

## Important result files

```text
outputs/tables/main/Table_1_fixed_effect_posterior_summaries.csv
outputs/tables/main/Table_2_validation_comparison.csv
outputs/tables/main/Table_3_decision_summary_pfpr20_p80.csv
outputs/tables/main/District_summary_by_year.csv
outputs/tables/main/District_extremes_by_year.csv
outputs/tables/main/Key_results_for_manuscript.csv
outputs/tables/supplementary/Table_S_final_model_selection_evidence.csv
outputs/tables/supplementary/Table_S_missing_covariate_values.csv
outputs/district/district_results_all_by_year.csv
outputs/district/district_aggregation_audit.csv
outputs/district/district_fallback_records.csv
outputs/logs/manuscript_figure_audit.csv
outputs/logs/district_content_audit.csv
```

## Reproducibility note

The final model is frozen to the eight covariates defined in `config/config.R`. Random and spatial block cross-validation estimate scaling and imputation parameters from the training observations in each fold. The complete output audit stops if any mandatory figure or table is absent, or if any survey year does not contain complete results for all 161 districts.

## Citation and archiving

Use the citation metadata in `CITATION.cff`. After the first GitHub release is archived in Zenodo, add the version-specific DOI to `CITATION.cff`, `.zenodo.json`, this README and the manuscript.

## License

The analysis code is released under the MIT License. This license does not override the terms governing any third-party input data.
