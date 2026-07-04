# Changelog

## 1.1.0 - 2026-07-03

- Corrected Supplementary Figure S9 to show internal calibration by survey year; retained pooled calibration only as a diagnostic.
- Reworked district aggregation to use exact polygon-cell overlap and geodesic cell-area weighting, with a fully documented nearest-valid-cell fallback and hard audits for complete, bounded and internally consistent results in all 161 districts.
- Added the Malawi context label to district and extended exceedance maps.
- Removed the unavailable external MAP comparison panels from the reproducible manuscript inventory and renumbered supplementary figures sequentially from S1 to S23.
- Removed the misleading one-row multivariable comparison output and replaced it with a retained-covariate selection-evidence table.
- Expanded the covariate missingness table to document standardisation and mean imputation.
- Added district summary, district extremes and consolidated key-result tables for manuscript revision.
- Added content-level figure, table and district audits and repository-relative checksum paths.
- Added a resume script that regenerates corrected outputs without repeating model fitting or cross-validation.
- Added a fast preflight script that checks packages, inputs, R syntax, helper functions, figure configuration and the 161-district boundary before a long run.

## 1.0.9 - 2026-07-03

- Promoted the area-weighted district mean PfPR map to Main Figure 3 for the planned district-results subsection.
- Renumbered the spatial hyperparameter densities as Main Figure 4 and the decision maps as Main Figure 5.
- Preserved the supervisor-directed moves of fixed-effect densities to Supplementary Figure S7 and spatial block fold panels to Supplementary Figure S10.
- Kept the supplementary inventory unchanged at Figures S1-S29.
- Removed the former duplicate unnumbered district PfPR map from `outputs/district/`; the manuscript-facing district map is written once under `outputs/figures/main/`.
- Updated the figure manifest, output audit, regression tests, release checklist and manuscript revision notes to five main figures and 29 supplementary figures.

## 1.0.8 - 2026-07-03

- Aligned figure placement with the supervisor annotations in the reviewed main-text PDF.
- Moved the fixed-effect posterior density panels from the main text to Supplementary Figure S7; Table 1 remains the main-text fixed-effect summary.
- Moved the spatial block cross-validation fold panels from the main text to Supplementary Figure S10.
- Renumbered the remaining main-text figures to four figures: observed PfPR, posterior mean surfaces, spatial hyperparameters and decision maps.
- Renumbered the supplementary inventory sequentially to Figures S1-S29 and updated all scripts, manifests, tests and release documentation.
- Added cleanup of obsolete v1.0.7 figure filenames so stale main-text files do not remain in outputs.

## 1.0.7 - 2026-07-03

- Restored the exact six-figure numbering used in the main manuscript.
- Configured and audited all 27 supplementary figure numbers.
- Added the missing fixed-width spatial calibration by year for Figure S10.
- Corrected Figure S8 to use pooled internal calibration deciles rather than duplicated year facets.
- Separated non-manuscript random-CV and fixed-effect-density diagnostics from manuscript figures.
- Added `scripts/06_render_validation_outputs.R` and `00_resume_from_figures.R` so figures can be regenerated from saved model and validation objects without rerunning cross-validation.
- Added a 33-figure manifest and exact output audit.
- External MAP comparison figures remain conditional until their exact source rasters are supplied.

## 1.0.6 — 2026-07-03

- Corrected year-specific filtering in prediction, exceedance, district aggregation and uncertainty plotting. The previous dplyr expression `.data$year == year` compared the year column with itself because data-mask variables take precedence, so every survey year was retained.
- Added a single `filter_year_rows()` helper and applied it consistently across stages 07–09 and supplementary uncertainty plots.
- Added a prediction-year regression test, a static audit rule for ambiguous year filtering and a resume script for stages 07–11.
- Improved the stage 07 error message to report expected and observed row counts.

## 1.0.5 — 2026-07-01

- Corrected calibration aggregation in stage 06. The previous `summarise()` created a scalar `examined` total before calling `weighted.mean()`, so the prediction vector and weight vector had different lengths.
- Centralised internal, random CV and spatial block CV calibration summaries in one validated helper to avoid duplicated aggregation logic.
- Added a regression test for weighted calibration summaries and an optional resume script for stages 06–11.
- Added a static audit rule that rejects the faulty weighted-mean expression.

## 1.0.4 — 2026-07-01

- Corrected binomial `Ntrials` handling for current R-INLA versions by passing an evaluated local vector rather than `dat$Ntrials`.
- Removed the obsolete `return.marginals` entry from `control.predictor` and placed marginal controls under `control.compute`.
- Added an explicit baseline INLA runtime check before covariate screening so systemic failures stop once with a clear message.
- Added binomial response and trial validation and retained detailed INLA error messages in screening tables.
- Extended the R-only static audit to detect the incompatible INLA call pattern.

## 1.0.3 — 2026-07-01

- Added the two required analytical CSV inputs to the complete downloadable package.
- Added the full `MOZ_dist161` shapefile component set.
- Added SHA-256 checksums for all included analytical inputs.
- Clarified the distinction between the self-contained local package, the code-only GitHub repository, and the complete Zenodo release.

## 1.0.2 — 2026-07-01

- Removed the optional Python audit and all `reticulate` requirements.
- Reimplemented the structural audit entirely in base R as `tests/run_static_audit.R`.
- Added base R syntax parsing and a check that the repository contains no R Markdown files.
- Clarified in the README that the complete analytical and audit workflow is R-only.

## 1.0.0 — 2026-06-30

- Replaced the monolithic R Markdown workflow with ordered R scripts and shared functions.
- Corrected survey-year assignment for all prediction-grid rows.
- Prevented averaging of repeated survey-year grids during rasterisation.
- Standardised the decision cutoff to p*=0.80.
- Froze the final manuscript covariate set using daytime LST at lag 0.
- Added district boundaries to manuscript maps and labelled Malawi.
- Added random five-fold cross-validation alongside spatial block cross-validation.
- Moved fixed-effect density and fold-level CV panels to supplementary outputs.
- Added canonical district-level CSV and GeoPackage outputs.
- Added GitHub, Zenodo, citation and release documentation.
- Added `Ch2_PfPR_reproducible.Rproj` at the repository root and updated the README to make the RStudio project the standard entry point.
