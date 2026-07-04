# Manuscript revision checklist after the v1.1.0 run

Use the regenerated CSV files as the source of truth when revising the main and supplementary TeX files.

## Main text

- Replace Table 1 values from `outputs/tables/main/Table_1_fixed_effect_posterior_summaries.csv`.
- Replace validation values from `Table_2_validation_comparison.csv` and describe internal fit, random fivefold CV and spatial block fivefold CV.
- Replace all 20% exceedance and decision values from `Table_3_decision_summary_pfpr20_p80.csv` and `exceedance_raster_index.csv`.
- Add the district-results subsection using `District_summary_by_year.csv`, `District_extremes_by_year.csv` and Main Figure 3.
- Do not describe the temporal decline as strictly monotonic unless the regenerated values support that wording.
- Update figure numbering to five main figures.

## Supplementary material

- Use Figures S1-S23 only.
- Figure S9 must be the internal calibration figure by survey year.
- Remove the external MAP comparison section and its figure references unless the exact source rasters are later supplied and deposited.
- Replace the former one-row multivariable comparison table with `Table_S_final_model_selection_evidence.csv`.
- Report missingness, standardisation and mean imputation using `Table_S_missing_covariate_values.csv`.
- Replace VIF, year-effect, hyperparameter and calibration values directly from the generated CSV files.

## District aggregation

- Confirm `outputs/logs/district_content_audit.csv` reports 161 districts, 161 unique districts and zero missing numerical values for every year.
- Report the documented nearest-valid-cell fallback for districts without valid raster support. The affected districts, distances and source cells are listed in `outputs/district/district_fallback_records.csv`.

## Final repository check

- Confirm `outputs/logs/manuscript_figure_audit.csv` contains five main and 23 supplementary figures with status `created`.
- Confirm `outputs/logs/mandatory_nonfigure_output_audit.csv` contains no missing files.
- Replace GitHub, DOI and ORCID placeholders only after the final release is archived.
