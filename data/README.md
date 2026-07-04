# Data placement and release policy

The public repository does not redistribute the cluster-level survey input, the 5 km covariate prediction grid or the district boundary files. These files must be obtained from their original providers and placed locally as follows:

```text
data/raw/Moz_ObsCov_data_5km_ALLsurveys.csv
data/raw/grid5km_covariates_allSurveys_anchorDomMonth_lag0to3.csv
data/boundaries/MOZ_dist161.shp
data/boundaries/MOZ_dist161.dbf
data/boundaries/MOZ_dist161.shx
data/boundaries/MOZ_dist161.prj
data/boundaries/MOZ_dist161.cpg
```

The expected SHA-256 checksums are recorded in `input_checksums_sha256.csv`. Run `scripts/00_preflight.R` after placing the files to verify the input structure before fitting the model.

## Publicly included outputs

The repository includes reviewed, non-identifiable derived products: manuscript figures and tables, district-level posterior summaries, district aggregation audit files and compact reproducibility logs. Large rasters, fitted model objects and cluster-level validation predictions are excluded from GitHub.

## Zenodo release

A GitHub release can be archived in Zenodo. Additional large derived outputs may be attached to the Zenodo record where redistribution is permitted. Source data should not be included unless the original providers explicitly permit redistribution.
