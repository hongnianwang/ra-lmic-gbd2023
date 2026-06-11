# Rheumatoid arthritis burden in 129 LMICs from GBD 2023

This repository contains the R code used to reproduce the figures and supporting analysis tables for a Global Burden of Disease Study 2023 analysis of rheumatoid arthritis burden in 129 low- and middle-income countries (LMICs). It does not re-run the GBD estimation process.

## Data

The analysis uses existing, publicly available, aggregate data. No individual-level data are used.

Raw GBD extracts and generated results are not included. Download the public source data separately and place them under `data/raw/`.

Main data sources:

- GBD 2023 Results Tool: https://vizhub.healthdata.org/gbd-results/
- GBD 2023 Socio-demographic Index: https://ghdx.healthdata.org/record/gbd-2023-socio-demographic-index-sdi
- World Bank GNI per capita: https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=DR0046430
- World Bank country and lending groups: https://ddh-openapi.worldbank.org/resources/DR0095334/download

`data/raw/gni_classification.csv` defines the 129 LMICs and their GNI strata. Required local input files are listed in `data/raw/README.md`.

## Usage

R version 4.3 or later is recommended. Required non-base packages are `data.table`, `dplyr`, `tidyr`, `ggplot2`, `patchwork`, `forecast`, `MASS`, `maps`, `RColorBrewer`, and `scales`.

From the repository root, run:

```r
source("scripts/99_run_all.R")
```

or:

```bash
Rscript scripts/99_run_all.R
```

Generated files are written to `outputs/`.

## Citation

Citation will be added after publication.

## License

MIT
