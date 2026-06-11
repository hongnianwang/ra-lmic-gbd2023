# Raw data

Raw GBD extracts are not included in this repository. Download the public source files and place them in this directory, or set `RA_GBD_SOURCE_DIR` to a directory containing the same files.

Expected local files:

- `Prevalence and incidence pick.csv`
- `DALY and Deaths pick.csv`
- `SDI.csv`
- `population/*.csv`

The first two files are analysis-ready extracts prepared from the GBD Results Tool for the 129 LMICs and the three GNI aggregate strata used in the manuscript. They include rheumatoid arthritis prevalence, incidence, deaths, and DALYs by location, sex, age, metric, and year. The scripts also accept underscore versions of these filenames: `Prevalence_and_incidence_pick.csv` and `DALY_and_Deaths_pick.csv`.

Data sources:

- GBD 2023 Results Tool: https://vizhub.healthdata.org/gbd-results/
- GBD 2023 Socio-demographic Index: https://ghdx.healthdata.org/record/gbd-2023-socio-demographic-index-sdi
- World Bank GNI per capita: https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=DR0046430
- World Bank country and lending groups: https://ddh-openapi.worldbank.org/resources/DR0095334/download

Large raw data files are intentionally excluded from Git. The bundled `gni_classification.csv` is a small metadata file used to define the study countries and GNI strata.

## File columns

The two rheumatoid arthritis burden files should contain:

`measure`, `location`, `sex`, `age`, `cause`, `metric`, `year`, `val`, `upper`, and `lower`.

The population files should be standard GBD population files with:

`measure_name`, `location_name`, `sex_name`, `age_name`, `metric_name`, `year`, `val`, `upper`, and `lower`.

The SDI file should be the IHME GBD 2023 SDI covariate file, renamed as `SDI.csv` for this repository, with:

`location_name`, `year_id`, and `mean_value`.

The bundled `gni_classification.csv` records the 129 LMICs and 2023 World Bank income strata used in the manuscript.
