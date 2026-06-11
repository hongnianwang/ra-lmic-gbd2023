ROOT <- normalizePath(getwd(), mustWork = TRUE)
DIR_DATA_RAW <- file.path(ROOT, "data", "raw")
DIR_DATA_PROCESSED <- file.path(ROOT, "data", "processed")
DIR_OUTPUTS <- file.path(ROOT, "outputs")
DIR_FIGURES <- file.path(DIR_OUTPUTS, "figures")
DIR_TABLES <- file.path(DIR_OUTPUTS, "tables")
DIR_INTERMEDIATE <- file.path(DIR_OUTPUTS, "intermediate")

dir.create(DIR_DATA_PROCESSED, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_TABLES, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_INTERMEDIATE, showWarnings = FALSE, recursive = TRUE)

source_data_root <- function() {
  env_root <- Sys.getenv("RA_GBD_SOURCE_DIR", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, mustWork = FALSE))
  }
  DIR_DATA_RAW
}

first_existing <- function(paths, label) {
  paths <- unique(paths)
  for (path in paths) {
    if (file.exists(path)) {
      return(normalizePath(path, mustWork = TRUE))
    }
  }
  stop(
    "Missing required input for ", label, ". Tried:\n",
    paste0("  - ", paths, collapse = "\n")
  )
}

input_file <- function(kind) {
  src <- source_data_root()
  switch(
    kind,
    pi_pick = first_existing(c(
      file.path(DIR_DATA_RAW, "Prevalence and incidence pick.csv"),
      file.path(DIR_DATA_RAW, "Prevalence_and_incidence_pick.csv"),
      file.path(src, "Prevalence and incidence pick.csv"),
      file.path(src, "Prevalence_and_incidence_pick.csv")
    ), "prevalence/incidence extract"),
    dd_pick = first_existing(c(
      file.path(DIR_DATA_RAW, "DALY and Deaths pick.csv"),
      file.path(DIR_DATA_RAW, "DALY_and_Deaths_pick.csv"),
      file.path(src, "DALY and Deaths pick.csv"),
      file.path(src, "DALY_and_Deaths_pick.csv")
    ), "DALY/deaths extract"),
    sdi = first_existing(c(
      file.path(DIR_DATA_RAW, "SDI.csv"),
      file.path(src, "SDI.csv")
    ), "SDI data"),
    gni = first_existing(c(
      file.path(DIR_DATA_RAW, "gni_classification.csv"),
      file.path(src, "gni_classification.csv")
    ), "GNI classification"),
    stop("Unknown input kind: ", kind)
  )
}

population_files <- function() {
  src <- source_data_root()
  dirs <- c(
    file.path(DIR_DATA_RAW, "population"),
    file.path(src, "population")
  )
  for (dir in dirs) {
    files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) > 0) {
      return(normalizePath(files, mustWork = TRUE))
    }
  }
  stop(
    "Missing population CSV files. Place them under data/raw/population/ ",
    "or set RA_GBD_SOURCE_DIR to a directory containing a population/ folder."
  )
}

processed_file <- function(name) {
  file.path(DIR_DATA_PROCESSED, name)
}

figure_file <- function(name) {
  file.path(DIR_FIGURES, name)
}

table_file <- function(name) {
  file.path(DIR_TABLES, name)
}
