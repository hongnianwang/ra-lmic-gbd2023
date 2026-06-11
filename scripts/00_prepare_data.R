source(file.path("R", "paths.R"))
source(file.path("R", "helpers.R"))

message("Preparing analysis data...")

pi <- data.table::fread(input_file("pi_pick"), showProgress = FALSE)
dd <- data.table::fread(input_file("dd_pick"), showProgress = FALSE)

ra <- bind_rows(pi, dd) %>%
  mutate(
    measure = as.character(measure),
    location = clean_location_names(location),
    sex = as.character(sex),
    age = as.character(age),
    metric = as.character(metric),
    year = as.integer(year),
    val = as.numeric(val),
    upper = as.numeric(upper),
    lower = as.numeric(lower)
  ) %>%
  filter(measure %in% MEASURE_ORDER, !is.na(location), !is.na(year))

data.table::fwrite(ra, processed_file("ra_lmic_all_measures.csv"))

all_age_rates <- ra %>%
  filter(age == "All ages", sex == "Both", metric == "Rate") %>%
  mutate(
    measure = factor(measure, levels = MEASURE_ORDER),
    location = factor(location, levels = c("All location", GNI_ORDER, sort(setdiff(unique(location), c("All location", GNI_ORDER)))))
  )
data.table::fwrite(all_age_rates, processed_file("all_age_rates.csv"))

gni_raw <- data.table::fread(input_file("gni"), showProgress = FALSE)
gni <- gni_raw %>%
  rename_with(~gsub("^GNI_Class$", "GNI_Class", .x)) %>%
  transmute(
    location = clean_location_names(location),
    GNI = suppressWarnings(as.numeric(GNI)),
    GNI_Class = case_when(
      GNI_Class %in% c("L", "GNI-L") ~ "GNI-L",
      GNI_Class %in% c("LM", "GNI-LM") ~ "GNI-LM",
      GNI_Class %in% c("UM", "GNI-UM") ~ "GNI-UM",
      TRUE ~ as.character(GNI_Class)
    )
  ) %>%
  distinct(location, .keep_all = TRUE) %>%
  filter(!is.na(location), location != "")
data.table::fwrite(gni, processed_file("gni_classification.csv"))

sdi_raw <- data.table::fread(input_file("sdi"), showProgress = FALSE)
population_raw <- data.table::rbindlist(lapply(population_files(), data.table::fread, showProgress = FALSE), fill = TRUE)

country_location_lookup <- population_raw %>%
  as_tibble() %>%
  transmute(
    location_id = suppressWarnings(as.integer(location_id)),
    location = clean_location_names(location_name)
  ) %>%
  semi_join(gni, by = "location") %>%
  filter(!is.na(location_id), !is.na(location), location != "") %>%
  distinct(location_id, location)

sdi <- sdi_raw %>%
  transmute(
    location_id = suppressWarnings(as.integer(location_id)),
    location = clean_location_names(location_name),
    year = as.integer(year_id),
    sdi = as.numeric(mean_value)
  ) %>%
  inner_join(country_location_lookup, by = c("location_id", "location")) %>%
  select(location, year, sdi) %>%
  distinct(location, year, .keep_all = TRUE)
data.table::fwrite(sdi, processed_file("sdi_lmic.csv"))

pop <- population_raw %>%
  as_tibble() %>%
  transmute(
    location = clean_location_names(location_name),
    sex = sex_name,
    age = as.character(age_name),
    metric = metric_name,
    year = as.integer(year),
    population = as.numeric(val)
  ) %>%
  filter(age == "All ages", sex == "Both", metric == "Number") %>%
  semi_join(gni, by = "location") %>%
  group_by(location, sex, year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
data.table::fwrite(pop, processed_file("population_lmic.csv"))

manifest <- tibble(
  item = c("RA burden estimates", "GNI classification", "SDI", "Population"),
  rows = c(nrow(ra), nrow(gni), nrow(sdi), nrow(pop)),
  output = c(
    file.path("data", "processed", "ra_lmic_all_measures.csv"),
    file.path("data", "processed", "gni_classification.csv"),
    file.path("data", "processed", "sdi_lmic.csv"),
    file.path("data", "processed", "population_lmic.csv")
  )
)
data.table::fwrite(manifest, table_file("data_manifest.csv"))

message("Done. Processed files written to: ", DIR_DATA_PROCESSED)
