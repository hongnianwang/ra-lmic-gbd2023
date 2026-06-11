suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

MEASURE_ORDER <- c("Prevalence", "Incidence", "Deaths", "DALYs")
GNI_ORDER <- c("GNI-L", "GNI-LM", "GNI-UM")
GNI_LABELS <- c("GNI-L" = "GNI-L", "GNI-LM" = "GNI-LM", "GNI-UM" = "GNI-UM")
GNI_COLORS <- c("GNI-L" = "#D55E00", "GNI-LM" = "#0072B2", "GNI-UM" = "#009E73")
YEAR_COLORS <- c("1990" = "#0F4D92", "2023" = "#B64342")

measure_label <- function(measure) {
  c(
    "Prevalence" = "Prevalence",
    "Incidence" = "Incidence",
    "Deaths" = "Deaths",
    "DALYs" = "DALYs"
  )[measure]
}

rate_label <- function(measure) {
  c(
    "Prevalence" = "Prevalence rate per 100,000",
    "Incidence" = "Incidence rate per 100,000",
    "Deaths" = "Death rate per 100,000",
    "DALYs" = "DALY rate per 100,000"
  )[measure]
}

clean_location_names <- function(x) {
  x <- as.character(x)
  recode <- c(
    "Iran (Islamic Republic of)" = "Iran",
    "Republic of Korea" = "South Korea",
    "Democratic People's Republic of Korea" = "North Korea",
    "Lao People's Democratic Republic" = "Laos",
    "Viet Nam" = "Vietnam",
    "Republic of Moldova" = "Moldova",
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Türkiye" = "Turkiye",
    "Turkey" = "Turkiye",
    "Democratic Republic of the Congo" = "DR Congo",
    "Congo" = "Congo (Brazzaville)",
    "United Republic of Tanzania" = "Tanzania",
    "Syrian Arab Republic" = "Syria",
    "Sao Tome and Principe" = "Democratic Republic of Sao Tome and Principe",
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "Cote d'Ivoire" = "Cote d'Ivoire",
    "Eswatini" = "Swaziland"
  )
  idx <- match(x, names(recode), nomatch = 0)
  x[idx > 0] <- unname(recode[idx])
  x
}

map_location_names <- function(x) {
  x <- clean_location_names(x)
  recode <- c(
    "Cabo Verde" = "Cape Verde",
    "Cote d'Ivoire" = "Ivory Coast",
    "Congo (Brazzaville)" = "Republic of Congo",
    "DR Congo" = "Democratic Republic of the Congo",
    "Democratic Republic of Sao Tome and Principe" = "Sao Tome and Principe",
    "Saint Vincent and the Grenadines" = "Saint Vincent",
    "Turkiye" = "Turkey"
  )
  idx <- match(x, names(recode), nomatch = 0)
  x[idx > 0] <- unname(recode[idx])
  x
}

age_order <- function() {
  c(
    "<5 years", "5-9 years", "10-14 years", "15-19 years",
    "20-24 years", "25-29 years", "30-34 years", "35-39 years",
    "40-44 years", "45-49 years", "50-54 years", "55-59 years",
    "60-64 years", "65-69 years", "70-74 years", "75-79 years",
    "80+ years"
  )
}

read_ra_data <- function() {
  f <- processed_file("ra_lmic_all_measures.csv")
  if (!file.exists(f)) {
    stop("Processed RA data not found. Run scripts/00_prepare_data.R first.")
  }
  data.table::fread(f, showProgress = FALSE)
}

read_country_ra_data <- function() {
  read_ra_data() %>%
    filter(!location %in% c("All location", GNI_ORDER))
}

read_gni <- function() {
  f <- processed_file("gni_classification.csv")
  if (!file.exists(f)) {
    stop("Processed GNI classification not found. Run scripts/00_prepare_data.R first.")
  }
  data.table::fread(f, showProgress = FALSE) %>%
    mutate(location = clean_location_names(location))
}

read_sdi <- function() {
  f <- processed_file("sdi_lmic.csv")
  if (!file.exists(f)) {
    stop("Processed SDI data not found. Run scripts/00_prepare_data.R first.")
  }
  data.table::fread(f, showProgress = FALSE) %>%
    mutate(location = clean_location_names(location))
}

read_population <- function() {
  f <- processed_file("population_lmic.csv")
  if (!file.exists(f)) {
    stop("Processed population data not found. Run scripts/00_prepare_data.R first.")
  }
  data.table::fread(f, showProgress = FALSE) %>%
    mutate(location = clean_location_names(location))
}

calc_eapc <- function(df) {
  df <- df %>% filter(!is.na(val), val > 0)
  if (nrow(df) < 3) {
    return(tibble(EAPC = NA_real_, LCI = NA_real_, UCI = NA_real_))
  }
  fit <- lm(log(val) ~ year, data = df)
  beta <- coef(summary(fit))["year", "Estimate"]
  se <- coef(summary(fit))["year", "Std. Error"]
  tibble(
    EAPC = 100 * (exp(beta) - 1),
    LCI = 100 * (exp(beta - 1.96 * se) - 1),
    UCI = 100 * (exp(beta + 1.96 * se) - 1)
  )
}

save_figure <- function(plot, name, width, height, dpi = 600) {
  ggsave(figure_file(paste0(name, ".png")), plot, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(figure_file(paste0(name, ".pdf")), plot, width = width, height = height, bg = "white")
}

theme_paper <- function(base_size = 8) {
  theme_bw(base_size = base_size, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      panel.border = element_rect(linewidth = 0.45, colour = "grey25", fill = NA),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size * 0.88, colour = "grey20"),
      axis.ticks = element_line(linewidth = 0.35, colour = "grey25"),
      strip.background = element_rect(fill = "grey96", colour = "grey35", linewidth = 0.35),
      strip.text = element_text(size = base_size, face = "bold"),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.background = element_blank(),
      plot.title = element_text(size = base_size * 1.05, face = "bold", hjust = 0),
      plot.tag = element_text(size = base_size * 1.15, face = "bold"),
      plot.margin = margin(4, 4, 4, 4)
    )
}
