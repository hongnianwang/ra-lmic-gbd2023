scripts <- c(
  "scripts/00_prepare_data.R",
  "scripts/01_figure_s1_trends.R",
  "scripts/02_maps_eapc.R",
  "scripts/03_figure_s9_sex_ratios.R",
  "scripts/04_inequality.R",
  "scripts/05_arima_forecast.R"
)

for (script in scripts) {
  message("\n==> Running ", script)
  source(script, echo = FALSE)
}

message("\nAll analyses completed. Outputs are in outputs/.")
