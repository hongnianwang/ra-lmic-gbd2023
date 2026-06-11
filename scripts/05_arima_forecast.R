source(file.path("R", "paths.R"))
source(file.path("R", "helpers.R"))

suppressPackageStartupMessages({
  library(forecast)
})

message("Running ARIMA forecasts and drawing Figure 3/Figure S11...")

forecast_locations <- c("All location", GNI_ORDER)

series <- read_ra_data() %>%
  filter(
    location %in% forecast_locations,
    sex == "Both",
    age == "All ages",
    metric == "Rate",
    measure %in% MEASURE_ORDER
  ) %>%
  arrange(measure, location, year)

analysis_grid <- tidyr::expand_grid(
  measure = MEASURE_ORDER,
  location = forecast_locations
)

arima_label <- function(fit) {
  ord <- forecast::arimaorder(fit)
  sprintf("ARIMA(%d,%d,%d)", ord[1], ord[2], ord[3])
}

fit_one <- function(df) {
  df <- arrange(df, year)
  y <- ts(df$val, start = min(df$year), frequency = 1)

  fit <- forecast::auto.arima(
    y,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  fc <- forecast::forecast(fit, h = 2050 - max(df$year), level = 95)

  future <- tibble(
    year = seq(max(df$year) + 1, 2050),
    val = as.numeric(fc$mean),
    lower = as.numeric(fc$lower[, 1]),
    upper = as.numeric(fc$upper[, 1]),
    type = "Projection"
  )

  historical <- df %>%
    transmute(year, val, lower, upper, type = "Historical")

  ljung_box_lag <- min(10, length(residuals(fit)) - 1)
  lb <- Box.test(
    residuals(fit),
    lag = ljung_box_lag,
    type = "Ljung-Box"
  )

  diagnostics <- tibble(
    arima_model = arima_label(fit),
    arima_full_model = as.character(fit),
    selected_by = "auto.arima",
    AIC = AIC(fit),
    BIC = BIC(fit),
    Ljung_Box_lag = ljung_box_lag,
    Ljung_Box_p = lb$p.value
  )

  list(data = bind_rows(historical, future), diagnostics = diagnostics)
}

forecast_results <- split(analysis_grid, seq_len(nrow(analysis_grid))) %>%
  lapply(function(spec) {
    df <- series %>%
      filter(measure == spec$measure, location == spec$location)
    out <- fit_one(df)
    list(
      data = out$data %>% mutate(measure = spec$measure, location = spec$location),
      diagnostics = out$diagnostics %>% mutate(measure = spec$measure, location = spec$location)
    )
  })

forecast_data <- bind_rows(lapply(forecast_results, `[[`, "data")) %>%
  mutate(
    measure = factor(measure, levels = MEASURE_ORDER),
    location = factor(location, levels = forecast_locations),
    type = factor(type, levels = c("Historical", "Projection"))
  )
diagnostics <- bind_rows(lapply(forecast_results, `[[`, "diagnostics")) %>%
  mutate(
    measure = factor(measure, levels = MEASURE_ORDER),
    location = factor(location, levels = forecast_locations)
  ) %>%
  arrange(measure, location)

projection_summary <- forecast_data %>%
  filter(location %in% GNI_ORDER) %>%
  group_by(measure, location) %>%
  summarise(
    baseline_2023 = val[year == 2023],
    baseline_lower_2023 = lower[year == 2023],
    baseline_upper_2023 = upper[year == 2023],
    projection_2050 = val[year == 2050],
    projection_lower_2050 = lower[year == 2050],
    projection_upper_2050 = upper[year == 2050],
    crosses_below_zero = any(val[type == "Projection"] < 0),
    .groups = "drop"
  ) %>%
  mutate(
    projection_2050 = if_else(crosses_below_zero, NA_real_, projection_2050),
    projection_lower_2050 = if_else(crosses_below_zero, NA_real_, projection_lower_2050),
    projection_upper_2050 = if_else(crosses_below_zero, NA_real_, projection_upper_2050),
    display_digits = case_when(
      measure %in% c("Deaths", "Incidence") ~ 2L,
      TRUE ~ 1L
    ),
    baseline_2023_display = mapply(round, baseline_2023, display_digits),
    projection_2050_display = mapply(round, projection_2050, display_digits),
    change_percent = if_else(
      crosses_below_zero,
      NA_real_,
      100 * (projection_2050 - baseline_2023) / baseline_2023
    )
  ) %>%
  arrange(measure, location)

data.table::fwrite(forecast_data, table_file("arima_forecast_1990_2050.csv"))
data.table::fwrite(diagnostics, table_file("arima_model_diagnostics.csv"))
data.table::fwrite(projection_summary, table_file("arima_projection_summary_2023_2050.csv"))

plot_forecast <- function(measures, name) {
  dat <- forecast_data %>%
    filter(measure %in% measures, location %in% GNI_ORDER) %>%
    mutate(
      measure = factor(as.character(measure), levels = measures),
      plot_val = pmax(val, 0),
      plot_lower = pmax(lower, 0),
      plot_upper = pmax(upper, 0)
    )

  make_panel <- function(measure, tag) {
    ggplot(dat %>% filter(measure == !!measure), aes(year, plot_val, color = location, linetype = type)) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "grey45", linewidth = 0.35) +
      geom_ribbon(
        data = dat %>% filter(measure == !!measure, type == "Projection"),
        aes(x = year, ymin = plot_lower, ymax = plot_upper, fill = location, group = location),
        inherit.aes = FALSE,
        alpha = 0.14,
        colour = NA
      ) +
      geom_line(linewidth = 0.72) +
      annotate("text", x = 2023.7, y = Inf, label = "2023", hjust = 0, vjust = 1.25, size = 2.45, colour = "grey35") +
      scale_color_manual(values = GNI_COLORS, labels = GNI_LABELS) +
      scale_fill_manual(values = GNI_COLORS, labels = GNI_LABELS, guide = "none") +
      scale_linetype_manual(values = c("Historical" = "solid", "Projection" = "longdash")) +
      scale_x_continuous(breaks = seq(1990, 2050, by = 10)) +
      labs(tag = tag, x = "Year", y = "All-age rate per 100,000 population", color = NULL, linetype = NULL) +
      theme_paper(8) +
      theme(
        legend.position = "bottom",
        legend.box.margin = margin(-7, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(12, 4, 0, 4),
        plot.tag.position = c(0.015, 1.025),
        plot.tag = element_text(size = 10, face = "bold", hjust = 0, vjust = 0)
      )
  }

  p <- make_panel(measures[1], "A") | make_panel(measures[2], "B")
  p <- p + plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box.margin = margin(-7, 0, 0, 0))

  save_figure(p, name, width = 7.2, height = 3.55)
}

plot_forecast(
  c("Incidence", "Deaths"),
  "Figure_3_ARIMA_incidence_deaths"
)

plot_forecast(
  c("Prevalence", "DALYs"),
  "Figure_S11_ARIMA_prevalence_DALYs"
)

message("Saved Figure 3 and Figure S11.")
