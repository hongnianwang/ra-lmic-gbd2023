source(file.path("R", "paths.R"))
source(file.path("R", "helpers.R"))

message("Running socioeconomic correlation and inequality analyses...")

country <- read_country_ra_data()
gni <- read_gni()
sdi <- read_sdi()
pop <- read_population()

burden_2023 <- country %>%
  filter(year == 2023, sex == "Both", age == "All ages", metric == "Rate") %>%
  dplyr::select(location, measure, rate = val) %>%
  left_join(gni, by = "location") %>%
  filter(!is.na(GNI), GNI > 0)

spearman_stats <- burden_2023 %>%
  group_by(measure) %>%
  summarise(
    rho = suppressWarnings(cor(GNI, rate, method = "spearman", use = "complete.obs")),
    p_value = suppressWarnings(cor.test(GNI, rate, method = "spearman", exact = FALSE)$p.value),
    n = n(),
    .groups = "drop"
  )
data.table::fwrite(spearman_stats, table_file("spearman_gni_burden_2023.csv"))

short_rate_label <- function(measure) {
  c(
    "Prevalence" = "Prevalence rate",
    "Incidence" = "Incidence rate",
    "Deaths" = "Death rate",
    "DALYs" = "DALY rate"
  )[measure]
}

compact_panel_theme <- function() {
  theme(
    axis.title = element_text(size = 7.8),
    axis.text = element_text(size = 7.0),
    plot.tag.position = c(0.01, 1.055),
    plot.tag = element_text(size = 11, face = "bold", hjust = 0, vjust = 0.5),
    plot.margin = margin(11, 2, 3, 2)
  )
}

make_corr_panel <- function(measure, tag) {
  dat <- burden_2023 %>% filter(measure == !!measure)
  stat <- spearman_stats %>% filter(measure == !!measure)
  p_label <- ifelse(stat$p_value < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", stat$p_value)))
  label <- sprintf("paste(\"Spearman's \", rho, \" = %.3f, \", italic(p), \" %s\")", stat$rho, p_label)

  ggplot(dat, aes(GNI, rate)) +
    geom_point(aes(color = GNI_Class), size = 1.6, alpha = 0.82) +
    geom_smooth(method = "lm", formula = y ~ log10(x), se = FALSE, linewidth = 0.55, colour = "grey25") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, linewidth = 0.6, linetype = "dashed", colour = "grey25", fill = "grey70", alpha = 0.18) +
    annotate("text", x = min(dat$GNI, na.rm = TRUE), y = max(dat$rate, na.rm = TRUE), label = label, parse = TRUE, hjust = 0, vjust = 1, size = 2.5) +
    scale_x_log10(labels = label_comma()) +
    scale_color_manual(name = "GNI class", values = GNI_COLORS, drop = FALSE) +
    labs(tag = tag, x = "GNI per capita (log scale)", y = short_rate_label(measure)) +
    theme_paper(8.2) +
    compact_panel_theme() +
    theme(legend.position = "none")
}

inequality_data <- country %>%
  filter(year %in% c(1990, 2023), sex == "Both", age == "All ages", measure %in% MEASURE_ORDER) %>%
  left_join(sdi, by = c("location", "year")) %>%
  left_join(pop, by = c("location", "sex", "year")) %>%
  filter(!is.na(sdi), !is.na(population))

population_size_breaks <- c(5000, 10000)
population_size_limit <- max(inequality_data$population / 1e5, population_size_breaks, na.rm = TRUE)
annotation_text_size <- 2.5
annotation_lineheight <- 0.95
annotation_x <- 0.05

legend_population_size <- function(x) {
  sqrt(x / population_size_limit) * 6.5
}

rank_for_inequality <- function(measure) {
  inequality_data %>%
    filter(measure == !!measure) %>%
    group_by(year, metric) %>%
    arrange(sdi, .by_group = TRUE) %>%
    mutate(
      pop_global = sum(population, na.rm = TRUE),
      cummu = cumsum(population),
      midpoint = cummu - population / 2,
      weighted_order = midpoint / pop_global
    ) %>%
    ungroup()
}

fit_sii <- function(df) {
  fit <- MASS::rlm(val ~ weighted_order, data = df)
  co <- coef(summary(fit))["weighted_order", ]
  estimate <- unname(co["Value"])
  se <- unname(co["Std. Error"])
  tibble(
    SII = estimate,
    LCI = estimate - 1.96 * se,
    UCI = estimate + 1.96 * se
  )
}

calc_ci_value <- function(df) {
  df <- df %>%
    arrange(sdi) %>%
    mutate(
      pop_global = sum(population, na.rm = TRUE),
      cummu = cumsum(population),
      frac_burden = cumsum(val) / sum(val, na.rm = TRUE),
      frac_population = cummu / pop_global
    )
  auc <- sum(diff(c(0, df$frac_population)) * (head(c(0, df$frac_burden), -1) + df$frac_burden) / 2, na.rm = TRUE)
  1 - 2 * auc
}

calc_ci <- function(df) {
  tibble(CI = calc_ci_value(df))
}

ineq_stats <- lapply(MEASURE_ORDER, function(measure) {
  ranked <- rank_for_inequality(measure)
  sii <- ranked %>%
    filter(metric == "Rate") %>%
    group_by(year) %>%
    group_modify(~ fit_sii(.x)) %>%
    ungroup() %>%
    mutate(measure = measure)

  ci <- ranked %>%
    filter(metric == "Number") %>%
    group_by(year) %>%
    group_modify(~ calc_ci(.x)) %>%
    ungroup() %>%
    mutate(measure = measure)

  left_join(sii, ci, by = c("measure", "year"))
}) %>%
  bind_rows() %>%
  dplyr::select(measure, year, SII, LCI, UCI, CI)

data.table::fwrite(ineq_stats, table_file("inequality_sii_ci.csv"))

make_sii_panel <- function(measure, tag) {
  ranked <- rank_for_inequality(measure) %>%
    filter(metric == "Rate") %>%
    mutate(year = factor(year))
  label_text <- ineq_stats %>%
    filter(measure == !!measure) %>%
    mutate(
      year = factor(year, levels = c(1990, 2023)),
      text = sprintf("%s: SII = %.2f (%.2f to %.2f)", year, SII, LCI, UCI)
    ) %>%
    arrange(year) %>%
    pull(text) %>%
    paste(collapse = "\n")

  ggplot(ranked, aes(weighted_order, val, color = year, fill = year)) +
    geom_point(aes(size = population / 1e5), alpha = 0.72, shape = 21, stroke = 0.25) +
    geom_smooth(method = MASS::rlm, formula = y ~ x, se = FALSE, linewidth = 0.55, alpha = 0.75) +
    annotate(
      "text",
      x = annotation_x,
      y = max(ranked$val, na.rm = TRUE),
      label = label_text,
      hjust = 0,
      vjust = 1,
      size = annotation_text_size,
      lineheight = annotation_lineheight
    ) +
    scale_color_manual(name = "Year", values = YEAR_COLORS) +
    scale_fill_manual(values = YEAR_COLORS, guide = "none") +
    scale_size_area(
      name = "Population/100,000",
      max_size = 6.5,
      limits = c(0, population_size_limit),
      breaks = population_size_breaks,
      labels = label_comma()
    ) +
    labs(tag = tag, x = "Relative rank by SDI", y = short_rate_label(measure)) +
    theme_paper(8.2) +
    compact_panel_theme() +
    theme(legend.position = "none")
}

make_ci_panel <- function(measure, tag) {
  ranked <- rank_for_inequality(measure) %>%
    filter(metric == "Number") %>%
    group_by(year) %>%
    arrange(sdi, .by_group = TRUE) %>%
    mutate(
      frac_burden = cumsum(val) / sum(val, na.rm = TRUE),
      frac_population = cummu / pop_global,
      year = factor(year)
    ) %>%
    ungroup()
  stats <- ineq_stats %>%
    filter(measure == !!measure) %>%
    mutate(
      year = factor(year, levels = c(1990, 2023)),
      text = sprintf("%s: CI = %.3f", year, CI)
    )
  label_text <- stats %>%
    arrange(year) %>%
    pull(text) %>%
    paste(collapse = "\n")

  ggplot(ranked, aes(frac_population, frac_burden, color = year)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.35, colour = "grey45") +
    geom_point(aes(size = population / 1e5, fill = year), alpha = 0.72, shape = 21, stroke = 0.25) +
    geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 4), se = FALSE, linewidth = 0.75) +
    annotate(
      "text",
      x = annotation_x,
      y = 0.95,
      label = label_text,
      hjust = 0,
      vjust = 1,
      size = annotation_text_size,
      lineheight = annotation_lineheight
    ) +
    scale_color_manual(name = "Year", values = YEAR_COLORS) +
    scale_fill_manual(values = YEAR_COLORS, guide = "none") +
    scale_size_area(
      name = "Population/100,000",
      max_size = 6.5,
      limits = c(0, population_size_limit),
      breaks = population_size_breaks,
      labels = label_comma()
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    labs(tag = tag, x = "Cumulative fraction of population ranked by SDI", y = paste0("Cumulative fraction of ", tolower(measure_label(measure)))) +
    theme_paper(8.2) +
    compact_panel_theme() +
    theme(legend.position = "none")
}

make_figure2_legend <- function() {
  gni <- tibble(
    label = names(GNI_COLORS),
    color = unname(GNI_COLORS),
    x = 0.20,
    y = c(0.86, 0.83, 0.8)
  )
  years <- tibble(
    label = names(YEAR_COLORS),
    color = unname(YEAR_COLORS),
    x = 0.20,
    y = c(0.36, 0.33)
  )
  sizes <- tibble(
    label = label_comma()(population_size_breaks),
    size = legend_population_size(population_size_breaks),
    x = 0.20,
    y = c(0.24, 0.21)
  )

  ggplot() +
    annotate("text", x = 0.02, y = 0.90, label = "GNI class", hjust = 0, vjust = 0.5, size = 2.45, fontface = "bold") +
    geom_point(data = gni, aes(x, y), size = 2.8, color = gni$color) +
    geom_text(data = gni, aes(x + 0.13, y, label = label), hjust = 0, vjust = 0.5, size = 2.35) +
    annotate("text", x = 0.02, y = 0.40, label = "Year", hjust = 0, vjust = 0.5, size = 2.45, fontface = "bold") +
    geom_segment(data = years, aes(x = x - 0.06, xend = x + 0.06, y = y, yend = y), linewidth = 0.75, color = years$color) +
    geom_point(data = years, aes(x, y), size = 2.7, shape = 21, stroke = 0.3, color = years$color, fill = years$color, alpha = 1) +
    geom_text(data = years, aes(x + 0.13, y, label = label), hjust = 0, vjust = 0.5, size = 2.35) +
    annotate("text", x = 0.02, y = 0.28, label = "Population/100,000", hjust = 0, vjust = 0.5, size = 2.35, fontface = "bold") +
    geom_point(data = sizes, aes(x, y, size = size), shape = 21, color = "grey25", fill = "white", stroke = 0.45) +
    geom_text(data = sizes, aes(x + 0.13, y, label = label), hjust = 0, vjust = 0.5, size = 2.25) +
    scale_size_identity() +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void(base_family = "sans") +
    theme(plot.margin = margin(13, 0, 4, 8))
}

figure_design <- "
ABL
CDL
EFL
"

figure2 <- wrap_plots(
  A = make_corr_panel("Prevalence", "A"),
  B = make_corr_panel("Deaths", "B"),
  C = make_sii_panel("Prevalence", "C"),
  D = make_sii_panel("Deaths", "D"),
  E = make_ci_panel("Prevalence", "E"),
  F = make_ci_panel("Deaths", "F"),
  L = make_figure2_legend(),
  design = figure_design,
  widths = c(1, 1, 0.39),
  heights = c(1, 1, 1)
)
save_figure(figure2, "Figure_2_socioeconomic_gradients_prevalence_deaths", width = 9.15, height = 8.9)

figure_s10 <- wrap_plots(
  A = make_corr_panel("Incidence", "A"),
  B = make_corr_panel("DALYs", "B"),
  C = make_sii_panel("Incidence", "C"),
  D = make_sii_panel("DALYs", "D"),
  E = make_ci_panel("Incidence", "E"),
  F = make_ci_panel("DALYs", "F"),
  L = make_figure2_legend(),
  design = figure_design,
  widths = c(1, 1, 0.39),
  heights = c(1, 1, 1)
)
save_figure(figure_s10, "Figure_S10_socioeconomic_gradients_incidence_DALYs", width = 9.15, height = 8.9)

message("Saved Figure 2 and Figure S10.")
