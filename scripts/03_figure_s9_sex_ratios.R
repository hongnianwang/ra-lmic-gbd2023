source(file.path("R", "paths.R"))
source(file.path("R", "helpers.R"))

message("Drawing Figure S9...")

ratio_data <- read_ra_data() %>%
  filter(
    location %in% GNI_ORDER,
    sex %in% c("Female", "Male"),
    age %in% age_order(),
    metric == "Rate",
    year == 2023,
    measure %in% MEASURE_ORDER
  ) %>%
  select(measure, location, sex, age, val) %>%
  tidyr::pivot_wider(names_from = sex, values_from = val) %>%
  mutate(
    ratio = if_else(!is.na(Male) & Male > 0, Female / Male, NA_real_),
    age = factor(age, levels = age_order()),
    location = factor(location, levels = GNI_ORDER),
    measure = factor(measure, levels = MEASURE_ORDER)
  )

data.table::fwrite(ratio_data, table_file("Figure_S9_female_to_male_ratios.csv"))

plot_data <- ratio_data %>%
  filter(is.finite(ratio))

make_panel <- function(measure_name, tag) {
  ggplot(plot_data %>% filter(measure == !!measure_name), aes(age, ratio, color = location, group = location)) +
    geom_hline(yintercept = 1, linewidth = 0.35, linetype = "dashed", colour = "grey45") +
    geom_line(linewidth = 0.78) +
    geom_point(size = 1.7, stroke = 0.2) +
    scale_color_manual(values = GNI_COLORS, labels = GNI_LABELS) +
    scale_y_log10(breaks = c(0.5, 1, 2, 4, 8, 12), labels = label_number(accuracy = 0.1)) +
    labs(tag = tag, title = measure_name, x = NULL, y = "Female-to-male ratio (log scale)", color = NULL) +
    theme_paper(8.2) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.6),
      plot.title = element_text(size = 8.6, face = "plain", hjust = 0.5),
      plot.margin = margin(10, 4, 2, 4),
      plot.tag.position = c(0.015, 1.02),
      plot.tag = element_text(size = 10, face = "bold", hjust = 0, vjust = 0),
      legend.position = "bottom",
      legend.box.margin = margin(-4, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0)
    )
}

p <- wrap_plots(
  make_panel("Prevalence", "A"),
  make_panel("Incidence", "B"),
  make_panel("Deaths", "C"),
  make_panel("DALYs", "D"),
  ncol = 2,
  guides = "collect"
) &
  theme(legend.position = "bottom", legend.box.margin = margin(-4, 0, 0, 0)) &
  guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 0.8, size = 2.2)))

save_figure(p, "Figure_S9_female_to_male_ratios", width = 7.2, height = 5.7)
message("Saved Figure S9.")
