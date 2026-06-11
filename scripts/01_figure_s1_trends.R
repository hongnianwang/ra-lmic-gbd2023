source(file.path("R", "paths.R"))
source(file.path("R", "helpers.R"))

message("Drawing Figure S1...")

df <- read_ra_data() %>%
  filter(
    sex == "Both",
    age == "All ages",
    metric == "Rate",
    location %in% c("All location", GNI_ORDER)
  ) %>%
  mutate(
    measure = factor(measure, levels = MEASURE_ORDER),
    location = factor(location, levels = c("All location", GNI_ORDER))
  )

loc_colors <- c("All location" = "#2B2B2B", GNI_COLORS)
loc_shapes <- c("All location" = 21, "GNI-L" = 22, "GNI-LM" = 24, "GNI-UM" = 23)
loc_lty <- c("All location" = "solid", "GNI-L" = "42", "GNI-LM" = "33", "GNI-UM" = "solid")

panel_titles <- c(
  "Prevalence" = "A. Prevalence rate",
  "Incidence" = "B. Incidence rate",
  "Deaths" = "C. Death rate",
  "DALYs" = "D. DALY rate"
)

p <- ggplot(df, aes(year, val, color = location, linetype = location)) +
  geom_line(linewidth = 0.65) +
  geom_point(
    data = df %>% filter(year %in% c(1990, 2000, 2010, 2023)),
    aes(shape = location),
    size = 1.25,
    fill = "white",
    stroke = 0.45
  ) +
  facet_wrap(~measure, scales = "free_y", ncol = 2, labeller = as_labeller(panel_titles)) +
  scale_color_manual(values = loc_colors, labels = c("All location" = "All locations", GNI_LABELS)) +
  scale_linetype_manual(values = loc_lty, labels = c("All location" = "All locations", GNI_LABELS)) +
  scale_shape_manual(values = loc_shapes, labels = c("All location" = "All locations", GNI_LABELS)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023)) +
  labs(x = "Year", y = "All-age rate per 100,000 population", color = NULL, linetype = NULL, shape = NULL) +
  theme_paper(8) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(
    color = guide_legend(nrow = 1, override.aes = list(linewidth = 0.7)),
    linetype = "none",
    shape = "none"
  )

save_figure(p, "Figure_S1_trends_by_GNI", width = 7.2, height = 5.2)
message("Saved Figure S1.")

