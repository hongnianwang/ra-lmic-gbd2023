source(file.path("R", "paths.R"))
source(file.path("R", "helpers.R"))

suppressPackageStartupMessages({
  library(maps)
  library(RColorBrewer)
})

message("Drawing maps for Figure 1 and Figures S2-S8...")

country <- read_country_ra_data()
world <- ggplot2::map_data("world") %>%
  filter(region != "Antarctica")

map_measure_label <- function(measure) {
  c(
    "Prevalence" = "prevalence",
    "Incidence" = "incidence",
    "Deaths" = "death",
    "DALYs" = "DALY"
  )[measure]
}

format_break <- function(x) {
  formatC(x, format = "f", digits = 2)
}

format_interval_labels <- function(breaks) {
  paste0("(", format_break(head(breaks, -1)), ", ", format_break(tail(breaks, -1)), "]")
}

map_panel <- function(values, value_col, legend_title, palette, reverse = FALSE) {
  plot_data <- values %>%
    mutate(map_location = map_location_names(location)) %>%
    dplyr::select(map_location, value = all_of(value_col))

  if ("Saint Vincent" %in% plot_data$map_location) {
    plot_data <- bind_rows(
      plot_data,
      plot_data %>% filter(map_location == "Saint Vincent") %>% mutate(map_location = "Grenadines")
    )
  }

  total <- left_join(world, plot_data, by = c("region" = "map_location"), relationship = "many-to-many")
  vals <- total$value[!is.na(total$value)]
  breaks <- unique(as.numeric(quantile(vals, probs = seq(0, 1, length.out = 8), na.rm = TRUE)))
  if (length(breaks) < 3) {
    breaks <- pretty(vals, n = 7)
  }

  labels <- format_interval_labels(breaks)
  if (length(labels) > 0) {
    labels[1] <- sub("^\\(", "[", labels[1])
  }

  total <- total %>%
    mutate(bin = cut(value, breaks = breaks, include.lowest = TRUE, labels = labels))

  colors <- brewer.pal(7, palette)
  if (reverse) colors <- rev(colors)
  colors <- colors[seq_along(levels(total$bin))]
  names(colors) <- levels(total$bin)

  base_map <- function(xlim = NULL, ylim = NULL, title = NULL, show_legend = FALSE) {
    p <- ggplot(total) +
      geom_polygon(aes(long, lat, group = group, fill = bin), colour = "black", linewidth = 0.22) +
      scale_fill_manual(values = colors, na.value = "grey85", drop = FALSE) +
      labs(title = title, fill = legend_title) +
      theme_bw(base_family = "sans") +
      theme(
        plot.title = element_text(size = 8.0, face = "plain", hjust = 0.5, margin = margin(t = 3, b = 6)),
        legend.position = if (show_legend) "inside" else "none",
        legend.position.inside = c(0.15, 0.25),
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(size = 7.5, hjust = 0),
        legend.text = element_text(size = 6.8),
        legend.key.height = grid::unit(0.34, "cm"),
        legend.key.width = grid::unit(0.46, "cm"),
        legend.spacing.y = grid::unit(0.07, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.84), colour = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(1, 1, 1, 1)
      ) +
      guides(fill = guide_legend(ncol = 2, title.position = "top"))

    if (is.null(xlim) || is.null(ylim)) {
      p + coord_quickmap(expand = FALSE)
    } else {
      p + coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE)
    }
  }

  inset_specs <- tibble(
    title = c("East Africa", "West Africa", "South Asia", "Southeast Asia", "Central America"),
    xlim = list(c(22, 55), c(-25, 20), c(55, 100), c(92, 145), c(-125, -60)),
    ylim = list(c(-15, 20), c(-5, 30), c(0, 40), c(-15, 30), c(-25, 35)),
    width = c(0.94, 1.25, 1.06, 1.17, 1.08)
  )
  inset_maps <- lapply(seq_len(nrow(inset_specs)), function(i) {
    base_map(inset_specs$xlim[[i]], inset_specs$ylim[[i]], inset_specs$title[i])
  })

  main <- base_map(show_legend = TRUE)
  main / wrap_plots(inset_maps, ncol = 5, widths = inset_specs$width) +
    plot_layout(heights = c(0.70, 0.30))
}

rate_map <- function(measure, figure_name) {
  values <- country %>%
    filter(measure == !!measure, sex == "Both", age == "Age-standardized", metric == "Rate", year == 2023) %>%
    dplyr::select(location, val)

  p <- map_panel(
    values,
    "val",
    paste0("Age-standardized ", map_measure_label(measure), " rate, 2023"),
    palette = "RdYlGn"
  )
  save_figure(p, figure_name, width = 10, height = 6)
}

pc_map <- function(measure, figure_name) {
  values <- country %>%
    filter(measure == !!measure, sex == "Both", age == "All ages", metric == "Number", year %in% c(1990, 2023)) %>%
    dplyr::select(location, year, val) %>%
    tidyr::pivot_wider(names_from = year, values_from = val, names_prefix = "case_") %>%
    mutate(val = if_else(case_1990 > 0, 100 * (case_2023 - case_1990) / case_1990, NA_real_)) %>%
    dplyr::select(location, val)

  data.table::fwrite(values, table_file(paste0("PC_", measure, ".csv")))

  p <- map_panel(
    values,
    "val",
    paste0("% change in ", map_measure_label(measure), " cases\n1990-2023"),
    palette = "RdYlGn"
  )
  save_figure(p, figure_name, width = 10, height = 6)
}

pc_table <- function(measure) {
  values <- country %>%
    filter(measure == !!measure, sex == "Both", age == "All ages", metric == "Number", year %in% c(1990, 2023)) %>%
    dplyr::select(location, year, val) %>%
    tidyr::pivot_wider(names_from = year, values_from = val, names_prefix = "case_") %>%
    mutate(val = if_else(case_1990 > 0, 100 * (case_2023 - case_1990) / case_1990, NA_real_)) %>%
    dplyr::select(location, val)

  data.table::fwrite(values, table_file(paste0("PC_", measure, ".csv")))
}

eapc_map <- function(measure, figure_name) {
  eapc <- country %>%
    filter(measure == !!measure, sex == "Both", age == "Age-standardized", metric == "Rate") %>%
    group_by(location) %>%
    group_modify(~calc_eapc(.x)) %>%
    ungroup()

  data.table::fwrite(eapc, table_file(paste0("EAPC_", measure, ".csv")))

  p <- map_panel(
    eapc,
    "EAPC",
    paste0("EAPC in age-standardized ", map_measure_label(measure), " rate\n1990-2023"),
    palette = "RdBu",
    reverse = FALSE
  )
  save_figure(p, figure_name, width = 10, height = 6)
}

eapc_map("DALYs", "Figure_1_EAPC_DALYs")
pc_table("DALYs")
pc_table("Prevalence")
pc_table("Incidence")
pc_table("Deaths")

rate_map("DALYs", "Figure_S2_DALY_rate_2023")
rate_map("Prevalence", "Figure_S3_Prevalence_rate_2023")
eapc_map("Prevalence", "Figure_S4_EAPC_Prevalence")
rate_map("Incidence", "Figure_S5_Incidence_rate_2023")
eapc_map("Incidence", "Figure_S6_EAPC_Incidence")
rate_map("Deaths", "Figure_S7_Deaths_rate_2023")
eapc_map("Deaths", "Figure_S8_EAPC_Deaths")

message("Saved map figures.")
