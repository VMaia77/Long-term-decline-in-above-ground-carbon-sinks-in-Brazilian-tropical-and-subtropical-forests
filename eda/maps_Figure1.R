# ============================
# Load packages
# ============================

library(ggplot2)
library(dplyr)
library(tidyr)
library(rworldmap)
library(raster)
library(sf)
library(multcomp)
library(geobr)
library(cowplot)
library(gridExtra)
library(ggspatial)
library(viridis)
library(scico)

source("./modelling/model_pipeline.R")

# ============================
# Global theme (journal-friendly)
# ============================

theme_set(
  theme_void(base_family = "Helvetica", base_size = 7)
)

# ============================
# Read Brazil map
# ============================

br1 <- read_country(
  year = 2010,
  simplified = TRUE,
  showProgress = TRUE
)

br1_crs <- st_transform(br1, crs = 4326)

# ============================
# Read and process data
# ============================

data_sites <- read.table(
  "census_int_level.csv",
  header = TRUE,
  dec = ".",
  sep = ","
)

data_sites <- pipeline_modelling(
  data_sites,
  is_census = FALSE
)

data_sites_agg <- data_sites %>%
  group_by(plot_code) %>%
  summarise(
    lat = mean(lat),
    long = mean(long),
    warm_dry_climate = mean(warm_dry_climate),
    .groups = "drop"
  )

# ============================
# Color-blind-safe palette (NO pinks)
# ============================

colours_palletes <- scico(
  20,
  palette = "lajolla"
)

# ============================
# Figure 1 — Brazil overview
# ============================

plot1 <- ggplot(data = br1_crs) +
  geom_sf(
    fill = "white",
    linewidth = 0.4,
    colour = "black"
  ) +
  geom_point(
    data = data_sites_agg,
    aes(x = long, y = lat, colour = warm_dry_climate),
    size = 1.4,
    shape = 16
  ) +
  scale_colour_gradientn(
    colours = colours_palletes
  ) +
  geom_rect(
    aes(
      xmin = -53,
      xmax = -41,
      ymin = -31,
      ymax = -11
    ),
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  theme(legend.position = "none")

# ============================
# Figure 1 — Zoom panel
# ============================

zoom_plot <- ggplot(data = br1_crs) +
  geom_sf(
    fill = "white",
    linewidth = 0.4,
    colour = "black"
  ) +
  geom_point(
    data = data_sites_agg,
    aes(x = long, y = lat, colour = warm_dry_climate),
    size = 2.3
  ) +
  scale_colour_gradientn(
    colours = colours_palletes,
    breaks = c(-3, -2, 0, 2),
    labels = c("Cold–wet", "", "", "Warm–dry"),
    guide = guide_colorbar(
      barheight = unit(30, "mm"),
      barwidth  = unit(3, "mm")
    )
  ) +
  labs(colour = "Climate gradient") +
  coord_sf(
    xlim = c(-53, -41),
    ylim = c(-31, -12),
    expand = FALSE
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.3,
    text_cex = 0.6,
    line_width = 0.4
  ) +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.4
    ),
    legend.key.size = unit(0.4, "cm")
  )

# ============================
# Combine panels
# ============================

grid_figure <- grid.arrange(
  plot1,
  zoom_plot,
  ncol = 2,
  widths = c(1.3, 1.3)
)

# ============================
# Save as vector PDF (editable)
# ============================

ggsave(
  filename = "figure1.pdf",
  plot = grid_figure,
  width = 180,
  height = 120,
  units = "mm",
  device = cairo_pdf
)
