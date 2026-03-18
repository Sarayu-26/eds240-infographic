library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(forcats)
library(tidyverse)
library(ggridges)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(stringr)
library(monochromeR)
library(showtext)
library(glue)

# -------------------------------------------------------------------
# Loading Data
# -------------------------------------------------------------------
fleet <- read.csv("data/fleet_chnms_2017_2024.csv")

boundary <- st_read("data/chnms_designated_boundary/CHNMS_py.shp") %>%
  st_transform(4326)

# -------------------------------------------------------------------
# Wrangle Data for Visualizations
# -------------------------------------------------------------------
fleet_sf <- st_as_sf(fleet, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326, remove = FALSE)

fleet_agg_fishing <- fleet %>%
  group_by(cell_ll_lat, cell_ll_lon) %>%
  summarise(total_hours = sum(hours, na.rm = TRUE), .groups = "drop")

mean_hours_gear <- fleet %>%
  filter(geartype_label != "Fishing") %>%
  group_by(geartype_label, year) %>%
  summarise(annual_hours = sum(hours, na.rm = TRUE), .groups = "drop") %>%
  filter(annual_hours > 0) %>%
  group_by(geartype_label) %>%
  summarise(mean_hours = mean(annual_hours, na.rm = TRUE))

fleet_agg_gear <- fleet %>%
  filter(geartype_label != "Fishing") %>%
  group_by(geartype_label) %>%
  summarise(total_fishing_hours = sum(hours, na.rm = TRUE), .groups = "drop") %>%
  left_join(mean_hours_gear, by = "geartype_label") %>%
  mutate(
    mean_hours     = replace_na(mean_hours, 0),
    geartype_label = str_wrap(geartype_label, width = 8),
    geartype_label = fct_reorder(geartype_label, total_fishing_hours)
  )

fleet_agg_gear_month <- fleet %>%
  filter(geartype_label != "Fishing") %>%
  group_by(geartype_label) %>%
  mutate(total_gear_hours = sum(fishing_hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    geartype_label = fct_reorder(
      geartype_label,
      total_gear_hours,
      .desc = TRUE
    )
  )


# -------------------------------------------------------------------
# Fonts
# -------------------------------------------------------------------
font_add_google(name = "Source Sans 3", family = "sourcesans")
font_add_google(name = "Merriweather", family = "merriweather")
showtext_auto()

# -------------------------------------------------------------------
# NOAA Color Palette
# -------------------------------------------------------------------
noaa_dark <- "#0D1B2A"
noaa_mid  <- "#5A8CAE"
noaa_pale <- "#D6EAF8"
noaa_grey <- "#5A6670"

# -------------------------------------------------------------------
# AIS Infographic Theme
# -------------------------------------------------------------------
circle_plot <- ggplot(fleet_agg_gear) +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 100, 500, 1000, 2500)),
    color = "lightgrey"
  ) +
  geom_col(
    aes(x = geartype_label, y = total_fishing_hours, fill = total_fishing_hours),
    position = "dodge2",
    show.legend = TRUE,
    alpha = 0.9,
    width = 0.6
  ) +
  geom_point(
    aes(x = geartype_label, y = mean_hours),
    size = 3,
    color = "gray12"
  ) +
  geom_segment(
    aes(x = geartype_label, xend = geartype_label, y = 0, yend = mean_hours),
    linetype = "dashed",
    color = "gray12"
  ) +
  coord_polar() +
  scale_fill_gradient(
    low   = "#C8D8E8",
    high  = "#1B3A5C",
    name  = "Total fishing hours",
    guide = guide_colorbar(
      direction      = "horizontal",
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.4, "cm"),
      title.position = "top",
      title.hjust    = 0,
      ticks          = FALSE
    )
  ) +
  scale_y_continuous(
    trans  = "log1p",
    labels = comma,
    breaks = c(0, 100, 500, 1000, 2500),
    name   = "Total Fishing Hours"
  ) +
  labs(
    title    = "AIS-Vessel fishing effort by gear type in Chumash Heritage National Marine Sanctuary",
    subtitle = " Pole and line and trawlers far outpace all other gear types.",
    caption  = "",
    x        = NULL
  ) +
  ais_infographic_theme() +
  theme(
    axis.title   = element_blank(),
    axis.ticks   = element_blank(),
    panel.grid   = element_blank(),
    legend.justification = "left",
    legend.margin = margin(t = 10),
    axis.text.x = element_text(
      family = "merriweather", size = 90,
      color = noaa_dark, lineheight = 0.3, face = "bold",
      margin = margin(t = 20)
    ),
    axis.text.y  = element_text(
      family = "merriweather", size = 90, color = "gray30"
    )
  )

circle_plot

ggsave(
  filename = "figures/ais_circle_plot.png",
  plot     = circle_plot,
  width    = 15,
  height   = 14,
  dpi      = 600
)
