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
ais_infographic_theme <- function() {
  theme_minimal() +
    theme(
      plot.background        = element_rect(fill = "#FCFCFA", color = NA),
      panel.background       = element_rect(fill = "#FCFCFA", color = NA),
      panel.grid             = element_blank(),
      panel.grid.major.y     = element_line(color = "grey90", linewidth = 0.4),
      legend.position        = "bottom",
      legend.justification   = c(1, 0),
      legend.key.width       = unit(1, "npc"),
      legend.margin          = margin(t = -10, r = 0, b = 0, l = 0),
      panel.spacing          = unit(0.1, "lines"),
      text                   = element_text(family = "sourcesans"),
      plot.title             = element_text(
        family = "sourcesans", size = 125, face = "bold",
        color = noaa_dark, hjust = 0, margin = margin(b = 6)
      ),
      plot.subtitle          = element_text(
        family = "merriweather", size = 90,
        color = noaa_grey, hjust = 0, margin = margin(b = 20)
      ),
      plot.caption           = element_text(
        family = "merriweather", size = 50,
        color = "#888888", hjust = 1, margin = margin(t = 13)
      ),
      axis.text.y            = element_text(
        color = noaa_dark, size = 85, face = "bold",
        hjust = 1, margin = margin(r = 10)
      ),
      axis.text.x            = element_text(
        color = noaa_grey, size = 80, margin = margin(t = 5)
      ),
      plot.margin            = margin(25, 25, 5, 25),
      legend.title           = element_text(family = "merriweather", face = "bold", size = 55),
      legend.text            = element_text(family = "merriweather", size = 50)
    )
}

# -------------------------------------------------------------------
# Ridgeline Plot
# -------------------------------------------------------------------
ais_ridgeline <- ggplot(fleet_agg_gear_month,
                        aes(x = month,
                            y = reorder(geartype_label, total_gear_hours, FUN = sum),
                            fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 1.9,
    rel_min_height = 0.01,
    alpha = 0.8,
    color = "#1a1a1a",
    size = 0.4
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    expand = c(0.04, 0)
  ) +
  scale_fill_gradientn(
    colors = alpha(c("#3B5F8F", "#5A8CAE", "#7FB8D4", "#FFD580", "#FFA040",
                     "#7FB8D4", "#A8D5E2", "#5A8CAE"), 1.0),
    name = NULL,
    guide = guide_colorbar(
      direction      = "horizontal",
      barwidth       = unit(0.755, "npc"),
      barheight      = unit(0.4, "cm"),
      title.position = "top",
      ticks          = FALSE,
      label          = FALSE
    )
  ) +
  labs(
    title    = "Seasonal AIS-Vessel fishing effort in Chumash Heritage National Marine Sanctuary",
    subtitle = "Gear types vary in when and how dramatically they peak throughout the year.",
    x        = NULL,
    y        = NULL,
    caption  = ""
  ) +
  ais_infographic_theme()

ais_ridgeline

ggsave(
  filename = "figures/ais_ridgeline.png",
  plot     = ais_ridgeline,
  width    = 14,
  height   = 8,
  dpi      = 600
)