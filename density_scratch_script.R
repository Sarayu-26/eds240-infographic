
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
# Density Plot
# -------------------------------------------------------------------
land <- ne_download(
  scale = 10,
  type = "land",
  category = "physical",
  returnclass = "sf"
) %>%
  st_transform(4326)

ca_bbox     <- st_as_sfc(st_bbox(c(xmin = -125, xmax = -114, ymin = 32, ymax = 42), crs = 4326))
california  <- st_intersection(land, ca_bbox)
chnms_bbox  <- st_as_sfc(st_bbox(boundary)) %>% st_buffer(0.1)

fleet_ocean    <- fleet_sf[!st_intersects(fleet_sf, california, sparse = FALSE), ]
fleet_ocean_df <- st_drop_geometry(fleet_ocean)

landmarks_coast <- tibble(
  name = c("Point Conception", "Gaviota", "Lompoc", "San Luis Obispo"),
  lon  = c(-120.4715, -120.2139, -120.4579, -120.6596),
  lat  = c(34.4486, 34.4717, 34.6392, 35.2828)
)

landmarks_seamount <- tibble(
  name = c("Rodriguez Seamount"),
  lon  = c(-121.0667),
  lat  = c(34.0500)
)

landmarks_marine <- tibble(
  name = c("Arguello Canyon", "Arguello Terrace", "Santa Lucia Bank"),
  lon  = c(-120.8500, -120.9500, -121.2000),
  lat  = c(34.3000, 34.5500, 34.6000)
)

dot_layer <- function(df) {
  geom_point(
    data   = df,
    aes(x = lon, y = lat),
    shape  = 21,
    size   = 1.5,
    fill   = noaa_pale,
    color  = noaa_dark,
    stroke = 0.8
  )
}

density_plot <- ggplot() +
  geom_sf(data = california, fill = "#F0EDE8", color = "grey60", linewidth = 0.4) +
  geom_sf(data = boundary, fill = alpha(noaa_mid, 0.05), color = noaa_dark, linewidth = 0.7) +
  stat_density_2d(
    data    = fleet_ocean_df,
    aes(x = cell_ll_lon, y = cell_ll_lat,
        weight = hours,
        fill   = after_stat(level),
        alpha  = after_stat(level)),
    geom    = "polygon",
    contour = TRUE
  ) +
  geom_sf(data = california, fill = "#F0EDE8", color = "grey60", linewidth = 0.4) +
  geom_sf(data = boundary, fill = NA, color = noaa_dark, linewidth = 0.7) +
  dot_layer(landmarks_coast) +
  dot_layer(landmarks_seamount) +
  dot_layer(landmarks_marine) +
  scale_fill_viridis_c(
    option    = "viridis",
    direction = -1,
    name      = "Fishing Effort Density",
    guide     = guide_colorbar(
      direction  = "horizontal",
      barwidth   = unit(0.5, "npc"),
      barheight  = unit(0.4, "cm"),
      title.position = "top",
      title.hjust    = 0,
      ticks      = FALSE
    )
  ) +
  scale_alpha(range = c(0.2, 0.9), guide = "none") +
  scale_x_continuous(
    breaks = seq(floor(st_bbox(chnms_bbox)["xmin"]),
                 ceiling(st_bbox(chnms_bbox)["xmax"]), by = 0.5),
    labels = function(x) paste0(abs(x), "°W")
  ) +
  scale_y_continuous(
    breaks = seq(floor(st_bbox(chnms_bbox)["ymin"]),
                 ceiling(st_bbox(chnms_bbox)["ymax"]), by = 0.25),
    labels = function(y) paste0(y, "°N")
  ) +
  labs(
    title    = "AIS-Vessel fishing effort density in Chumash Heritage National Marine Sanctuary",
    subtitle = "Fishing effort density is concentrated nearshore and around Point Conception",
    caption  = ""
  ) +
  ais_infographic_theme() +
  theme(
    panel.background = element_rect(fill = "#D6EAF8", color = NA),
    panel.border     = element_rect(fill = NA, color = "grey50", linewidth = 0.5),
    panel.grid.major = element_line(color = "grey93", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.title       = element_blank(),
    axis.text.x      = element_text(family = "merriweather", size = 60, color = noaa_dark),
    axis.text.y      = element_text(family = "merriweather", size = 60, color = noaa_dark),
    legend.position        = "bottom",
    legend.justification   = "center",
    legend.key.width       = unit(0.5, "npc"),
    legend.margin          = margin(t = 30, r = 0, b = 0, l = 0),
    legend.title     = element_text(
      family = "merriweather", face = "bold", size = 70,
      color = noaa_dark
    ),
    legend.text      = element_text(family = "merriweather", size = 60, color = "grey30")
  ) +
  coord_sf(
    xlim   = c(st_bbox(chnms_bbox)["xmin"] - 0.3, st_bbox(chnms_bbox)["xmax"] + 0.3),
    ylim   = c(st_bbox(chnms_bbox)["ymin"] - 0.3, st_bbox(chnms_bbox)["ymax"] + 0.3),
    expand = FALSE
  )
density_plot

ggsave(
  filename = "figures/ais_density_plot.png",
  plot     = density_plot,
  width    = 14,
  height   = 10,
  dpi      = 600
)