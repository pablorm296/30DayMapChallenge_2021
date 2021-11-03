# Set up ======================================================================
library(tidyverse)
library(sf)
library(extrafont)
library(patchwork)

# Data load ===================================================================

# Power stations location and info
power_stations <- read_sf("Data/PowerStations/centrales_electricas_100MW+.shp")

# Mexico administrative
mexico_admin <- read_sf("../Common/MarcoGeoestadístico/01_Entidades.shp")

# Ggplot themes ===============================================================
source("../Common/R/ggplot_themes.R")

# Data cleaning ===============================================================

# Get old column names
names(power_stations)

# Define new column names
new_col_names <- c("country", "station_name", "owner",
                   "operator", "lat", "lon", "city", "municipality", "state",
                   "pc", "addr", "total_mw", "green_mw", "primary_source_of_energy",
                   "primary_source_of_green_energy",
                   "coal_mw", "naturalg_mw", "oil_mw", "other_mw", "hydro_mw", "pumped_mw",
                   "nuclear_mw", "solar_mw", "wind_mw", "geo_mw", "bio_mw", "tide_mw",
                   "source", "date", "geometry")

# Set new col names
names(power_stations) <- new_col_names

# Keep only power stations in Mexico
power_stations |>
  filter(country == "México") -> power_stations

# Make nuclear a renewable energy
power_stations |>
  mutate(green_mw = if_else(primary_source_of_energy == "Nuclear", 
                            total_mw, 
                            green_mw)) -> power_stations

# Make a dummy var to identify green energies
power_stations |>
  mutate(type = if_else(green_mw > 0,
                        "Energías renovables + nuclear",
                        "Energías no renovables")) -> power_stations

# Aux objects =================================================================

# Init container
aux_objects <- list()

# Colors
aux_objects$colors_renewable_nonrenewable <- c("Energías no renovables" = "#f06292",
                                               "Energías renovables + nuclear" = "#4db6ac")

# Aux plots ===================================================================

# Init container
aux_plots <- list()

# MW by type of energy
power_stations |>
  ggplot(aes(x = type, colour = type, fill = type)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
           alpha = 0.75) +
  scale_colour_manual(values = aux_objects$colors_renewable_nonrenewable) +
  scale_fill_manual(values = aux_objects$colors_renewable_nonrenewable) +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, by = 0.1),
                     labels = scales::percent) +
  labs(title = "Proporción de la capacidad instalada por tipo de energía.",
       x = "Tipo de energía",
       y = "% de la capacidad instalada",
       colour = "Tipo de energía",
       fill = "Tipo de energía") +
  challenge_theme() -> aux_plots$bar_type_of_energy

# MW by type of station and primary source of energy
power_stations |>
  ggplot(aes(x = fct_infreq(primary_source_of_energy), 
             colour = type, 
             fill = type)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
           alpha = 0.75, position = "dodge") +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.05),
                     labels = scales::percent) +
  scale_colour_manual(values = aux_objects$colors_renewable_nonrenewable) +
  scale_fill_manual(values = aux_objects$colors_renewable_nonrenewable) +
  labs(title = "Proporción de la capacidad instalada por fuente primaria y tipo de enería.",
       x = "Fuente primaria de energía",
       y = "% de la capacidad instalada",
       colour = "Tipo de energía",
       fill = "Tipo de energía") +
  challenge_theme() -> aux_plots$bar_primary_source

# Maps ========================================================================

# Get crs of mexico administrative shapefile
st_crs(mexico_admin) -> mex_crs

# Set new crs for power stations shapefile
power_stations |>
  st_transform(mex_crs) -> power_stations

# Create map
ggplot() +
  geom_sf(colour = "#f5f5f5",
          fill = "#e0e0e0",
          size = 0.25,
          data = mexico_admin) + 
  geom_sf(aes(colour = type, size = total_mw),
          alpha = 0.5,
          data = power_stations) +
  scale_size(limits = c(100, 2778.4), breaks = seq(100, 2000, by = 500),
             range = c(1, 10)) +
  scale_colour_manual(values = aux_objects$colors_renewable_nonrenewable) +
  scale_fill_manual(values = aux_objects$colors_renewable_nonrenewable) +
  labs(title = "¿Dónde están las centrales eléctricas de México?",
       subtitle = "Por tipo de energía y capacidad (MW)",
       colour = "Tipo de energía",
       size = "Capacidad (MW)") +
  challenge_points_theme() -> map_power_stations

# Arrange plots using patchwork <3
map_power_stations / (aux_plots$bar_type_of_energy | aux_plots$bar_primary_source) + 
  plot_layout(heights = c(3,1)) +
  plot_annotation(caption = str_c("Datos: Centrales eléctricas con capacidad mayor o igual a 100MW. Cooperación de América del Norte en Información Energética (CANIE).",
                                  "Elaboración: Pablo R. (Twitter: @_poolish_).",
                                  sep = "\n"),
                  theme = challenge_theme()) -> final

# Save final plot
ggsave(filename = "Out/01.png", plot = final, width = 3840, height = 2160, units = "px",
       scale = 1.25)
