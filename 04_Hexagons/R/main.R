# Set up ======================================================================
library(tidyverse)
library(sf)
library(extrafont)
library(patchwork)
library(units)

# Data load ===================================================================

# Read aribnb data
airbnb_data <- read_csv("Data/listings.csv.gz")

# GeoData load ================================================================

# Read shapefile with Mexico states
mex_admin_est_geo <- read_sf("../Common/MarcoGeoestadístico/01_Entidades.shp")

# Read shapefile with Mexico municipalities
mex_admin_mun_geo <- read_sf("../Common/MarcoGeoestadístico/02_Municipios.shp")

# Read H3 grids (8 and 6 resolution levels)
h3_8 <- read_sf("Out/H3-Ciudad_de_México-8.geojson")
h3_6 <- read_sf("Out/H3-Ciudad_de_México-6.geojson")

# Read colonias data
colonias <- read_sf("Data/Colonias/coloniascdmx.shp")

# Themes ======================================================================

source("../Common/R/ggplot_themes.R")

# CRS cleaning ================================================================

# Get INEGI CRS
inegi_crs <- st_crs(mex_admin_est_geo)

# Reproject h3 hex grids using INEGI's crs
h3_8 |>
  st_transform(inegi_crs) -> h3_8

h3_6 |>
  st_transform(inegi_crs) -> h3_6

# Reproject colonias
colonias |>
  st_transform(inegi_crs) -> colonias

# Data cleaning ===============================================================

# Keep useful variables
names(airbnb_data)

vars_to_keep <- c("id", "latitude", "longitude", "property_type", "price",
                  "neighbourhood_cleansed")
airbnb_data |>
  select(all_of(vars_to_keep)) -> airbnb_data

# Clean prices
airbnb_data |>
  mutate(price = str_remove_all(price, "[\\$\\,]")) |>
  mutate(price = as.numeric(price)) -> airbnb_data

# Get lat and long as points
airbnb_data |>
  st_as_sf(coords = c("longitude", "latitude")) -> airbnb_data

# Reproject points
airbnb_data |>
  st_set_crs(value = 4326) -> airbnb_data

airbnb_data |>
  st_transform(inegi_crs) -> airbnb_data

# Get intersection matrix with h3_6 and h3_8
intersection_h3_6 <- st_intersects(h3_6, airbnb_data, sparse = F)
intersection_h3_8 <- st_intersects(h3_8, airbnb_data, sparse = F)

# Define a function to get the id of the hex
get_hex_id <- function(x, hex_grid) {
  i <- which(x)
  
  if(length(i) != 1) {
    return(NA_character_)
  }
  
  id <- hex_grid$hex_id[i]
  return(id)
}

# Get id of hexagons at different resolutions
airbnb_data |>
  mutate(parent_hex_6 = apply(intersection_h3_6, 2, get_hex_id, hex_grid = h3_6),
         parent_hex_8 = apply(intersection_h3_8, 2, get_hex_id, hex_grid = h3_8)) -> airbnb_data

# Get airbnb data for resolution 6
airbnb_data |>
  filter(!is.na(parent_hex_6)) |>
  mutate(geometry = NULL) |>
  as_tibble() |>
  group_by(parent_hex_6) |>
  summarise(count = n(),
            price_mean = mean(price, na.rm = T)) |>
  ungroup() -> airbnb_data_6

# Get airbnb data for resolution 8
airbnb_data |>
  filter(!is.na(parent_hex_8)) |>
  mutate(geometry = NULL) |>
  as_tibble() |>
  group_by(parent_hex_8) |>
  summarise(count = n(),
            price_mean = mean(price, na.rm = T)) |>
  ungroup() -> airbnb_data_8

# Clean colonias data ----

# Set vars to keep
vars_to_keep <- c("id", "nombre")

colonias |>
  select(all_of(vars_to_keep)) -> colonias

# Clean names
colonias |>
  mutate(nombre = str_to_title(nombre)) |>
  mutate(nombre = str_replace_all(nombre, 
                                  pattern = regex("\\s(I[XV]|V?I{0,3})(\\s|$)", 
                                                  ignore_case = T), str_to_upper)) -> colonias

# Get intersection matrix with colonias
intersection_colonias <- st_intersects(colonias, airbnb_data, sparse = F)

# Define a function to get the name of the colonia
get_colonia_name <- function(x, colonias) {
  i <- which(x)
  
  if(length(i) != 1) {
    return(NA_character_)
  }
  
  id <- colonias$nombre[i]
  return(id)
}

# Get colonia for each airbnb
airbnb_data |>
  mutate(colonia = apply(intersection_colonias, 2, get_colonia_name, colonias = colonias)) -> airbnb_data

# Join H3 geo with data =======================================================

# Join h3 data with airbnb data
h3_6 |>
  left_join(airbnb_data_6, by = c("hex_id" = "parent_hex_6")) -> h3_6
h3_8 |>
  left_join(airbnb_data_8, by = c("hex_id" = "parent_hex_8")) -> h3_8

# Set count to 0 if count is NA
h3_6 |>
  mutate(count = if_else(is.na(count), 0L, count)) -> h3_6
h3_8 |>
  mutate(count = if_else(is.na(count), 0L, count)) -> h3_8

# Compute areas and densities
h3_6 |>
  mutate(area = st_area(geometry),
         density = count / set_units(area, "km^2"),
         land_value = price_mean / set_units(area, "m^2")) -> h3_6

h3_8 |>
  mutate(area = st_area(geometry),
         density = count / set_units(area, "km^2"),
         land_value = price_mean / set_units(area, "m^2")) -> h3_8

# Get aux plots ===============================================================

#Init container
aux_plots <- list()

# ¿Dónde están los airbnb? (delegaciones)
airbnb_data |>
  ggplot(aes(x = fct_infreq(neighbourhood_cleansed))) +
  geom_bar(colour = "#FF5A5F", fill = "#FF5A5F", alpha = 0.75) +
  scale_y_continuous(limits = c(0, 7400), breaks = seq(0, 7400, 1000),
                     labels = function(x) format(x, big.mark = " ")) +
  labs(title = "Número de alojamientos por alcaldía",
       x = "Alcaldía",
       y = "Alojamientos") +
  challenge_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> aux_plots$location_alcaldías

# Top colonias con más airbnbs
airbnb_data |>
  mutate(geometry = NULL) |>
  as_tibble() |>
  group_by(colonia) |>
  summarise(n = n()) |>
  ungroup() |>
  arrange(-n) |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(colonia, -n), y = n)) +
  geom_bar(stat = "identity",
           colour = "#FF5A5F", fill = "#FF5A5F", alpha = 0.75) +
  scale_y_continuous(limits = c(0, 740), breaks = seq(0, 740, 100),
                     labels = function(x) format(x, big.mark = " ")) +
  labs(title = "Las 15 colonias con más Airbnbs",
       x = "Colonia",
       y = "Alojamientos") +
  challenge_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> aux_plots$location_colonias

# Distribución precio
airbnb_data |>
  filter(price <= 7200, price > 0) |>
  ggplot(aes(x = price)) +
  geom_histogram(bins = 50,
                 colour = "#FF5A5F", fill = "#FF5A5F", alpha = 0.75) +
  scale_x_continuous(limits = c(0, 7200), breaks = seq(0, 7200, 500),
                     labels = function(x) (str_c("$", x, sep = ""))) +
  labs(title = "Distribución de precio por noche",
       x = "Precio",
       y = "Frecuencia") +
  challenge_theme() +
  annotate("label", x = 3500, y = 2000, label = str_c("Precio promedio: $1'199",
                                                      "Precio medio: $750", sep = "\n"),
           color = "#404040", family = "Roboto") -> aux_plots$hist_prices

# Colonias más caras
airbnb_data |>
  filter(price <= 7200, price > 0) |>
  mutate(geometry = NULL) |>
  as_tibble() |>
  group_by(colonia) |>
  summarise(price = mean(price, na.rm = T)) |>
  ungroup() |>
  arrange(-price) |>
  slice_head(n = 15) |>
  ggplot(aes(x = reorder(colonia, -price), y = price)) +
  geom_bar(stat = "identity",
           colour = "#FF5A5F", fill = "#FF5A5F", alpha = 0.75) +
  scale_y_continuous(limits = c(0, 5200), breaks = seq(0, 5200, 500),
                     labels = function(x) format(x, big.mark = " ")) +
  labs(title = "Las 15 colonias más caras",
       subtitle = "Precio promedio por noche",
       x = "Colonia",
       y = "Precio promedio") +
  challenge_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> aux_plots$prices_colonias

# Make map ====================================================================

# Get municipalities of mexico city
cdmx_mun <- mex_admin_mun_geo |>
  filter(CVE_ENT == "09")

# Airbnb density
ggplot() +
  geom_sf(fill = NA, data = cdmx_mun) +
  geom_sf(aes(fill = density |> as.double()), 
          colour = NA, alpha = 0.75, size = 0.25,
          data = h3_8) +
  scale_fill_viridis_c(limits = c(0, 752), breaks = seq(0, 752, 100)) + 
  labs(title = "¿Dónde hay más Airbnbs en la CDMX?",
       subtitle = "Densidad de alojamientos por km² usando H3 en resolución 8\n(Uber’s Hexagonal Hierarchical Spatial Index)",
       fill = "Alojamientos por km²") +
  challenge_theme() -> map_density_8

((map_density_8 ) | (aux_plots$location_alcaldías / aux_plots$location_colonias)) +
  plot_layout(widths = c(1.5,1)) +
  plot_annotation(caption = str_c("Datos: Inside Airbnb (Nov 2021).",
                                  "Elaboración: Pablo R. (Twitter: @_poolish_)",
                                  sep = "\n"),
                  theme = challenge_theme())

ggsave(filename = "Out/01.png", width = 3840, height = 2160, units = "px", bg = "#FFFFFF",
       scale = 1.25)

# Before making the average price map and land value, truncate price
quantile(h3_8$price_mean, probs = 0.99, na.rm = T)
quantile(h3_8$land_value, probs = 0.99, na.rm = T)

h3_8 |>
  mutate(land_value = land_value |> as.double()) |>
  mutate(price_mean = if_else(price_mean >= 7636, 7636, price_mean),
         land_value = if_else(land_value >= 0.00904546, 0.00904546, land_value)) -> h3_8

h3_8 |>
  mutate(land_value = (as.double(density)*land_value*3650))

# Airbnb average price
ggplot() +
  geom_sf(fill = NA, data = cdmx_mun) +
  geom_sf(aes(fill = price_mean), 
          colour = NA, alpha = 0.75, size = 0.25,
          data = h3_8) +
  scale_fill_viridis_c(limits = c(0, 7636), breaks = seq(0, 7636, 1000), na.value = NA) + 
  labs(title = "¿Dónde están los Airbnbs más caros?",
       subtitle = "Precio promedio de alojamiento por noche usando H3 en resolución 8\n(Uber’s Hexagonal Hierarchical Spatial Index)",
       fill = "Precio promedio por noche") +
  challenge_theme() -> map_average_8

