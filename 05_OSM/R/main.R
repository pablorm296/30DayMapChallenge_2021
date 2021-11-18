#Set up =======================================================================
# Load packages
library(tidyverse)
library(sf)
library(extrafont)
library(patchwork)
library(osmdata)

# Load ggplot2 themes =========================================================
source("../Common/R/ggplot_themes.R")

# Load vector data ============================================================

# Load INEGI's vector data
mex_admin_est <- read_sf("../Common/MarcoGeoestadístico/01_Entidades.shp")
mex_admin_mun <- read_sf("../Common/MarcoGeoestadístico/02_Municipios.shp")

# Load colonias vector data
cdmx_colonias <- read_sf("../04_Hexagons/Data/Colonias/coloniascdmx.shp")

# Clean vector data ===========================================================

## Colonias data ----

# Keep only colonia name
cdmx_colonias |>
  select(nombre) -> cdmx_colonias

# Clean colonias name
cdmx_colonias |>
  # To title case
  mutate(nombre = str_to_title(nombre)) |>
  # Clean roman numerals
  mutate(nombre = str_replace_all(nombre, 
                                 pattern = regex("\\s(I[XV]|V?I{0,3})(\\s|$)", 
                                                 ignore_case = T), str_to_upper)) -> cdmx_colonias

# Clean double spaces
cdmx_colonias |>
  mutate(nombre = str_squish(nombre)) -> cdmx_colonias

## Admin data ----

cdmx_admin_est <- mex_admin_est |>
  filter(CVE_ENT == "09")

cdmx_admin_mun <- mex_admin_mun |>
  filter(CVE_ENT == "09")

## Set crs to EPGS:4326 ----

cdmx_colonias |>
  st_transform(crs = "EPSG:4326") -> cdmx_colonias

cdmx_admin_est |>
  st_transform(crs = "EPSG:4326") -> cdmx_admin_est

cdmx_admin_mun |>
  st_transform(crs = "EPSG:4326") -> cdmx_admin_mun

# Get colonias of interest ====================================================

# Init a container
my_colonias <- list()

# Centro
cdmx_colonias |>
  filter(str_detect(nombre, "Centro [IVX]")) -> my_colonias$centro

# Estrella del sur
cdmx_colonias |>
  filter(str_detect(nombre, "Estrella Del Sur")) -> my_colonias$estrella_del_sur

# Lomas de padierda
cdmx_colonias |>
  filter(str_detect(nombre, "Lomas De Padierna")) -> my_colonias$lomas_de_padierna

# Get streets in colonias of interest =========================================

# Init a container
my_streets <- list()

# Define a function to get the streets inside an area of interest
get_osm_streets <- function(sf_polygons) {
  # Define query
  q <- opq(bbox = st_bbox(sf_polygons), timeout = 1000) |>
    add_osm_features(features = c('"highway"="motorway"',
                                  '"highway"="motorway_link"',
                                  '"highway"="trunk"',
                                  '"highway"="trunk_link"',
                                  '"highway"="primary"',
                                  '"highway"="primary_link"',
                                  '"highway"="secondary"',
                                  '"highway"="secondary_link"',
                                  '"highway"="tertiary"',
                                  '"highway"="tertiary_link"',
                                  '"highway"="unclassified"',
                                  '"highway"="residential"'))
  
  # Request data
  r <- osmdata_sf(q)
  
  # Return lines
  return(r$osm_lines)
}

# Define a function to crop an sf feature to an specified bbox
crop_sf_to_bbox <- function(sf_feauture, bbox) {
  result <- sf_feauture |>
    st_crop(bbox)
  
  return(result)
}

# Get streets from Mexico City
cdmx_streets <- get_osm_streets(cdmx_admin_est)

cdmx_streets |>
  st_transform(crs = "EPSG:4326") -> cdmx_streets

# For each colonia, crop the streets
for (colonia_name in names(my_colonias)) {
  my_streets[[colonia_name]] <- crop_sf_to_bbox(cdmx_streets, 
                                                bbox = st_bbox(my_colonias[[colonia_name]]))
}

# Generate maps ===============================================================

# Init container
my_maps <- list()

# Define a function to create tha basic colonia map
create_colonia_map <- function(sf_streets, name) {
  map <- ggplot() +
    geom_sf(aes(size = highway), 
            fill = NA, colour = "#404040", alpha = 0.8,
            data = sf_streets) +
    scale_size_discrete(limits = c("residential", "unclassified", "tertiary_link",
                                   "tertiary", "secondary_link", "secondary", 
                                   "primary_link", "primary", "trunk_link", "trunk", 
                                   "motorway_link", "motorway"),
                        range = c(0.25, 1.25), guide = "none") +
    labs(title = str_c("Colonia", name |> str_replace_all(pattern = "_", replacement = " ") |> str_to_title(), 
                       sep = " ")) +
    challenge_theme_no_lines() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_blank()
    )
  
  return(map)
}

for (colonia_name in names(my_streets)) {
  my_maps[[colonia_name]] <- create_colonia_map(my_streets[[colonia_name]], colonia_name)
}

(my_maps$estrella_del_sur | my_maps$lomas_de_padierna | my_maps$centro) +
  plot_layout(widths = c(1,1,1), heights = c(1)) +
  plot_annotation(caption = str_c("Datos: OpenStreetMaps.", "Elaboración: Pablo R (Twitter: @_poolish_",
                                  sep = "\n"),
                  theme = challenge_theme_no_lines())

ggsave(filename = "Out/my_colonias.png", width = 3840, height = 1920, units = "px",
       bg = "#FFFFFF")
