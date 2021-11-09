# Set up ======================================================================
# Load packages
library(tidyverse)
library(sf)
library(extrafont)
library(patchwork)

# Load geo data ===============================================================

mex_admin_est <- read_sf("../Common/MarcoGeoestadístico/01_Entidades.shp")
mex_admin_mun <- read_sf("../Common/MarcoGeoestadístico/02_Municipios.shp")

# Soruce ggplot theme =========================================================

source("../Common/R/ggplot_themes.R")

# Data set up and cleaning ====================================================

# Get municipality centroids
mex_mun_centroids <- mex_admin_mun |>
  st_centroid()

# Define a list of the state capitals
mex_state_capitals <- c("01001", "02002", "03003",
                        "04002", "05030", "06002",
                        "07101", "08019", "09015",
                        "10005", "11015", "12029",
                        "13048", "14039", "15106",
                        "16053", "17008", "18017",
                        "19039", "20067", "21114",
                        "22014", "23004", "24028",
                        "25006", "26030", "27004",
                        "28041", "29033", "30087",
                        "31050", "32056")

mex_mun_centroids |>
  mutate(is_capital = CVEGEO %in% mex_state_capitals) -> mex_mun_centroids

# Get centroids from only capitals
mex_mun_centroids |>
  filter(is_capital) -> capitals_centroids

# Get crs of INEGI's shapefile
mex_admin_mun |>
  st_crs() -> inegi_crs

# Set national shapefile to INEGI's crs
mex_admin_nac |>
  st_transform(crs = inegi_crs) -> mex_admin_nac

# Make national map
mex_admin_est |>
  st_buffer(dist = 100) |>
  st_union() -> mex_admin_nac

# Compute voronoi polygons ====================================================

# Get voronoi polygons using location of capitals
st_voronoi(capitals_centroids$geometry |> st_union(), dTolerance = 50) -> voronoi_capitals

# Validate voroi polygons
voronoi_capitals |>
  st_collection_extract("POLYGON") |>
  st_make_valid() -> voronoi_capitals

# Simplify and validate national polygon
mex_admin_nac |>
  st_simplify(preserveTopology = T, dTolerance = 3) -> mex_admin_nac

mex_admin_nac |>
  st_make_valid() -> mex_admin_nac

# Cut voronoi polygons
voronoi_capitals |>
  st_simplify(dTolerance = 1) |>
  st_crop(st_bbox(mex_admin_nac)) |>
  ggplot() +
  geom_sf(fill = NA)

# Get interseciton with mexico
voronoi_states <- st_intersection(voronoi_capitals, mex_admin_nac) 

# Get map =====================================================================

ggplot() +
  geom_sf(fill = NA, colour = "#404040", data = voronoi_states) +
  geom_sf(colour = "#e91e63", alpha = 0.75, data = capitals_centroids) +
  labs(title = "¿Cómo serían los estados si a cada ciudad capital le asignamos la región formada por todo lo que está más cerca de ella que de ninguna otra?", 
       subtitle = "Teselación de Voronoi para los centroides de los 32 municipios capitales de México.",
       caption = str_c("Para la Ciudad de México, la alcaldía Cuauhtémoc se tomo como el municipio capital.",
                       "Elaboración: Pablo R. (Twitter: @_poolish_).",
                       sep = "\n")) +
  challenge_theme()

# Save final plot
ggsave(filename = "Out/01.png", width = 4000, height = 2160, units = "px", bg = "#FFFFFF",
       scale = 1.25)
