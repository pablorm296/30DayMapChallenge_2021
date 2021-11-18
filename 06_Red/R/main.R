# Set up ======================================================================
library(tidyverse)
library(sf)
library(extrafont)
library(patchwork)

# Data load ===================================================================

estatal <- read_rds("Data/estatal.Rds")
municipal <- read_rds("Data/municipal.Rds")

# Geo data load ===============================================================

mex_admin_est <- read_sf("Data/Vectors/01_Entidades.shp")
mex_admin_mun <- read_sf("Data/Vectors/02_Municipios.shp")

# Data cleaning and subset ====================================================

# Define a function to keep only the last 3 months of data
keep_last_year <- function(x) {
  x |>
    filter(Fecha >= "2020-09-01") -> x
}

estatal |>
  keep_last_year() -> estatal

municipal |>
  keep_last_year() -> municipal

# Create datasets with only incidencia delictiva total
estatal |>
  filter(Delito == "Asesinato") -> estatal_idt

municipal |>
  filter(Delito == "Asesinato") -> municipal_idt

# Define a function to compute the rate by 100 000 inhabitants
get_rate <- function(x, ..., population_base = 100000, source = Carpetas) {
  
  # Group data
  x |>
    group_by(...) -> x
  
  # Compute rates
  x |>
    summarise(Población = max(Población, na.rm = T),
              "{{source}}" := sum({{source}}, na.rm = T),
              Tasa = {{source}} / Población * population_base) -> x
  
  # Ungroup
  x |>
    ungroup() -> x
  
  return(x)
}

# Get rate for states
estatal_idt |>
  get_rate(Entidad, Delito) -> estatal_idt

# Get rate for municipalities
municipal_idt |>
  get_rate(Entidad, Municipio, Id_mun, Delito) -> municipal_idt

# Clean municipalities (there are municipalities without data)
municipal_idt |>
  mutate(Carpetas = if_else(is.infinite(Población),
                            NA_real_,
                            Carpetas),
         Tasa = if_else(is.infinite(Población),
                        NA_real_,
                        Tasa)) |>
  mutate(Población = if_else(is.infinite(Población),
                             NA_real_,
                             Población)) -> municipal_idt

# Aux objects =================================================================

# Init container
aux_objects <- list()

## Ggplot theme ----

dataint_theme <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#404040"),
      title = element_text(family = "Open Sans", face = "bold"),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#606060",
                                  hjust = 0)
    )
}

## Colors ----
aux_objects$colors_nacional_estatal <- c("FALSE" = "#0097a7",
                                         "TRUE" = "#006064")

## Captions ----
aux_objects$captions <- list()

aux_objects$captions$source <- "Crime Data: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP), Mexico, October 2021."
aux_objects$captions$source_population <- "Population Data: Consejo Nacional de Población (CONAPO), Mexico, 2021."
aux_objects$captions$copyright <- "© 2021 DataInt Consultores, S.A. de C.V. Some Rights Reserved."

# Aux plots ===================================================================
plots <- list()

# Incidencia delictiva estatal
estatal_idt |>
  mutate(EsNacional = if_else(Entidad == "Nacional", "bold", "plain"),
         Entidad = if_else(Entidad == "Nacional",
                           "National",
                           Entidad)) |>
  ggplot(aes(x = reorder(Entidad, Tasa), y = Tasa,
             colour = Tasa, fill = Tasa)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.9) +
  geom_label(aes(label = Tasa |> round(digits = 1),
                 fontface = EsNacional),
             fill = "#FFFFFF", colour = "#404040",
             size = 3, family = "Roboto") +
  # scale_colour_manual(values = aux_objects$colors_nacional_estatal,
  #                     guide = "none") +
  # scale_fill_manual(values = aux_objects$colors_nacional_estatal,
  #                   guide = "none") +
  scale_fill_distiller(type = "seq", palette = "OrRd", direction = 1,
                       guide = "none") +
  scale_colour_distiller(type = "seq", palette = "OrRd", direction = 1,
                       guide = "none") +
  scale_y_continuous(limits = c(0, 82), breaks = seq(0, 82, by = 5)) +
  coord_flip() +
  labs(x = "State", y = "Cases/100K pop") +
  dataint_theme() -> plots$bar_crime_rate_per100k

# State map ===================================================================

# Init maps container
maps <- list()

# Join geo data with crime data
mex_admin_est |>
  left_join(estatal_idt, by = c("NOMGEO" = "Entidad")) -> mex_admin_est_idt

# Make map
ggplot() +
  geom_sf(aes(fill = Tasa), colour = "#FFFFFF", size = 0.25, alpha = 0.9,
          data = mex_admin_est_idt) +
  scale_fill_distiller(type = "seq", palette = "OrRd", direction = 1,
                         guide = "none") +
  labs(fill = "Cases/100K pop") +
  dataint_theme() -> maps$ent_rate

# Patch with state bar plot
(plots$bar_crime_rate_per100k | maps$ent_rate) + 
  plot_layout(widths = c(1,2)) +
  plot_annotation(
    title = "Murders in Mexico",
    subtitle = "Number of murder cases filed at each State Prosecutor's Office per 100'000 people (Sep/20 - Sep/21)",
    caption = str_c(aux_objects$captions$source,
                    aux_objects$captions$source_population,
                    aux_objects$captions$copyright,
                    sep = "\n"),
    theme = dataint_theme() + theme(plot.title = element_text(size = 26)))

ggsave(filename = "Out/state.png", width = 3840, height = 2160, scale = 1.25, units = "px", 
       bg = "#FFFFFF")

# Municipal map ===============================================================

# Get municipality centroids
mex_mun_centroids <- mex_admin_mun |>
  st_centroid()

# Merge with crime data
mex_mun_centroids |>
  left_join(municipal_idt, by = c("CVEGEO" = "Id_mun")) -> mex_mun_centroids

# Remove municipalities with 0 cases
mex_mun_centroids |>
  filter(Carpetas > 0) -> mex_mun_centroids

# Limit rate by the 99 percentile
rate_limit <- quantile(mex_mun_centroids$Tasa, probs = 0.975)

mex_mun_centroids |>
  mutate(Tasa = if_else(Tasa >= rate_limit, 
                        rate_limit,
                        Tasa)) -> mex_mun_centroids

# Get bbox from mexico's central valley
mex_admin_est |>
  filter(NOMGEO %in% c("Ciudad de México", "México", "Tlaxcala")) |>
  st_bbox() -> mex_center_bbox

# Make map
ggplot() +
  geom_sf(colour = "#404040", fill = NA,
          data = mex_admin_est) +
  geom_sf(aes(size = Carpetas, colour = Tasa), alpha = 0.75,
          data = mex_mun_centroids) +
  scale_size(range = c(0.5, 15), guide = "none") +
  scale_colour_distiller(type = "seq", palette = "OrRd", direction = 1,
                         guide = "none") +
  dataint_theme() -> maps$mun_hotspots

maps$mun_hotspots +
  coord_sf(xlim = c(mex_center_bbox[1], mex_center_bbox[3]),
           ylim = c(mex_center_bbox[2], mex_center_bbox[4])) -> maps$mun_hotspots_center

(maps$mun_hotspots | maps$mun_hotspots_center) +
  plot_layout(widths = c(2,1)) +
  plot_annotation(
    title = "Murder in Mexico",
    subtitle = "Each bubble represents a municipality. Its colour is given by the murder rate (per 100K pop), while its size is given by the total number of cases (Sep/2020 - Sep/2021)",
    caption = str_c(aux_objects$captions$source,
                    aux_objects$captions$source_population,
                    aux_objects$captions$copyright,
                    sep = "\n"),
    theme = dataint_theme() + theme(plot.title = element_text(size = 26)))

ggsave(filename = "Out/hotspots.png", width = 3840, height = 2160, scale = 1.25, units = "px", 
       bg = "#FFFFFF")
