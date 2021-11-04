# Set up ======================================================================
library(tidyverse)
library(foreign)
library(sf)
library(extrafont)
library(patchwork)
library(lubridate)

# Data load ===================================================================

# Deaths in 2020 data
defunciones <- read.dbf("Data/Defunciones/defun20.dbf") |> as_tibble()

# Mexican list of diseases
lista_mex <- read.dbf("Data/Defunciones/LISTAMEX.dbf") |> as_tibble()

# International catalogue of diseases
lista_int <- read.dbf("Data/Defunciones/CATMINDE.DBF") |> as_tibble()

# Mexico administrative
mexico_admin <- read_sf("../Common/MarcoGeoestadístico/01_Entidades.shp")
mexico_mun <- read_sf("../Common/MarcoGeoestadístico/02_Municipios.shp")

# Ggplot themes ===============================================================
source("../Common/R/ggplot_themes.R")

# Fix data encoding ===========================================================

# Mexican list of diseases
# POR QUÉ DEMONIOS EL INEGI USA IBM850?
lista_mex |> 
  map_dfc(function(x) str_conv(x, "IBM850")) -> lista_mex

# International catalogue of diseases
lista_int |> 
  map_dfc(function(x) str_conv(x, "IBM850")) -> lista_int

# Deaths in 2020
defunciones |>
  map_dfc(function(x) str_conv(x, "IBM850")) -> defunciones

# Add covid causes ============================================================

lista_int |>
  add_case(CVE = "U071", DESCRIP = "COVID-19, Virus identificado") |>
  add_case(CVE = "U072", DESCRIP = "COVID-19, Virus no identificado") |>
  add_case(CVE = "U071", DESCRIP = "COVID-19, Síndrome Inflamatorio Multisistémico Asociado") -> lista_int

lista_mex |>
  add_case(CVE = "06T", DESCRIP = "COVID-19") -> lista_mex

# Clean data ==================================================================

# Use disease names in factors
defunciones |>
  mutate(LISTA_MEX = factor(LISTA_MEX, levels = lista_mex$CVE, 
                            labels = lista_mex$DESCRIP),
         CAUSA_DEF = factor(CAUSA_DEF, levels = lista_int$CVE,
                            labels = lista_int$DESCRIP)) -> defunciones

# Keep useful columns
names(defunciones)
cols2keep <- c("ENT_REGIS", "MUN_REGIS", 
               "ENT_RESID", "MUN_RESID", 
               "ENT_OCURR", "MUN_OCURR", 
               "DIA_REGIS", "MES_REGIS", "ANIO_REGIS",
               "DIA_OCURR", "MES_OCURR", "ANIO_OCUR",
               "LISTA_MEX", "CAUSA_DEF", "SITIO_OCUR", "DERECHOHAB", "ASIST_MEDI")

defunciones |>
  select(all_of(cols2keep)) -> defunciones

# Rename columns
new_col_names <- c("registro_ent", "registro_mun",
                   "residencia_ent", "residencia_mun",
                   "suceso_ent", "suceso_mun",
                   "registro_dia", "registro_mes", "registro_anio",
                   "suceso_dia", "suceso_mes", "suceso_anio",
                   "causa_mex", "causa_int", "sitio", "derechohab", "asistencia_medica")
names(defunciones) <- new_col_names

# Clean site of death factor
defunciones |>
  mutate(sitio = factor(sitio, 
                        levels = as.character(1:12),
                        labels = c("SSA", "IMSS PROSPERA", "IMSS", "ISSSTE",
                                   "PEMEX", "SEDENA", "SEMAR", "Otro (Público)",
                                   "Privado", "Vía pública", "Hogar", "Otro"))) -> defunciones

# Clean derechohab
defunciones |>
  mutate(derechohab = factor(derechohab,
                             levels = as.character(1:9),
                             labels = c("Ninguna", "IMSS", "ISSSTE", "PEMEX",
                                        "SEDENA", "SEMAR", "Seguro Popular",
                                        "Otra", "IMSS PROSPERA"))) -> defunciones

# Clean asistencia med
defunciones |>
  mutate(asistencia_medica = factor(asistencia_medica,
                             levels = as.character(1:2),
                             labels = c("Con asistencia médica",
                                        "Sis asistencia médica"))) -> defunciones

# Clean dates
defunciones |>
  mutate(registro_date = str_c(registro_anio, 
                               registro_mes |> str_pad(width = 2, pad = "0"), 
                               registro_dia |> str_pad(width = 2, pad = "0"), 
                               sep = "/"),
         suceso_date = str_c(suceso_anio, 
                             suceso_mes |> str_pad(width = 2, pad = "0"), 
                             suceso_dia |> str_pad(width = 2, pad = "0"), 
                             sep = "/")) -> defunciones

defunciones |>
  mutate(registro_date = as.Date(registro_date),
         suceso_date = as.Date(suceso_date)) -> defunciones

# Create location ids
defunciones |>
  mutate(residencia_id = str_c(residencia_ent, residencia_mun),
         suceso_id = str_c(suceso_ent, suceso_mun)) -> defunciones

# Keep data only from 2020 and from covid
defunciones |>
  filter(causa_mex == "COVID-19", suceso_date >= "2020-01-01") -> covid_2020

# Create a dummy var: the person moved from the municipality where she/he lived?
covid_2020 |>
  mutate(migró = if_else(residencia_id != suceso_id, "Tuvo que desplazarse",
                         "No tuvo que desplazarse")) -> covid_2020

covid_2020 |>
  mutate(migró_ent = if_else(residencia_ent != suceso_ent, "Tuvo que desplazarse",
                         "No tuvo que desplazarse")) -> covid_2020

# Aux plots ===================================================================

# Init container
aux_plots <- list()

# ¿Dónde ocurrieron las muertes por covid?
covid_2020 |>
  ggplot(aes(x = fct_infreq(sitio))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
           alpha = 0.75, colour = "#78909c", fill = "#78909c") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05)) +
  coord_flip() +
  labs(title = "¿Dónde ocurrieron las defunciones por COVID-19?",
       x = "Lugar",
       y = "% de las defunciones") +
  challenge_theme_dark_no_grid.y() -> aux_plots$bar_place_of_death

# ¿Eran derechohabientes?
covid_2020 |>
  ggplot(aes(x = fct_infreq(derechohab))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
           alpha = 0.75, colour = "#78909c", fill = "#78909c") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.05)) +
  coord_flip() +
  labs(title = "¿A que institución estaban afiliadas\nlas personas que murieron por COVID-19?",
       x = "Derechohabiencia",
       y = "% de las defunciones") +
  challenge_theme_dark_no_grid.y() -> aux_plots$bar_institution

# ¿Tuvieron que moverse para recibir atención?
covid_2020 |>
  filter(asistencia_medica == "Con asistencia médica") |>
  ggplot(aes(x = fct_infreq(migró))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
           alpha = 0.75, colour = "#78909c", fill = "#78909c", width = 0.25) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.55), breaks = seq(0, 0.55, by = 0.05)) +
  labs(title = "¿Cuánta gente que murió por COVID-19\ntuvo que ir a otro municipio para recibir atención?",
       subtitle = "Proporción de defunciones que sí recibieron atención médica",
       x = NULL,
       y = "% de las defunciones") +
  coord_flip() +
  challenge_theme_dark_no_grid.y() -> aux_plots$bar_moved

# Create movement matrix ======================================================

# Get movements
covid_2020 |>
  filter(migró == "Tuvo que desplazarse") |>
  select(residencia_id, suceso_id) -> covid_movement

covid_movement |>
  group_by(residencia_id, suceso_id) |>
  summarise(count = n()) -> covid_movement

# Get centroids ===============================================================

mexico_mun |>
  st_centroid() -> mexico_centroids

covid_movement |>
  left_join(mexico_centroids |> 
              select(CVEGEO),
            by = c("residencia_id" = "CVEGEO")) |>
  rename(residencia_geo = geometry) -> covid_movement

covid_movement |>
  left_join(mexico_centroids |> 
              select(CVEGEO),
            by = c("suceso_id" = "CVEGEO")) |>
  rename(suceso_geo = geometry) -> covid_movement

# Map =========================================================================

ggplot() +
  geom_sf(colour = NA, fill = "#000000", data = mexico_admin) +
  geom_curve(aes(x = unlist(map(residencia_geo, 1)),
                 y = unlist(map(residencia_geo, 2)),
                 xend = unlist(map(suceso_geo, 1)),
                 yend = unlist(map(suceso_geo, 2)),
                 size = count,
                 alpha = count),
             angle = 100, curvature = 0.25,
             colour = "#cfd8dc",
             data = covid_movement) +
  scale_size_continuous(range = c(0.25, 2), guide = "none") +
  scale_alpha_continuous(range = c(0.125, 0.75), guide = "none") +
  labs(title = "¿Cuánto tuvieron que desplazarse las personas para atenderse por COVID-19?",
       subtitle = "Municipio de residencia → municipio de defunción (defunciones por COVID-19)",
       x = NULL,
       y = NULL) +
  challenge_lines_theme() -> map

(map / (aux_plots$bar_moved | aux_plots$bar_place_of_death | aux_plots$bar_institution)) +
  plot_layout(heights = c(2,1)) +
  plot_annotation(caption = str_c("Datos: Mortalidad general en México (2020). INEGI.",
                                  "Elaboración: Pablo R. (Twitter: @_poolish_).",
                                  sep = "\n"),
                  theme = challenge_theme()) -> final

# Save final plot
ggsave(filename = "Out/01.png", plot = final, width = 4000, height = 2160, units = "px",
       scale = 1.5)

# Save final plot
ggsave(filename = "Out/02.png", plot = map, width = 3840, height = 2160, units = "px",
       scale = 1.5)
