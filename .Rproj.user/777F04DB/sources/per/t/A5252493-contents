library(auk)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

#Defino el directorio de trabajo

f_sed <- "C:/Users/Joaquín Aldabe/Nextcloud/Proyecto IM/eBird/data/ebd_BR-RS_fotfly_smp_relJun-2023_sampling.txt"
checklists <- read_sampling(f_sed, unique = FALSE)
glimpse(checklists)


#Hacemos un histograma de las observaciones según el esfuerzo en km recorridos

checklists_traveling <- filter(checklists, protocol_type == "Traveling")
ggplot(checklists_traveling) +
  aes(x = effort_distance_km) +
  geom_histogram(binwidth = 5) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(x = "Distance traveled [km]",
       y = "# of eBird checklists",
       title = "Distribution of distance traveled on eBird checklists")

#Cargamos los datos de registros de la especie (archivo EBD)

f_ebd <- "C:/Users/Joaquín Aldabe/Nextcloud/Proyecto IM/eBird/data/ebd_BR-RS_fotfly_smp_relJun-2023.txt"
observations <- read_ebd(f_ebd, unique = T, rollup = T)
glimpse(observations)

#Colapsamos las checklist repetidas (cuando hay más de un observador que aportó a la checklist)

checklists_unique <- auk_unique(checklists, checklists_only = TRUE)

#Generamos datos de detección y no detección


# Importamos de nuevo los datos de las checklist y filtramos las listas que reportaron todas las especies observadas
checklists <- read_sampling(f_sed) %>% 
  # subset to complete checklists
  filter(all_species_reported)
# import observation data
observations <- read_ebd(f_ebd) %>% 
  # subset to complete checklists
  filter(all_species_reported)
# zero-fill to produce detection/non-detection data
zf <- auk_zerofill(observations, checklists, collapse = TRUE)
glimpse(zf)# import checklist data

#Convertimos las X de la variable observation count a NA

zf$observation_count <- if_else(zf$observation_count == "X", 
                                NA_character_, zf$observation_count) %>% 
  as.integer()
select(zf, observation_count, species_observed) %>% 
  head(10)

#Filtramos por años, tipo de protocolo y distancia

zf_filtered <- zf %>% 
filter(year(observation_date) >= 2013, year(observation_date) <= 2022,
       protocol_type %in% c("Traveling", "Stationary"),
       duration_minutes < 6 * 60,
       effort_distance_km < 10 | protocol_type == "Stationary")
nrow(zf)
#> [1] 22838
nrow(zf_filtered)

#Seleccionamos solo las columnas que necesitamos
checklists_zf <- zf_filtered %>% 
select(checklist_id, 
       latitude, longitude,
       observation_date, time_observations_started,
       protocol_type,
       duration_minutes, effort_distance_km, number_observers,
       observation_count, species_observed)

#APLICACIONES

#En que porcentaje de listas se detectó a la tijereta
mean(checklists_zf$species_observed)

#Como cambia la frecuencia de detección entre los meses del año

monthly_detection <- checklists_zf %>% 
  mutate(month = month(observation_date)) %>% 
  group_by(month) %>% 
  summarize(detection_frequency = mean(species_observed))

# plot monthly detection frequency
ggplot(monthly_detection) +
  aes(x = month, y = detection_frequency) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Month of year",
       y = "Detection frequency",
       title = "Monthly detection frequency for Fork-tailed Flycatcher",
       subtitle = "Rio Grande do Sul, Brazil")

#CONVERTIMOS A DATOS ESPACIALES

checklists_sf <- st_as_sf(checklists_zf, coords = c("longitude", "latitude"),
                          # 4326 is the code for an unprojected lon/lat
                          # coordiante reference system
                          crs = 4326)
print(checklists_sf)

#Hacemos un buffer de 50km de radio de Gramado

gramado_point <- st_sfc(st_point(c(-50.876, -29.375)), crs = 4326)
# 50km = 50,000m
gramado_circle <- st_buffer(gramado_point, dist = 50000)
checklists_gramado <- checklists_sf[gramado_circle, ]
mean(checklists_gramado$species_observed)

crs <- st_crs("+proj=laea +lat_0=-30 +lon_0=-50")

# polygons from natural earth
ne_land <- read_sf("C:/Users/Joaquín Aldabe/Nextcloud/Proyecto IM/eBird/data/gis-data.gpkg", "ne_land") %>% 
  st_transform(crs = crs)
ne_country_lines <- read_sf("C:/Users/Joaquín Aldabe/Nextcloud/Proyecto IM/eBird/data/gis-data.gpkg", "ne_country_lines") %>% 
  st_transform(crs = crs)
rgds_boundary <- read_sf("C:/Users/Joaquín Aldabe/Nextcloud/Proyecto IM/eBird/data/gis-data.gpkg", "ne_states") %>% 
  filter(state_code == "BR-RS") %>% 
  st_transform(crs = crs)

# transformamos datos de ebird a la misma proyección de área
checklists_proj <- st_transform(checklists_sf, crs = crs)

par(mar = c(0.25, 0.25, 2, 0.25))

# start with a blank plot of the data to define the spatial extent of the map
plot(st_geometry(checklists_proj), col = NA)

# contextual gis data
plot(ne_land, col = "#dddddd", border = "#888888", lwd = 0.5, add = TRUE)
plot(rgds_boundary, col = "#cccccc", border = NA, add = TRUE)
#> Warning in plot.sf(rgds_boundary, col = "#cccccc", border = NA, add = TRUE):
#> ignoring all but the first attribute
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)

# ebird observations
# not detected
plot(filter(checklists_proj, !species_observed),
     pch = 19, cex = 0.2, col = alpha("#555555", 0.5),
     add = TRUE)
#> Warning in plot.sf(filter(checklists_proj, !species_observed), pch = 19, :
#> ignoring all but the first attribute
# detected
plot(filter(checklists_proj, species_observed),
     pch = 19, cex = 0.3, col = alpha("#4daf4a", 1),
     add = TRUE)
#> Warning in plot.sf(filter(checklists_proj, species_observed), pch = 19, :
#> ignoring all but the first attribute
# legend
legend("bottomright", bty = "n",
       col = c("#555555", "#4daf4a"),
       legend = c("eBird checklists", "Fork-tailed Flycatcher sightings"),
       pch = 19)
box()
title("Fork-tailed Flycatcher eBird Observations (2013-2022)")
