#  leer archivo .json en R
library(jsonlite)

data <- jsonlite::fromJSON("~/Downloads/ejecuciones2.json")

library(dplyr)
library(tidyr)

data_2 <- tibble(data) |> 
  janitor::clean_names() |> 
  rename(city = 5) |> 
  unnest_wider(geo, names_sep = "_")

data_2 |> glimpse()

data_2 |> 
  count(fields$smlCity)

library(sf)

data_3 <- data_2 |> 
  st_as_sf(coords = c("geo_2", "geo_1"),
               crs = 4674)

data_4 <- data_3 |> 
  filter(city$smlCity == "Santiago")


library(chilemapas)

# obtener mapa por zonas rural/urbano
mapa_zonas_urbanas <- chilemapas::mapa_zonas |> 
  filter(codigo_region == 13) |> 
  left_join(chilemapas::codigos_territoriales |> 
              select(matches("comuna")))


# vector con geocódigos que deseamos remover
islas_urbanas <- c("13124071004", "13124071005", "13124081001", "13124071001", "13124071002", "13124071003", #Pudahuel
                   "13401121001", #San Bernardo
                   "13119131001", #Maipú
                   "13203031000", "13203031001", "13203031002", "13203011001", "13203011002" #San José de Maipo
)

comunas_urbanas <- c("Pudahuel", "Cerro Navia", "Conchali", "La Pintana", "El Bosque", 
                     "Estacion Central", "Pedro Aguirre Cerda", "Recoleta", "Independencia", 
                     "La Florida", "Penalolen", "Las Condes", "Lo Barnechea", "Quinta Normal", 
                     "Maipu", "Macul", "Nunoa", "Puente Alto", "Quilicura", "Renca", 
                     "San Bernardo", "San Miguel", "La Granja", "Providencia", "Santiago",
                     "San Joaquin", "Lo Espejo", "La Reina", "San Ramon", "La Cisterna", 
                     "Lo Prado", "Cerrillos", "Vitacura", "Huechuraba",
                     "San Jose de Maipo")

# mapa de sectores urbanos, de comunas urbanas
mapa_urbano <- mapa_zonas_urbanas |> 
  st_set_geometry(mapa_zonas_urbanas$geometry) |> 
  # filtrar comunas urbanas
  filter(nombre_comuna %in% comunas_urbanas) |>
  # filtrar islas urbanas
  filter(!geocodigo %in% islas_urbanas) |>
  # unir comunas
  group_by(nombre_comuna, codigo_comuna) %>%
  summarise(geometry = st_union(geometry)) |>
  ungroup()
  
# solamente puntos dentro del mapa
data_5 <- st_filter(data_4, mapa_urbano)

library(ggplot2)
library(ggblend)
library(ggview)

ggplot() +
  # mapa de fondo
  geom_sf(data = mapa_urbano,
          fill = "#E7E7E7", color = "grey40", linewidth = 0.3) +
  # puntos
  geom_sf(data = data_5 |> 
            mutate(geometry = st_jitter(geometry, factor = 0.007)),
          color = "black", alpha = 0.6, 
          size = 2) *
  copy_under(color = "#E7E7E7", size = 2.5, alpha = 1) +
  # coord_sf(xlim = c(-70.8, -70.5), ylim = c(-33.7, -33.3)) +
  # fondo negro
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank()) +
  canvas(6, 6)

ggsave("img/mapas/ejecuciones/mapa_ejecuciones_b.jpeg", width = 6, height = 6)
