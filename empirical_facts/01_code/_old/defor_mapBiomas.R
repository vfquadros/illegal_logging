library(sf)
library(tmap)
library(readxl)
library(tidyverse)
library(dplyr)

directory <- getwd()

#load
df <- read_excel(file.path(directory, "02_data_in/mapBiomas/deforestation/tabela_desmatamento_vegetacao_secundaria_mapbiomas_col8.xlsx"), 
                sheet = "CITY_STATE_BIOME")
munic <- st_read(file.path(directory, "02_data_in/IBGE/MunicShape"))

estados_AL <- c("RO", "RR", "AM", "AP", "TO", "MA", "PA", "AC")
df <- df %>% 
  filter(level_4 == "Forest Formation", 
         dr_class_name == "Supressão Veg. Primária",
         biome == "Amazônia",
         state %in% estados_AL)

munic <- munic %>% 
  filter(SIGLA_UF %in% estados_AL)

munic <- merge(df, munic, by.x = "GEOCODE", by.y = "CD_MUN")
munic <- st_as_sf(munic)

munic <- munic %>% 
  mutate(deforestation = df$'2018'+df$'2019'+df$'2020')

munic$defor <- munic$deforestation/munic$AREA_KM2

map <- tm_shape(munic) +
  tm_fill("defor", style = "quantile", title = "Volume", n = 5) +
  tm_borders()

path_map <- file.path(directory, "04_outputs/maps/deforestation.png")
tmap_save(map, filename = path_map, width = 10, height = 8, units = "in")
