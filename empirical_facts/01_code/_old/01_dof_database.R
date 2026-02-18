#######################################################################################################################
## Project: Amazon Access
## Author: Victor Quadros
## Date: completar
## Content: Creates the final dataframe to perform regressions.
#######################################################################################################################

## PREAMBLE ---------------------------------------------------------------------
library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(readxl)
library(tidyverse)

directory <- getwd()
sf_use_s2(FALSE)
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC", "MT") 

anos <- 2008:2022 # prodes changed their methodology in 2008

## LOAD, CHECK CRS AND FIX TOPOLOGY ---------------------------------------------

munic <- st_read(file.path(directory, "02_data_in/IBGE/MunicShape"), layer = "BR_Municipios_2020") %>% 
  filter(SIGLA_UF %in% estados)

prodes <- st_read(file.path(directory, "02_data_in/INPE/PRODES/shapefiles"), layer = "yearly_deforestation") %>% 
  filter(state %in% estados)
years <- 2008:2022
  
names(munic) <- c("cd_mun", "nm_mun", "uf", "area_km2", "geometry")

munic2 <- st_drop_geometry(munic)

aux <- list()
for (i in years){
    
    aux[[paste0(i)]] <- munic2
    aux[[paste0(i)]]$year <- i
    
  }
  
panel <- data.table(do.call(rbind, aux))
  
# Change/Check CRS
source(file.path(directory, "_functions/associateCRS.R"))
polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")

munic <- st_transform(munic, crs = polyconic)
prodes <- st_transform(prodes, crs = polyconic)
prodes <- st_buffer(prodes, dist=0)
st_crs(munic) == st_crs(prodes)

# Intersection

list <- list()
for(i in anos){
  
  prodes2 <- prodes %>% filter(year == i)
  
  # total deforestation
  inter <- st_intersection(munic, prodes2)
  
  inter$area <- as.numeric(st_area(inter))/10^6 # dividing to get km^2
  inter<-st_drop_geometry(inter)
  
  inter <- inter %>% 
    group_by(cd_mun) %>% 
    summarise(defor_prodes = sum(area), year = i)
  
  list[[paste0(i)]] <- inter
  
}
  aux <- bind_rows(list, .id = "i")

  panel <- merge(panel, aux, by = c("cd_mun", "year"), all.x = TRUE)
  panel$i <- NULL
  
list <- list()
load(file.path(directory, "03_data_out/sf_protectedAreas_legalAmazon.Rda"))
protected <- st_transform(protected_legalAmazon, crs = polyconic)

aux2 <- st_intersection(protected, munic)
aux2$area <- as.numeric(st_area(aux2))/10^6
aux2 <- st_drop_geometry(aux2)

aux2 <- aux2 %>% group_by(cd_mun) %>% summarise(area_protected = sum(area))
for(i in years){
  
  prodes2 <- prodes %>% filter(year == i)
  
  # deforestation in protected areas
  inter2 <- st_intersection(protected, prodes2)
  inter <- st_intersection(munic, inter2)
  
  inter$area <- as.numeric(st_area(inter))/10^6
  
  inter<-st_drop_geometry(inter)
  
  inter <- inter %>% 
    group_by(cd_mun) %>% 
    summarise(defor_protected = sum(area), year = i)

  
  list[[paste0(i)]] <- inter
  
}
  aux <- bind_rows(list, .id = "i")
  
  panel <- merge(panel, aux, by = c("cd_mun", "year"), all.x = TRUE)
  panel <- merge(panel, aux2, by = c("cd_mun"), all.x = TRUE)
  panel$i <- NULL
  
# deforestation in areas with no destination

list <- list()
  for(estado in estados){
    x <- st_read(sprintf(file.path(directory, "02_data_in/publicForests/SFB/2020/_old/CNFP_2020_%s.shp"), estado))
    
    x <- st_make_valid(x)
    
    x <- st_transform(x, crs=polyconic)
    st_crs(x) == st_crs(prodes)
    x <- st_buffer(x, dist = 0) # fix topology
    
    x <- subset(x, protecao == "SEM DESTINACAO")
    
    list[[paste0(estado)]] <- x
  }
  
nodest <- bind_rows(list, .id = "estado")

aux2 <- st_intersection(nodest, munic)
aux2$area <- as.numeric(st_area(aux2))/10^6
aux2 <- st_drop_geometry(aux2)

aux2 <- aux2 %>% group_by(cd_mun) %>% summarise(area_nodest = sum(area))

list <- list()
for(i in years){
  
  prodes2 <- prodes %>% filter(year == i)  
  
  inter2 <- st_intersection(nodest, prodes2)
  inter <- st_intersection(munic,inter2) 
  
  inter$area <- as.numeric(st_area(inter))/10^6
  
  inter<-st_drop_geometry(inter)
  
  inter <- inter %>% 
    group_by(cd_mun) %>% 
    summarise(defor_nodest = sum(area), year = i)
  
  list[[paste0(i)]] <- inter
  
}

aux <- bind_rows(list, .id = "i")

panel <- merge(panel, aux, by = c("cd_mun", "year"), all.x = TRUE)
panel <- merge(panel, aux2, by = c("cd_mun"), all.x = TRUE)
panel$i <- NULL

## PRODES ACUMULATED ----------------------------------------------------------------------------------------

# prodes total
load(file.path(directory, "02_data_in/INPE/PRODES/blt_lcv_dfrst_laz_maskShiftPostAccumDeforestProdes_sf_2.Rdata"))
prodes2 <- blt_lcv_dfrst_laz_maskShiftPostAccumDeforestProdes_sf_2
blt_lcv_dfrst_laz_maskShiftPostAccumDeforestProdes_sf_2<- NULL

prodes2 <- st_transform(prodes2, crs = polyconic)
prodes2 <- st_buffer(prodes2, dist=0)
st_crs(munic) == st_crs(prodes2)

inter <- st_intersection(prodes2,munic)
inter$area <- as.numeric(st_area(inter))/10^6
inter <- st_drop_geometry(inter)

inter <- inter %>% 
  group_by(cd_mun) %>% 
  summarise(defor_prodes_acum2007 = sum(area))

panel <- merge(panel, inter, by = "cd_mun", all.x = TRUE)

# protected

inter2 <- st_intersection(protected, prodes2)
inter <- st_intersection(munic, inter2)

inter$area <- as.numeric(st_area(inter))/10^6

inter<-st_drop_geometry(inter)

inter <- inter %>% 
  group_by(cd_mun) %>% 
  summarise(defor_protected_acum2007 = sum(area))

panel <- merge(panel, inter, by = "cd_mun", all.x = TRUE)

# nodest

inter2 <- st_intersection(nodest, prodes2)
inter <- st_intersection(munic, inter2)

inter$area <- as.numeric(st_area(inter))/10^6

inter<-st_drop_geometry(inter)

inter <- inter %>% 
  group_by(cd_mun) %>% 
  summarise(defor_nodest_acum2007 = sum(area))

panel <- merge(panel, inter, by = "cd_mun", all.x = TRUE)

# deforestation in private areas (residual)

panel <- panel %>%
  mutate(defor_nodest = replace(defor_nodest, is.na(defor_nodest), 0)) %>% 
  mutate(defor_protected = replace(defor_protected, is.na(defor_protected), 0)) %>% 
  mutate(defor_prodes = replace(defor_prodes, is.na(defor_prodes), 0)) %>% 
  mutate(area_nodest = replace(area_nodest, is.na(area_nodest), 0)) %>% 
  mutate(area_protected= replace(area_protected, is.na(area_protected), 0)) %>% 
  mutate(defor_nodest_acum2007 = replace(defor_nodest_acum2007, is.na(defor_nodest_acum2007), 0)) %>% 
  mutate(defor_protected_acum2007 = replace(defor_protected_acum2007, is.na(defor_protected_acum2007), 0)) %>% 
  mutate(defor_prodes_acum2007 = replace(defor_prodes_acum2007, is.na(defor_prodes_acum2007), 0))

panel$defor_private <- panel$defor_prodes - panel$defor_protected - panel$defor_nodest
panel$area_private <- panel$area_km2 - panel$area_protected - panel$area_nodest
panel$defor_private_acum2007 <- panel$defor_prodes_acum2007 - panel$defor_protected_acum2007 - panel$defor_nodest_acum2007

panel <- panel %>%
  mutate(defor_private = replace(defor_private, is.na(defor_private), 0)) %>% 
  mutate(area_private = replace(area_private, is.na(area_private), 0)) %>% 
  mutate(defor_private_acum2007 = replace(defor_private_acum2007, is.na(defor_private_acum2007), 0))
  
## ACUMULATED DEFORESTATION ------------------------------------------------------------------

panel[order(rank(year)), defor_prodes_acum := cumsum(defor_prodes) + defor_prodes_acum2007, by = cd_mun]
panel[order(rank(year)), defor_protected_acum := cumsum(defor_protected) + defor_protected_acum2007, by = cd_mun]
panel[order(rank(year)), defor_nodest_acum := cumsum(defor_nodest) + defor_nodest_acum2007, by = cd_mun]
panel[order(rank(year)), defor_private_acum := cumsum(defor_private) + defor_private_acum2007, by = cd_mun]

## VOLUME OUT -----------------------------------------------------------------------------------------------

def <- list()
for(ano in anos){  
df <- fread(sprintf(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_amz_%s.csv"), ano))
df <- df %>%
  group_by(code_origem,ano) %>% 
  summarise(vol_out = sum(volume))
df$code_origem <- as.character(df$code_origem)
def[[paste0(ano)]] <- df
}
df <- bind_rows(def, .id = "ano")

df$ano <- as.numeric(df$ano)

panel <- merge(panel, df, by.x = c("cd_mun", "year"), by.y = c("code_origem", "ano"), all.x = TRUE)

## VOLUME TIMBERFLOW -----------------------------------------------------------
load(file.path(dir, "02_data_in/alt_timberflow/db_timberflow.Rda"))
panel <- merge(panel, db_timberflow, by.x = c("cd_mun", "year"), by.y = c("cod_ibge", "year"), all.x =TRUE)

## DEFOR MAPBIOMAS --------------------------------------------------------------
mapbio <- read_excel(file.path(directory, "02_data_in/mapBiomas/deforestation/tabela_desmatamento_vegetacao_secundaria_mapbiomas_col8.xlsx"), 
                           sheet = "CITY_STATE_BIOME") %>% 
  filter(level_4 == "Forest Formation", 
         dr_class_name == "Supressão Veg. Primária",
         biome == "Amazônia",
         state %in% estados) %>% 
         pivot_longer(cols = c('2018','2019','2020','2021'),  
                      names_to = "year",  
                      values_to = "defor_mapbio") %>%
         mutate(cd_mun = as.character(GEOCODE),
                year = as.numeric(year)) %>% 
         select('cd_mun', 'year', 'defor_mapbio')

panel <- panel %>% 
  left_join(mapbio)

## VOLUME MAIN REG
panel <- as.data.table(panel)
panel[,volume := vol_out,]
panel[uf == "PA" & year < 2019, volume := volTimber,]
panel[uf == "MT", volume := volTimber,]

## SAVE PANEL ------------------------------------------------------------------
save(panel, file=file.path(dir, "03_data_out/panel.Rda"))


## LONG DIFF -------------------------------------------------------------------
panel <- panel %>%
  filter(year %in% 2018:2022) %>% 
  mutate(volTimber = replace(volTimber, is.na(volTimber), 0),
         vol_out = replace(vol_out, is.na(vol_out), 0),
         defor_mapbio = replace(defor_mapbio,is.na(defor_mapbio), 0))

panel <- as.data.table(panel)

panel[uf == "MT", volume := volTimber,]
panel[uf != "MT", volume := vol_out,]

panel[order(rank(year)), vol_out_acum := cumsum(volume), by = cd_mun]
panel[order(rank(year)), defor_prodes_acum := cumsum(defor_prodes), by = cd_mun]
panel[order(rank(year)), defor_private_acum := cumsum(defor_private), by = cd_mun]
panel[order(rank(year)), defor_nodest_acum := cumsum(defor_nodest), by = cd_mun]
panel[order(rank(year)), defor_protected_acum := cumsum(defor_protected), by = cd_mun]
panel[order(rank(year)), defor_protected_acum := cumsum(defor_protected), by = cd_mun]
panel[order(rank(year)), defor_protected_acum := cumsum(defor_protected), by = cd_mun]
panel[order(rank(year)), defor_mapbio_acum := cumsum(defor_mapbio), by = cd_mun]

df <- panel %>% 
  filter(year == 2022)

df <- as.data.frame(df)

save(df, file=file.path(directory, "03_data_out/longDiff.Rda"))
## DISTANCE FROM POSTS ---------------------------------------------------------