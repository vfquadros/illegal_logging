# PREAMBLE ---------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)

dir <- getwd()
polyconic <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"
sf_use_s2(FALSE)
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC") # MT foi retirado por falta de dados no dof

# INTERSECTIONS  ------------------------------------------------------------------------

load(file.path(dir, "03_data_out/sf_protectedAreas_legalAmazon.Rda"))
munic <- st_read(file.path(dir, "02_data_in/IBGE/MunicShape"), layer = "BR_Municipios_2020")
munic <- st_transform(munic, crs = polyconic)

load(file.path(dir, "02_data_in/PRODES/blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2.Rdata"))
prodes <- blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2
blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2 <- NULL
prodes <- st_transform(prodes, crs = polyconic)
prodes <- prodes %>% 
  filter(prodes_year >= 2018)
#estado <- "RO"
list <- list()
for(estado in estados){
  x <- st_read(sprintf(file.path(dir, "02_data_in/publicForests/SFB/2020/CNFP_2020_%s.shp"), estado))
  
  x <- st_make_valid(x)
  
  x <- st_transform(x, crs=polyconic)
  st_crs(x) == st_crs(prodes)
  x <- st_buffer(x, dist = 0) # fix topology
  
  x <- subset(x, protecao == "SEM DESTINACAO")
  
  inter2 <- st_intersection(x, prodes)
  inter <- st_intersection(munic,inter2) 
  
  inter$area <- as.numeric(st_area(inter))/10^6
  
  inter<-st_drop_geometry(inter)
  
  inter <- inter %>% 
    group_by(CD_MUN) %>% 
    summarise(defor_nodest = sum(area))
  
  list[[paste0(estado)]] <- inter
}

nodest <- bind_rows(list, .id = "estado")
nodest <- subset(nodest,select = c("CD_MUN", "defor_nodest"))

nodest <- nodest %>% 
  group_by(CD_MUN) %>% 
  summarise(defor_nodest = sum(defor_nodest))

