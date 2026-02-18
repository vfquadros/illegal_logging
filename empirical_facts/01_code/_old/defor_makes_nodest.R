#######################################################################################################################
## Project: Amazon Access
## Author: Victor Quadros
## Date: 26/06/2024
## Content: Makes a shapefile of FPND
#######################################################################################################################

## PREAMBLE --------------------------------------------------------------------

library(sf)
library(dplyr)

dir <- getwd()
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC", "MT") 
source(file.path(dir, "_functions/associateCRS.R"))
polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")

# FPND -------------------------------------------------------------------------

list <- list()
for(estado in estados){
  x <- st_read(sprintf(file.path(dir, "02_data_in/publicForests/SFB/2020/_old/CNFP_2020_%s.shp"), estado))
  
  x <- st_make_valid(x)
  
  x <- st_transform(x, crs=polyconic)
  x <- st_buffer(x, dist = 0) # fix topology
  x <- subset(x, protecao == "SEM DESTINACAO")
  
  list[[paste0(estado)]] <- x
}

sf_FPND_legalAmazon <- bind_rows(list, .id = "estado")

save(sf_FPND_legalAmazon, file=file.path(dir, "03_data_out/territories/sf_FPND_legalAmazon.Rda"))
