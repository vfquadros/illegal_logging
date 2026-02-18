#==============================================================================#
# Project: Illegal Logging                                                     #
# Author: Victor Quadros                                                       #
# Content:                                                                     # 
#==============================================================================#

rm(list = ls())
gc()

# PREAMBLE ---------------------------------------------------------------------

library(data.table)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(viridis)
library(scales)
library(terra)
library(sf)
library(raster)     
library(gdistance)
library(sf)

vec_lglamz <- c("RO", "RR", "AC", "AP", "AM", "MT", "PA", "TO", "MA")

# INPUTS -----------------------------------------------------------------------

load("empirical_facts/03_data_outputs/tab/df_munic_template.RData")
load("empirical_facts/03_data_outputs/tab/sf_munic_template.RData")
load("empirical_facts/03_data_outputs/tab/df_template.RData")
load("empirical_facts/03_data_outputs/tab/df_fixed_effects.RData")
df_mapbiomes_2010 <- fread("empirical_facts/03_data_outputs/tab/df_mapbiomes_2010.csv")

df_deforestation <- df_template %>% 
  left_join(df_fixed_effects) %>% 
  left_join(df_mapbiomes_2010) %>%
  filter(sigla_uf %in% vec_lglamz) %>% 
  mutate(aux = ifelse(land_use_2010 %in% c("novege_area", "pasture", "other_agri", "soy",
                                                   "mosaic"),1,0)) %>% 
  group_by(cd_mun) %>% 
  summarise(defor_share = sum(aux)/n())

df_defor_out <- df_munic_template  %>% 
  left_join(df_deforestation) %>%
  select(defor_share) %>% 
  mutate(defor_share = ifelse(is.na(defor_share)| defor_share <0.01, 0.1, defor_share)) 

m <- min(df_defor_out$defor_share)

df_defor_out <- df_defor_out %>% 
  mutate(defor_share = log(defor_share/m))

write.csv(df_defor_out, "empirical_facts/03_data_outputs/tab/D.csv", row.names = FALSE)
# POPULATION -------------------------------------------------------------------

df_pop <- fread("empirical_facts/02_inputs/tabular/population.csv", skip  = 4, nrows = 5565) %>% 
  mutate(NM_MUN = substr(V1, 8, nchar(V1))) %>% 
  rename(population = V2)

df_pop_out <- df_munic_template  %>% 
  left_join(df_pop) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  select(population)

df_pop_out$population[173] <- mean(df_pop_out$population, na.rm = T)

write.csv(df_pop_out, "empirical_facts/03_data_outputs/tab/L.csv", row.names = FALSE)

# KAPPA ------------------------------------------------------------------------

sf_munic_template <- st_transform(sf_munic_template, 5880)

pts <- st_point_on_surface(sf_munic_template)
coords <- st_coordinates(pts)   # N x 2 (X,Y)
N <- nrow(coords)
dist_m <- st_distance(pts)      # units: meters, class "units"
dist_m <- 1000*units::drop_units(dist_m)  # numeric matrix
eta <- -1.19/(1-4.107)

kappa <- dist_m^(eta)

# Fix diagonal: set within-municipality cost to 1
diag(kappa) <- 1

write.table(
  kappa,
  file = "empirical_facts/03_data_outputs/kappa.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE)

# TRADE COSTS ------------------------------------------------------------------

r_final_grid <- rast("empirical_facts/02_inputs/raster/r_final_grid.tif")
r_tau <- rast("empirical_facts/02_inputs/raster/cost_raster_raster_proj.tif")
r_tau <- project(r_tau, r_final_grid, method = "bilinear")

r_tau <- crop(r_tau, r_final_grid)
r_tau <- mask(r_tau, r_final_grid)

r_tau <- resample(r_tau, r_final_grid, method = "near")

# Convert to RasterLayer for gdistance
r <- raster(r_tau)   # now class(r) is "RasterLayer"

# Build transition using conductance = 1/cost
tr <- transition(1 / r, transitionFunction = mean, directions = 8)
tr <- geoCorrection(tr, type = "c")

# Points: use SpatialPoints in the SAME CRS as r
sf_pts <- st_point_on_surface(st_transform(sf_munic_template, crs(r)))
sp_pts <- as(sf_pts, "Spatial")

# All-pairs least-cost
cd <- costDistance(tr, sp_pts)
cd <- as.matrix(cd)

cd <- cd/10000+1
diag(cd) <- 1
# Export (numeric, no headers)
write.table(cd, "empirical_facts/03_data_outputs/costdist.csv", sep = ",", row.names = FALSE, col.names = FALSE)
