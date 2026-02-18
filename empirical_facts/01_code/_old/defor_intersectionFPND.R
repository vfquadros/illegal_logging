

  library(data.table)
  library(dplyr)
  library(sf)
  library(ggplot2)
  
  directory <- getwd()
  sf_use_s2(FALSE)
  estados <- c("RO", "RR", "AM", "AP", 
               "TO", "MA", "PA", "AC") # MT foi retirado por falta de dados no dof
  
  # Change/Check CRS
  source(file.path(directory, "_functions/associateCRS.R"))
  polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")
  
  
  load(file.path(directory, "02_data_in/PRODES/blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2.Rdata"))
  prodes <- blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2
  blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2 <- NULL
  
  
  # Clean data
  prodes <- prodes %>% 
    filter(prodes_year >= 2018)
  
  
  
 for(i in estados){
   db <-  st_read(file.path(directory,"02_data_in/publicForests/SFB/2020/"), layer = paste0("CNFP_2020_", i))
   db <- st_make_valid(db)
   db <- st_transform(db, crs = polyconic)
   db <- st_buffer(db,0)
   
   # 
   lala <- st_intersection(prodes, db)
   
 } 
 
 
 
 
 
 
