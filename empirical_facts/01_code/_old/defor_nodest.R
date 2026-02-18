# PREAMBLE ----
library(readxl)
library(data.table)
library(dplyr)
library(sf)

directory <- getwd()
sf_use_s2(FALSE)
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC") # MT foi retirado por falta de dados no dof

# LOAD, CHECK CRS, FIX TOPOLOGY