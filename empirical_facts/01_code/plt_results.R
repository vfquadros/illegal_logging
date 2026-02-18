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

vec_lglamz <- c("RO", "RR", "AC", "AP", "AM", "MT", "PA", "TO", "MA")

source("empirical_facts/00_functions/ggplot_settings.R")

# INPUTS -----------------------------------------------------------------------

load("empirical_facts/03_data_outputs/tab/df_munic_template.RData")
sf_munic <- st_read("empirical_facts/02_inputs/vect/BR_Municipios_2020.shp") %>% 
  filter(SIGLA_UF %in% vec_lglamz) %>% 
  mutate(NM_MUN = toupper(iconv(NM_MUN, from = "UTF-8", to = "ASCII//TRANSLIT")))

for(phi in c(0,1,5)){
  
  df_w <- fread(paste0("model/03_results/w_phi",phi,".csv"))
  df_R <- fread(paste0("model/03_results/R_phi",phi,".csv"))
  df_D <- fread(paste0("model/03_results/D_phi",phi,".csv"))
  
  df_aux <- df_munic_template %>% 
    left_join(df_w) %>% 
    left_join(df_D) %>% 
    left_join(df_R) 
  
  sf <- sf_munic %>% 
    left_join(df_aux) 
  
  # MAP R ------------------------------------------------------------------------
  
  plt <-ggplot() +
    geom_sf(data = sf, aes(fill = R)) +
    scale_fill_viridis_c(
      name = "Population (thousands)",
      option = "viridis",
      labels = label_number(accuracy = 0.1)
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal") +
    guides(fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(12, "cm"),  # wider bar
      barheight = grid::unit(0.4, "cm")))
  
  ggsave(plt, dpi = 600, filename = paste0("model/R_phi",phi,".png"),
         height = 4, width = 5, bg = "white")
  
  # MAP D ------------------------------------------------------------------------
  
  plt <-ggplot() +
    geom_sf(data = sf, aes(fill = D)) +
    scale_fill_viridis_c(
      name = expression("Normalized"~(D_n)),
      option = "viridis",
      labels = label_number(accuracy = 0.1)
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal") +
    guides(fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(12, "cm"),  # wider bar
      barheight = grid::unit(0.4, "cm")))
  
  ggsave(plt, dpi = 600, filename = paste0("model/D_phi",phi,".png"),
         height = 4, width = 5, bg = "white")
  
  # HIST W -----------------------------------------------------------------------
  
  plt <- ggplot(df_aux, aes(x = log(w))) +
    geom_histogram(
      bins = 30,          # number of bins (you can adjust)
      fill = "deepskyblue", # bar fill color
      color = "black")+   # border color +
    labs(y = "Count", x = "") +
    theme_raw_stats
  ggsave(plt, dpi = 600, filename = paste0("model/whist_phi",phi,".png"),
         height = 4, width = 5, bg = "white")
  # HIST R -----------------------------------------------------------------------
  plt <- ggplot(df_aux, aes(x = log(R))) +
    geom_histogram(
      bins = 30,          # number of bins (you can adjust)
      fill = "deepskyblue", # bar fill color
      color = "black")+   # border color +
    labs(y = "Count", x = "") +
    theme_raw_stats
  ggsave(plt, dpi = 600, filename = paste0("model/Rhist_phi",phi,".png"),
         height = 4, width = 5, bg = "white")
}
