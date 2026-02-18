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

# INPUTS -----------------------------------------------------------------------

load("empirical_facts/03_data_outputs/tab/dof.RData")
df_municipios <- fread("empirical_facts/02_inputs/tabular/ibge_munic_codes.csv")
sf_munic <- st_read("empirical_facts/02_inputs/vect/BR_Municipios_2020.shp") %>% 
  filter(SIGLA_UF %in% vec_lglamz) %>% 
  mutate(NM_MUN = toupper(iconv(NM_MUN, from = "UTF-8", to = "ASCII//TRANSLIT")))

# VOLUME AND NET VOLUMES -------------------------------------------------------

df_timber <- dt_all %>% 
  filter(uf_origem %in% vec_lglamz) %>% 
  group_by(municipio_origem) %>% 
  summarise(volume_out = sum(volume, na.rm = T)) %>% 
  rename(NM_MUN=municipio_origem)

df_timber2 <- dt_all %>% 
  filter(uf_destino %in% vec_lglamz) %>% 
  group_by(municipio_destino) %>% 
  summarise(volume_in = sum(volume, na.rm = T)) %>% 
  rename(NM_MUN=municipio_destino)

sf <- sf_munic %>% 
  left_join(df_timber) %>% 
  left_join(df_timber2) %>% 
  mutate(volume_in = ifelse(is.na(volume_in), 0, volume_in),
         volume_out = ifelse(is.na(volume_out), 0, volume_out),
         net_volume = volume_out-volume_in)

plt <-ggplot() +
  geom_sf(data = sf, aes(fill = volume_out)) +
  scale_fill_viridis_c(
    name = expression("Log Volume"~(m^3)),
    option = "viridis",
    trans = "log",
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

ggsave(plt, dpi = 600, filename = "empirical_facts/04_results/maps/timber_volumes_out.png",
       height = 4, width = 5, bg = "white")


mx <- max(abs(sf$net_volume), na.rm = TRUE)  # symmetric limits around 0
plt2 <-ggplot(sf) +
    geom_sf(aes(fill = net_volume)) +
    scale_fill_gradient2(
      name = expression(Net~Volume~(MMm^3)),  # change title if you want
      low = "darkred",
      high = "darkgreen",
      midpoint = 0,
      limits = c(-mx, mx),
      oob = scales::squish,
      trans = scales::modulus_trans(p = 0.05),  # try 0.2–0.5
      labels = label_number(accuracy = 1,     scale = 1/1e6)
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(12, "cm"),
        barheight = grid::unit(0.4, "cm")))

ggsave(plt2, dpi = 600, filename = "empirical_facts/04_results/maps/net_timber_volumes.png",
       height = 4, width = 5, bg = "white")

# TRANSPORTATION COSTS ---------------------------------------------------------

load("empirical_facts/03_data_outputs/tab/df_tau.RData")
load("empirical_facts/03_data_outputs/tab/df_template.RData")
load("empirical_facts/03_data_outputs/tab/df_fixed_effects.RData")

df <- df_template %>% 
  left_join(df_tau) %>% 
  left_join(df_fixed_effects) %>% 
  filter(sigla_uf %in% vec_lglamz)

plt <-ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = tau)) +
  coord_equal() +  
  scale_fill_viridis_c(
    name = "Normalized transportation costs",
    option = "viridis",
    trans = "log",
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

ggsave(plt, dpi = 600, filename = "empirical_facts/04_results/maps/tau.png",
       height = 4, width = 5, bg = "white")

# LIST OF MUNICS ---------------------------------------------------------------

sf_munic_template <- sf_munic %>% 
              left_join(df_timber) %>% 
              left_join(df_timber2) %>% 
              mutate(sample = ifelse(volume_in >0 | volume_out >0,1,0)) %>% 
              filter(sample == 1) %>% 
              mutate(id = row_number()) %>% 
              rename(cd_mun = CD_MUN) %>% 
              select(id, NM_MUN, cd_mun)

save(sf_munic_template, file = "empirical_facts/03_data_outputs/tab/sf_munic_template.RData")

df_munic_template <- sf_munic_template %>% st_drop_geometry()
save(df_munic_template, file = "empirical_facts/03_data_outputs/tab/df_munic_template.RData")

# WAGE -------------------------------------------------------------------------

df_wage <- fread("empirical_facts/02_inputs/tabular/wage.csv",   skip  = 4, nrows = 5565) %>% 
  mutate(NM_MUN = substr(V1, 8, nchar(V1))) %>% 
  rename(wage = V2)

df_wage_out <- df %>% 
  left_join(df_wage) %>% 
  distinct(id, .keep = TRUE)

# POPULATION X DEFORESTATION MAP -----------------------------------------------

sf_points <- sf_munic %>% 
  left_join(df_timber) %>% 
  left_join(df_timber2) %>% 
  mutate(sample = ifelse(volume_in >0 | volume_out >0,1,0)) %>% 
  filter(sample == 1) %>%
  mutate(id = row_number()) %>% 
  left_join(df_pop) %>% 
  filter(population > 300000) %>% 
  st_centroid()

df_mapbiomes_2010 <- fread("empirical_facts/03_data_outputs/tab/df_mapbiomes_2010.csv") %>% 
  mutate(land_use = case_when(land_use_2010 %in% c("forest", "noforest_natur_form") ~ "Vegetated",
                              land_use_2010 %in% c("novege_area", "pasture", "other_agri", "soy",
                                              "mosaic") ~ "Non-Vegetated",
                              TRUE ~ land_use_2010))

df <- df_template %>% 
  left_join(df_mapbiomes_2010) %>% 
  left_join(df_fixed_effects) %>% 
  filter(sigla_uf %in% vec_lglamz)

plt <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = land_use)) +
  coord_equal() +
  geom_sf(
    data = sf_points,
    aes(size = population),
    shape = 21,
    fill  = NA,
    color = "#00BFFF",   # DeepSkyBlue
    stroke = 0.8
  ) +
  scale_size_area(
    max_size = 8,
    breaks = c(3e5, 5e5, 1e6, 2e6),
    labels = label_number(scale_cut = cut_short_scale())  # <— replacement
  ) +
  scale_fill_manual(values = c("Vegetated" = "#1b7837", 
                               "Non-Vegetated" = "#f6e8c3",
                               "water" = "navy"),
                    breaks = c("Vegetated", 
                               "Non-Vegetated"),
                    labels = c("Vegetated", 
                               "Non-Vegetated"))+
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title.position = "top",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(color = "", fill = "", size = "Population")

ggsave(plt, dpi = 600, filename = "empirical_facts/04_results/maps/population.png",
       height = 4, width = 5, bg = "white")

# KAPPA MATRIX -----------------------------------------------------------------
# PI MATRIX --------------------------------------------------------------------
