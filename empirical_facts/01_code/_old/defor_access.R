#######################################################################################################################
## Project: Amazon Access
## Author: Victor Quadros
## Date: 03/05/2024
## Content: Builds the accessibility variable and makes preliminary analysis.
#######################################################################################################################

# PREAMBLE ---------------------------------------------------------------------
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)

dir <- getwd()

estados <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA")

# LOADING ----------------------------------------------------------------------
data1 <- st_read(file.path(dir, "02_data_in/MINFRA/Hidrovias/Hidrovias.shp"))
data2 <- st_read(file.path(dir, "02_data_in/MINFRA/Rodovias/Rodovias.shp"))
ufs <- st_read(file.path(dir, "02_data_in/NEREUS_USP/UFEBRASIL.shp"))
munic <- st_read(file.path(dir, "02_data_in/IBGE/MunicShape")) %>%
  filter(munic$SIGLA_UF %in% estados)

source(file.path(directory, "_functions/associateCRS.R"))
polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")

data1 <- st_transform(data1, crs=polyconic)
data2 <- st_transform(data2, crs=polyconic)
ufs <- st_transform(ufs, crs=polyconic)
munic <- st_transform(munic, crs = polyconic)

# PLOTTING ---------------------------------------------------------------------
ggplot() +
  geom_sf(data = ufs %>% filter(as.numeric(CD_GEOCODU) <= 21 | as.numeric(CD_GEOCODU) ==51))+
  geom_sf(data = data1 %>% filter(est_origem %in% estados), color = "blue")+
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom", 
        legend.key = element_blank(),
        #title = element_text(size = 12),
        title = element_blank(),
        legend.text = element_text(size=12), 
        legend.direction = "vertical", 
        plot.margin = grid::unit(c(0,0,0,0), "in"))

ggsave(plot = last_plot(), file.path(directory, "04_outputs/access/rivers.png"))

ggplot() +
  geom_sf(data = ufs %>% filter(as.numeric(CD_GEOCODU) <= 21 | as.numeric(CD_GEOCODU) ==51))+
  geom_sf(data = data2 %>% filter(sg_uf %in% estados), color = "red")+
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom", 
        legend.key = element_blank(),
        #title = element_text(size = 12),
        title = element_blank(),
        legend.text = element_text(size=12), 
        legend.direction = "vertical", 
        plot.margin = grid::unit(c(0,0,0,0), "in"))

ggsave(plot = last_plot(), file.path(directory, "04_outputs/access/roads.png"))


# INTERSECTION -----------------------------------------------------------------

inter <- st_intersection(munic, data1)
inter$len = as.numeric(st_length(inter))/1000
inter <- st_drop_geometry(inter)

access <- inter %>% 
  group_by(CD_MUN) %>% 
  summarise(len_rivers = sum(len))

inter2 <- st_intersection(munic, data2)
inter2$len = as.numeric(st_length(inter2))/1000
inter2 <- st_drop_geometry(inter2)

inter2 <- inter2 %>% 
  group_by(CD_MUN) %>% 
  summarise(len_roads = sum(len))

access <- merge(access, inter2, all = T) %>% 
  mutate(len_rivers = ifelse(is.na(len_rivers), 0, len_rivers)) %>% 
  mutate(len_roads = ifelse(is.na(len_roads), 0, len_roads))


  





# REGRESS ----------------------------------------------------------------------

load(file.path(dir, "03_data_out/panel.Rda"))

panel <- merge(panel, access, by.x = "cd_mun", by.y = "CD_MUN", all.x = TRUE)
panel$fe <- paste(panel$year, panel$uf)

df <- panel %>% 
  filter(uf !="MT",
         year >= 2018) %>% 
  mutate(vol_out = replace(vol_out, is.na(vol_out), 0)) %>% 
  mutate(len_rivers = replace(len_rivers, is.na(len_rivers), 0)) %>% 
  mutate(len_roads = replace(len_roads, is.na(len_roads), 0))

names = c("defor_prodes", "defor_protected", "defor_private", "defor_nodest")
names_acum = c("defor_prodes_acum", "defor_protected_acum", 
               "defor_private_acum", "defor_nodest_acum")
areas = c("area_km2", "area_protected", "area_private", "area_nodest")
titles = c("Lgl Amz - MT (Flow)", "Protected Areas (Flow)", "Private areas (Flow)", "Non-destinated areas (Flow)")
titles_acum = c("Lgl Amz - MT (Stock)", "Protected Areas (Stock)", "Private areas (Stock)", "Non-destinated areas (Stock)")

df<-as.data.frame(df)
j=1
for(i in names){
  df$var.y <- df[,names[j]]
  df$var.area <- df[,areas[j]]
  df$var.y_acum <- df[,names_acum[j]]
  
  models <- list(feols(log(var.y+0.01) ~ log(vol_out+0.01) + len_rivers + len_roads, df),
                 feols(log(var.y+0.01) ~ log(vol_out+0.01) + len_rivers + len_roads| fe, df),
                 feols(var.y/var.area ~ log(vol_out+0.01)+ len_rivers + len_roads, df),
                 feols(var.y/var.area ~ log(vol_out+0.01) + len_rivers + len_roads| fe, df),
                 feols(log(var.y_acum+0.01) ~ log(vol_out+0.01)+ len_rivers + len_roads, df),
                 feols(log(var.y_acum+0.01) ~ log(vol_out+0.01) + len_rivers + len_roads | fe, df),
                 feols(var.y_acum/var.area ~ log(vol_out+0.01)+ len_rivers + len_roads, df),
                 feols(var.y_acum/var.area ~ log(vol_out+0.01)+ len_rivers + len_roads | fe, df))
  
  etable(models,
         tex = TRUE,
         digits = "r4",
         digits.stats = 2,
         #   se.row = T,
         coef.just = "c",
         se.below = T,
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), 
         depvar = F,
         headers = list(":_:" = c(rep("Log-Log (Flow)", 2), rep("Perc-Log (Flow)",2), rep("Log-Log (Stock)", 2), rep("Perc-Log (Stock)",2))),
         
         placement = "h!", 
         #label = auxLabel,
         #postprocess.tex = newAdjustbox_etable, # adjustbox only for table
         #table_width = 0.65, # argument for newAdjustbox_etabl
         file = sprintf(file.path(dir, 
                                  "04_outputs/access/tables/regs_%s.tex"), names[j]),
         replace = T
         
  )
  
  j = j + 1
}

ggplot(df %>% filter(year ==2022), aes(x = len_rivers)) +
  geom_histogram(fill = "red", color = "red", alpha = 0.7) +
  labs(title = "Access (Rivers)", x = "Lenght (km)", 
       y = "Municipalities") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), file.path(directory, "04_outputs/access/hist_rivers.png"))

ggplot(df %>% filter(year ==2022), aes(x = len_roads)) +
  geom_histogram(fill = "red", color = "red", alpha = 0.7) +
  labs(title = "Access (Roads)", x = "Lenght (km)", 
       y = "Municipalities") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), file.path(directory, "04_outputs/access/hist_roads.png"))


