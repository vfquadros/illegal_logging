#######################################################################################################################
## Project: Amazon Access
## Author: Victor Quadros
## Date: 23/05/2024
## Content: Plots maps used in the section "Dados"
#######################################################################################################################

## PREAMBLE --------------------------------------------------------------------
library(dplyr)
library(fixest)
library(ggplot2)
library('ggpubr')
library(modelsummary)
library(data.table)
library(sf)

dir <- getwd()
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC", "MT")
source(file.path(dir, "_functions/associateCRS.R"))
polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")

# DATA ------------------------------------------------------------------------

# Ufs
uf <- st_read(file.path(dir, "02_data_in/NEREUS_USP/UFEBRASIL.shp")) %>% 
    filter(CD_GEOCODU %in% c("11","14","13","16","17","21","15","12", "51"))

uf <- st_transform(uf, crs = polyconic)
# Munics
munic <- st_read(file.path(dir, "02_data_in/IBGE/MunicShape"), layer = "BR_Municipios_2020") %>% 
  filter(SIGLA_UF %in% estados)
munic <- st_transform(munic, crs = polyconic)
names(munic) <- c("cd_mun", "nm_mun", "uf", "area_km2", "geometry")

# Prodes
prodes <- st_read(file.path(dir, "02_data_in/INPE/PRODES/shapefiles"), layer = "yearly_deforestation") %>% 
  filter(state %in% estados)
prodes <- st_transform(prodes, crs = polyconic)
prodes <- st_buffer(prodes, dist=0)

# Protected
load(file.path(dir, "03_data_out/territories/sf_protectedAreas_legalAmazon.Rda"))
protected <- st_transform(protected, crs = polyconic)

# Nodest
load(file.path(dir, "03_data_out/territories/sf_FPND_legalAmazon.Rda"))
nodest <- st_transform(sf_FPND_legalAmazon, crs = polyconic)

# BIG MAPS ---------------------------------------------------------------------
load(file.path(dir, "03_data_out/panel.Rda"))

panel <- panel %>% filter(year == 2022)
area_amz <- sum(panel$area_km2)

# Protected
df <-  panel %>% filter(year == 2022, area_protected > 0)
r <- length(df$year)/length(panel$year)
s <- sum(df$area_protected)/area_amz

a <- ggplot() +
  geom_sf(data = uf, color = "lightgrey")+
  geom_sf(data = protected, fill = "darkgreen", color = "darkgreen")+
  labs(title = "Áreas Protegidas", 
       subtitle = paste0("% de municípios: ", round(100*r,2)," | % da área: ", round(100*s,2))) + 
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "white"),
         panel.border = element_rect(colour = "white", fill = NA),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5))

# Nodest
df <-  panel %>% filter(year == 2022, area_nodest > 0)
r <- length(df$year)/length(panel$year)
s <- sum(df$area_nodest)/area_amz

b <- ggplot() +
  geom_sf(data = uf, color = "lightgrey")+
  geom_sf(data = nodest, fill = "blue", color = "blue")+
  labs(title = "FPND", 
       subtitle = paste0("% de municípios: ", round(100*r,2)," | % da área: ", round(100*s,2))) + 
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         panel.background = element_rect(fill = "white"),
         panel.border = element_rect(colour = "white", fill = NA),
         panel.grid.major = element_line(color = "white"),
         plot.title = element_text(hjust = 0.5, size = 16),
         plot.subtitle = element_text(hjust = 0.5))

# Private
df <-  panel %>% filter(year == 2022, area_private > 0)
r <- length(df$year)/length(panel$year)
s <- sum(df$area_private)/area_amz

c <- ggplot() +
  geom_sf(data = uf, color = "red", fill = "red") +
  geom_sf(data = nodest, color = "lightgrey", fill = "lightgrey") +
  geom_sf(data = protected, color = "lightgrey", fill = "lightgrey") +
  labs(title = "Áreas Privadas", 
       subtitle = paste0("% de municípios: ", round(100*r,2)," | % da área: ", round(100*s,2))) + 
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "white"),
         panel.border = element_rect(colour = "white", fill = NA),
         plot.title = element_text(hjust = 0.5, size = 16),
         plot.subtitle = element_text(hjust = 0.5))

blank <- ggplot() + theme_void()
d <- ggarrange(
  ggarrange(a,b,blank, nrow = 1,
            widths = c(1, 1,0), legend = "none"),
  ggarrange(blank, c, blank, nrow = 1,
            widths = c(0.5, 1, 0.5), legend = "none"),
  nrow = 2,
  legend = "none"
)

ggsave(plot = d, file.path(dir, "04_outputs/relatfin/plots/territories.jpeg"),
       height = 6, width = 7, dpi = 350)

# SMALL MAPS -------------------------------------------------------------------

geom <- munic %>%  filter(nm_mun == "São Félix do Xingu") %>% 
  mutate(Legenda = rep(1, 1))

df2 <- st_intersection(geom, protected) %>% 
  mutate(Legenda = rep("Protegidas", 9)) %>% 
  select(Legenda, geometry)

df3 <- st_intersection(geom, nodest) %>% 
  mutate(Legenda = rep("FPND", 17))  %>% 
  select(Legenda, geometry)

df4 <- st_difference(geom, st_union(df2,df3)) %>% 
 mutate(Legenda = rep("Privadas", 153)) %>% 
  select(Legenda, geometry)

df <- rbind(df4,df2,df3)
df<-st_as_sf(df) 

df$Legenda <- factor(df$Legenda, levels = c("Privadas", "Protegidas", "FPND", 
                                            "Desmatamento"))

k <- ggplot(df) +
  geom_sf(mapping = aes(fill = Legenda), lwd = 0) +
  scale_fill_manual(values = c("Privadas" = "red","FPND" = "blue",
                               "Protegidas" ="darkgreen"))+
  labs(fill = "Legenda", title = "São Félix do Xingú (PA)") + 
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.text=element_text(size=12),
        legend.title = element_text(size=12)) 

pr <- st_intersection(prodes %>% filter(year <=2012), geom)
a <- ggplot(df) +
  geom_sf(aes(fill = Legenda), lwd = 0) +
  geom_sf(data = pr, color = "black", fill = "black") +
  scale_fill_manual(values = c("Privadas" = "red","FPND" = "blue",
                               "Protegidas" ="darkgreen"))+
  labs(title = "2012") +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

pr2 <- st_intersection(prodes %>% filter(year <=2018), geom)
b <- ggplot(df) +
  geom_sf(aes(fill = Legenda), lwd = 0) +
  geom_sf(data = pr2, color = "black", fill = "black") +
  scale_fill_manual(values = c("Privadas" = "red","FPND" = "blue",
                               "Protegidas" ="darkgreen"))+
  labs(title = "2018") +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

pr3 <- st_intersection(prodes, geom)
c <- ggplot(df) +
geom_sf(aes(fill = Legenda), lwd = 0) +
  geom_sf(data = pr3, color = "black", fill = "black") +
  scale_fill_manual(values = c("Privadas" = "red","FPND" = "blue",
                               "Protegidas" ="darkgreen"))+
  labs(title = "2022") +
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

d <- ggarrange(a,b,c, nrow=1,ncol=3, common.legend = TRUE, legend = "bottom")
ggsave(plot = d, file.path(dir, "04_outputs/relatfin/plots/sfelix.png"),
       height = 6, width = 7, dpi = 350)

# MAP S. FELIX PRESENTATION

pr3 <- pr3 %>%  
  mutate(Legenda = rep("Desmatamento", 24201)) %>%
  select(Legenda, geometry)

df <- rbind(df, pr3)
df<-st_as_sf(df) 

df$Legenda <- factor(df$Legenda, levels = c("Privadas", "Protegidas", "FPND", 
                                            "Desmatamento"))

e <- ggplot(df) +
  geom_sf(aes(fill = Legenda), lwd = 0) +
  geom_sf(data = pr3, color = "black", fill = "black") +
  labs(title = "São Félix do Xingu (PA)") +
  scale_fill_manual(values = c("Privadas" = "red","FPND" = "blue",
                               "Protegidas" ="darkgreen", "Desmatamento" = "black"))+
  theme(panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14)) +
  guides(fill = guide_legend(
    nrow = 2,
    byrow = TRUE))

ggsave(plot = e, file.path(dir, "04_outputs/relatfin/plots/sfelix_present.png"),
       height = 6, width =4.5, dpi = 350)
