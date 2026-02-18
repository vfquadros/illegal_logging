# PREAMBLE ----
library(readxl)
library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(modelsummary)

directory <- getwd()
sf_use_s2(FALSE)
estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC") # MT foi retirado por falta de dados no dof


# PRODES DEFORESTATION ----

# Load data
munic <- st_read(file.path(directory, "02_data_in/IBGE/MunicShape"), layer = "BR_Municipios_2020")

load(file.path(directory, "02_data_in/PRODES/blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2.Rdata"))
prodes <- blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2
blt_lcv_dfrst_laz_maskShiftPostDeforestProdes_sf_2 <- NULL

toras <- bind_rows(fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_2018.csv"),dec=","),
                   fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_2019.csv"),dec=","),
                   fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_2020.csv"),dec=","))

# Clean data
prodes <- prodes %>% 
  filter(prodes_year >= 2018)

saidas <- toras %>% 
  filter(uf_origem %in% estados) %>% 
  group_by(code_origem) %>% 
  summarise(volume_saindo = sum(as.numeric(volume), na.rm = T))

areas <- st_drop_geometry(munic) %>% filter(SIGLA_UF %in% estados)
df <- subset(areas,select = c(CD_MUN, AREA_KM2))
saidas <- merge(saidas, areas,by.x = "code_origem", by.y = "CD_MUN", all.y = TRUE)

saidas <- saidas %>%
  mutate(volume_saindo = replace(volume_saindo, is.na(volume_saindo), 0))

# Change/Check CRS
source(file.path(directory, "_functions/associateCRS.R"))
polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")

munic <- st_transform(munic, crs = polyconic)
prodes <- st_transform(prodes, crs = polyconic)
prodes <- st_buffer(prodes, dist=0)
st_crs(munic) == st_crs(prodes)

# Intersection
inter <- st_intersection(munic, prodes)

inter$area <- as.numeric(st_area(inter))/10^6 # dividing to get km^2
inter<-st_drop_geometry(inter)

inter <- inter %>% 
  group_by(CD_MUN) %>% 
  summarise(defor_prodes = sum(area))

df <- merge(df, inter, by = "CD_MUN", all.x = TRUE)
df <- df %>%
  mutate(defor_prodes = replace(defor_prodes, is.na(defor_prodes), 0))

# MAPBIOMAS ----

# Load
biomas <- read_excel(file.path(directory, "02_data_in/mapBiomas/deforestation/tabela_desmatamento_vegetacao_secundaria_mapbiomas_col8.xlsx"),sheet = "CITY_STATE_BIOME")

# Clean
biomas <- biomas %>% 
  filter(level_4 == "Forest Formation", 
         dr_class_name == "Supressão Veg. Primária",
         state %in% estados,
         biome == "Amazônia")


biomas <- biomas %>% 
  mutate(defor_mapbiomas = (biomas$'2018'+biomas$'2019'+biomas$'2020')/100)

biomas <- subset(biomas, select = c("GEOCODE", "defor_mapbiomas"))

df <- merge(df, biomas, by.x = "CD_MUN", by.y = "GEOCODE", all.x = TRUE)
df <- df %>%
  mutate(defor_mapbiomas = replace(defor_mapbiomas, is.na(defor_mapbiomas), 0))

# CONSERVATION UNITS ---- 

# Load data
uc <- st_read(file.path(directory, "02_data_in/protectedAreas/MMA"), layer = "ucstodas", options = "ENCODING=WINDOWS-1252")

# Check/Change CRS
st_crs(uc) <- "+proj=longlat +ellps=aust_SA +towgs84=-67.35,3.88,-38.22"
uc<-st_transform(uc, crs=polyconic)
st_crs(uc) == st_crs(prodes)
uc <- st_buffer(uc, dist = 0) # fix topology

# Intersection 
inter2 <- st_intersection(uc, prodes)
inter <- st_intersection(munic,inter2) 

inter$area <- as.numeric(st_area(inter))/10^6

inter<-st_drop_geometry(inter)

inter <- inter %>% 
  group_by(CD_MUN) %>% 
  summarise(defor_uc = sum(area))

# Merge
df <- merge(df, inter, by = "CD_MUN", all.x = TRUE)
df <- df %>%
  mutate(defor_uc = replace(defor_uc, is.na(defor_uc), 0))

# INDIGINOUS SITES ----

# Load data
ind <- st_read(file.path(directory, "02_data_in/protectedAreas/FUNAI"), layer = "ti_sirgasPolygon", options = "ENCODING=WINDOWS-1252")

# Check/Change CRS
ind<-st_transform(ind, crs=polyconic)
st_crs(ind) == st_crs(prodes)
ind <- st_buffer(ind, dist = 0) # fix topology

# Intersection 
inter2 <- st_intersection(ind, prodes)
inter <- st_intersection(munic,inter2) 

inter$area <- as.numeric(st_area(inter))/10^6

inter<-st_drop_geometry(inter)

inter <- inter %>% 
  group_by(CD_MUN) %>% 
  summarise(defor_ind = sum(area))

# Merge
df <- merge(df, inter, by = "CD_MUN", all.x = TRUE)
df <- df %>%
  mutate(defor_ind = replace(defor_ind, is.na(defor_ind), 0))

# NON-DESTINATED AREAS ----

list <- list()
for(estado in estados){
  x <- st_read(sprintf(file.path(directory, "02_data_in/publicForests/SFB/2020/CNFP_2020_%s.shp"), estado))
  
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

df <- merge(df, nodest, by = "CD_MUN", all.x = TRUE)
df <- df %>%
  mutate(defor_nodest = replace(defor_nodest, is.na(defor_nodest), 0))

saidas <- subset(saidas, select = c(code_origem, volume_saindo))
names(saidas) <- c("CD_MUN", "toras")

df <- merge(df, saidas, by = "CD_MUN", all.x = TRUE)
df <- df %>%
  mutate(toras = replace(toras, is.na(toras), 0))

#save(df, file = file.path(directory, "03_data_out/base_final.RData"))

# PRIVATE AREAS----

load(file.path(directory, "03_data_out/sf_protectedAreas_legalAmazon.Rda"))
protected <- st_transform(protected_legalAmazon, crs = polyconic)
inter2 <- st_intersection(prodes, protected)
inter <- st_intersection(munic, inter2)

protected$areas <- as.numeric(st_area(inter))/10^6

#private_defor <- total - protected - nodest
# SCATTER N' REGRESS! ----

# Prodes Deforestation
ggplot(df, aes(x = log(toras + 0.01), y = log(defor_prodes  + 0.01))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot Desmatamento Prodes (m^2) x Volume saindo (m^3) (logs)") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), 
       filename = file.path(directory, "04_outputs/regs/prodes.png"), 
       width = 10, height = 7) 

model_prodes <- lm(log(defor_prodes + 0.01) ~ log(toras + 0.01), data = df)

# MapBiomas Deforestation
ggplot(df, aes(x = log(toras + 0.01), y = log(defor_mapbiomas  + 0.01))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot Desmatamento MapBiomas (m^2) x Volume saindo (m^3) (logs)") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), 
       filename = file.path(directory, "04_outputs/regs/mapbiomas.png"), 
       width = 10, height = 7) 

model_mapbiomas <- lm(log(defor_mapbiomas + 0.01) ~ log(toras + 0.01), data = df)

# Conservation Units
ggplot(df, aes(x = log(toras + 0.01), y = log(defor_uc  + 0.01))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot Desmatamento Unidades de Conservação (m^2) x Volume saindo (m^3) (logs)") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), 
       filename = file.path(directory, "04_outputs/regs/uc.png"), 
       width = 10, height = 7, dpi = 600) 

model_uc <- lm(log(defor_uc + 0.01) ~ log(toras + 0.01), data = df)

# Indiginous sites
ggplot(df, aes(x = log(toras + 0.01), y = log(defor_ind  + 0.01))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot Desmatamento Terras Indígenas (m^2) x Volume saindo (m^3) (logs)") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), 
       filename = file.path(directory, "04_outputs/regs/ind.png"), 
       width = 10, height = 7) 

model_ind <- lm(log(defor_ind + 0.01) ~ log(toras + 0.01), data = df)

# No destination
ggplot(df, aes(x = log(toras + 0.01), y = log(defor_nodest  + 0.01))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot Desmatamento Sem destinação (m^2) x Volume saindo (m^3) (logs)") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), 
       filename = file.path(directory, "04_outputs/regs/nodest.png"), 
       width = 10, height = 7) 

model_nodest <- lm(log(defor_nodest + 0.01) ~ log(toras + 0.01), data = df)

models <- list(model_prodes, model_mapbiomas, model_uc, model_ind, model_nodest)

modelsummary(models,  estimate = "{estimate}{stars}", output = "latex")
