######################################################################################
## Project: Amazon Access                                                           ##
## Author: Victor Quadros                                                           ##
## Date: 23/05/2024                                                                 ##
## Content: Generates all the results added to the `Relatório Final` of the project.## 
######################################################################################

# PREAMBLE ---------------------------------------------------------------------

library(sf)          #
library(dplyr)       #
library(data.table)  #
library(ggplot2)     #
library(xtable)      #
library('ggpubr')
library(scales)

dir <- getwd()
options(scipen=999)

estados <- c("RO", "RR", "AM", "AP", 
             "TO", "MA", "PA", "AC", "MT")
source(file.path(dir, "_functions/associateCRS.R"))
polyconic <- AssociateCRS("Proj_SIRGAS2000polyconic")

load(file.path(dir, "03_data_out/panel.Rda"))
#ufs
uf <- st_read(file.path(dir, "02_data_in/NEREUS_USP/UFEBRASIL.shp")) %>% 
  filter(CD_GEOCODU %in% c("11","14","13","16","17","21","15","12", "51"))
uf <- st_transform(uf, crs = polyconic)
# Munics
munic <- st_read(file.path(dir, "02_data_in/IBGE/MunicShape"), layer = "BR_Municipios_2020") %>% 
  filter(SIGLA_UF %in% estados)
munic <- st_transform(munic, crs = polyconic)
names(munic) <- c("cd_mun", "nm_mun", "sigla_uf", "area_km2", "geometry")

# Ibama's posts
ibama <- st_read(file.path(dir, "02_data_in/Ibama/superintendencias"), layer = "adm_edif_pub_civil_ibama_p") %>% filter(estado %in% estados)
sf_posts <- st_transform(ibama, crs = polyconic)

load(file.path(dir, "03_data_out/territories/sf_FPND_legalAmazon.Rda"))
nodest <- st_transform(sf_FPND_legalAmazon, crs = polyconic)

# Main data
load(file.path(dir, "03_data_out/longDiff.Rda"))

# INTRODUCTION ----

# Table 1

# Table 1 and 2
toras <- bind_rows(fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_2018.csv"),dec=","),
                   fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_2019.csv"),dec=","),
                   fread(file.path(directory, "02_data_in/Ibama/dof/_cleanData/toras/toras_2020.csv"),dec=","))

tot_vol = sum(as.numeric(toras$volume), na.rm= TRUE)
sp <-  toras %>%
  group_by(nome_popular) %>% 
  summarise(vol = round(sum(as.numeric(volume), na.rm = TRUE)/1000,1)) %>% 
  arrange(vol) %>%
  top_n(10)

sp <- sp[order(-sp$vol), ]
sp <- rbind(sp, c("Total", round((tot_vol/1000), 3)))
sp$perc = round(100*(as.numeric(sp$vol)/(tot_vol/1000)), 3)
xtable(sp, include.rownames = FALSE)

# DATA AND DESCRIPTIVE STATISTICS ----------------------------------------------

# Table 2
toras <- toras %>% 
  filter(uf_origem %in% estados)

tot_vol = sum(as.numeric(toras$volume), na.rm= TRUE)
sp <-  toras %>%
  group_by(nome_popular) %>% 
  summarise(vol = round(sum(as.numeric(volume), na.rm = TRUE)/1000,1)) %>% 
  arrange(vol) %>%
  top_n(10)

sp <- sp[order(-sp$vol), ]
sp <- rbind(sp, c("Total", round((tot_vol/1000), 3)))
sp$perc = round(100*(as.numeric(sp$vol)/(tot_vol/1000)), 3)
xtable(sp, include.rownames = FALSE)

# Vol toras
df <- builder(2012,2022) # carregue o arquivo dof_correlations_toras_def.R
panel_geom <- munic %>% 
  left_join(df, by = "cd_mun") %>%
  mutate(volume = replace(volume, volume==0, NA),
         defor_prodes = replace(defor_prodes,defor_prodes==0, NA),
         volume2 = volume/1000)

a <- ggplot() +
  geom_sf(data = panel_geom,aes(fill = volume2), linewidth = 0, alpha = 0.9) +
  labs(title = "Saídas de toras acumuladas (2012-2022)")+
  scale_fill_viridis_c(
    trans = "log",
    breaks = c(.2,1,10,50,250,1000),
    name = expression("Saídas de toras" ~(1000*m^3)),
    option = "H",
    na.value = "grey50",
    guide = guide_legend(
      keyheight = unit(4, units = "mm"),
      keywidth = unit(5, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1)) +
  theme(legend.position = c(0.21, 0.1),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        legend.text=element_text(size=9),
        legend.title = element_text(size=11))

b <- ggplot(panel_geom[panel_geom$volume2>0,],aes(x = volume2)) +
  geom_histogram(alpha = 0.7, 
                 bins = 30, fill = "darkred") +
  labs(x = expression("Saídas de toras" ~(dam^3)), y = "Número de Municípios") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))
c <- ggarrange(a,b)


ggsave(plot = annotate_figure(c, top = text_grob("Volume acumulado de toras (2012-2022)", 
                                                 color = "black", face = "bold", size = 14))
       , file.path(dir, "04_outputs/relatfin/plots/dscrpt_toras.jpeg"),
       height = 4, width = 9, dpi = 450)

# Defor 
a <- ggplot(panel_geom) +
  geom_sf(aes(fill = defor_prodes/area_km2.x), linewidth = 0, alpha = 0.9) +
  labs(title = "Desmatamento acumulado (2012-2022) por área (%)")+
  scale_fill_viridis_c(
    trans = "log",
    breaks = c(0.0001,0.001,0.01,0.05,0.15),
    labels = label_percent(),
    name = "Area Desmatada (%)",
    option = "H",
    na.value = "grey50",
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1,
      na.value = "grey50")) +
  theme(legend.position = c(0.21, 0.1),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        legend.text=element_text(size=13),
        legend.title = element_text(size=15))

ggsave(plot = a, file.path(dir, "04_outputs/relatfin/plots/dscrp_defor.jpeg"),
       height = 6, width = 10, dpi = 450)


ggplot(df, aes(x = defor_prodes_acum, y = defor_mapbio_acum/100)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue",lwd = 0.8) +  
  geom_abline(color="red", lwd = 0.8, lty = 2)+
  labs(x = expression("PRODES" ~ (km^2)), y = expression("MapBiomas" ~ (km^2)),
       title = "Correlação entre medidas de desmatamento  acumulado (2012-2022)") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
ggsave(plot = last_plot(), file.path(dir, "04_outputs/relatfin/plots/comparing_defors.png"),
       height = 5, width = 9, dpi = 450)

#Plot vols
  
estados <- unique(panel$uf)
data <- vector('list', length(estados))
for(i in estados){
  message(i)
  data[[i]] <- local({
    i <- i
    ggplot(panel %>% filter(year >= 2012) %>% group_by(uf,year) %>% 
             summarise(vol_out = sum(vol_out, na.rm = TRUE),
                       volTimber = sum(volTimber, na.rm = TRUE)) %>% 
             filter(uf == i) %>% 
             mutate(group1 = rep("DOF", 11),
                    group2 = rep("Timberflow", 11)),
           aes(x = year)) +
      geom_line(aes(y = vol_out/1000, color = group1), lwd = 1) +
      geom_line(aes(y = volTimber/1000, color = group2), lwd = 1, linetype = "dashed") +
      labs(title = i, y = "Volume", x = "Anos") +
      scale_x_continuous(breaks=c(2012,2017,2022)) + 
      theme(panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(linetype = 3, color = "grey"),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.key = element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=12),
            plot.title = element_text(hjust = 0.5, size = 14))
    
  })
}

k <- ggarrange(data$MT, data$PA, data$AM, data$RR, data$AC, data$AP, data$RO, data$MA,
          data$TO, ncol = 3, nrow = 3, common.legend = TRUE)

ggsave(plot = k, file.path(dir, "04_outputs/relatfin/plots/volumes.png"),
       height = 5, width = 9, dpi = 450)

df2 <- panel %>% 
  group_by(uf, year) %>% 
  summarise(vol_out = sum(vol_out, na.rm = TRUE),
            volTimber = sum(volTimber, na.rm = TRUE))

ggplot(df2, aes(x = year)) +
  geom_line(aes(y = vol_out/1000), color = "red", lwd = 1) +
  geom_line(aes(y = volTimber/1000), color = "blue", lwd = 1, linetype = "dashed") +
  labs(title = "PA", x = "Ano",
       y = expression("Saídas de toras" ~ (1000*m^3))) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 14))
  


# RESULTS ----------------------------------------------------------------------
input(file.path(dir, "01_code/subcodes/dof_correlations_toras_def.R"))

# Time Regs

models_list <- list(
  df1 = builder(2012,2013), df2 = builder(2012,2014), df3 = builder(2012,2015),
  df4 = builder(2012,2016), df5 = builder(2012,2017), df6 = builder(2012,2018),
  df7 = builder(2012,2019), df8 = builder(2012,2020), df9 = builder(2012,2021),
  df10 = builder(2012,2022))

names <- c("defor_prodes", "defor_protected", "defor_nodest", "defor_private")
names2 <- c("Desmatamento Total", "Áreas Protegidas", "FPND", "Áreas Privadas")
colors <- c("purple", "darkgreen", "blue", "red")
j=1
for(j in 1:4){
models_list <- lapply(models_list, function(df) df %>% mutate(var.y =df[,names[j]]))
models <- lapply(models_list, function(df) feols(log(var.y+0.01) ~ l_volume + area_km2| uf, 
                                                 df ))
coef_data <- lapply(models, function(model) tidy(model, conf.int = TRUE))
coef_data <- do.call(rbind, coef_data) %>% filter(term == "l_volume")
coef_data$model_index <- 2013:2022

k <- ggplot(coef_data, aes(x = model_index, y = estimate)) +
  geom_point(color = colors[j], size = 0.5) + #ylim(0.05,0.21) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = colors[j]) +
  labs(x = "Ano", y = "Correlação toras-desmatamento", title = names2[j]) +
  scale_x_continuous(breaks=2013:2022)+ ylim(-0.05, 0.27) + 
  geom_hline(yintercept = 0, color = "black", lwd = 1.05) + 
theme(panel.background = element_blank(),
panel.border = element_blank(),
panel.grid.major = element_line(linetype = 3, color = "grey"),
panel.grid.minor = element_blank(),
axis.ticks = element_blank(),
legend.position = "bottom",
legend.key = element_blank(),
legend.title = element_blank(),
legend.text=element_text(size=12),
plot.title = element_text(hjust = 0.5, size = 14))
ggsave(plot = k, sprintf(file.path(dir, "04_outputs/relatfin/plots/regs_time%s.png"), names[j]),
       height = 3, width = 5, dpi = 450)

}

# Coef Plot presentation

df <- builder(2012,2022)
coef_data <- bind_rows(tidy(feols(log(defor_prodes+0.01) ~ l_volume + area_km2| uf,df), conf.int = TRUE),
                       tidy(feols(log(defor_private+0.01) ~ l_volume + area_km2| uf,df), conf.int = TRUE),
                       tidy(feols(log(defor_nodest+0.01) ~ l_volume + area_km2| uf,df), conf.int = TRUE),
                       tidy(feols(log(defor_protected+0.01) ~ l_volume + area_km2| uf,df),conf.int = TRUE)) %>% 
  filter(term == "l_volume") %>% 
  mutate(gp = c("Total", "Privadas", "FPND", "Protegidas"))

k<- ggplot(coef_data, aes(x = gp, y = estimate)) +
  geom_point(aes(color = gp), size = 0.5) + #ylim(0.05,0.21) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high,color = gp), width = 0.2) +
  labs(y = "Coeficiente", x = "") +
  scale_color_manual(values = c("Privadas" = "red","FPND" = "blue",
                               "Protegidas" ="darkgreen", "Total" = "purple"))+
  geom_hline(yintercept = 0, color = "black", lwd = 1.05) + 
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 14))
ggsave(plot = k, file.path(dir, "04_outputs/relatfin/plots/regs_presen.png"),
       height = 4, width = 6, dpi = 450)
