#######################################################################################################################
## Project: Amazon Access
## Author: Victor Quadros
## Date: 23/05/2024
## Content: Correlation and regressions about round-wood volume and deforestation in the final specification.
##          Difference between 2017 and 2022 "cross-section".
#######################################################################################################################

## PREAMBLE -----------------------------------------------------------------------------------------------------------
library(dplyr)
library(fixest)
library(ggplot2)
library(modelsummary)
library(data.table)
library(sf)

dir <- getwd()

## BUILDING --------------------------------------------------------------------
builder <- function(i,j){
load(file.path(dir, "03_data_out/panel.Rda"))
panel <- panel %>%
  filter(year %in% i:j) %>% 
  mutate(volTimber = replace(volTimber, is.na(volTimber), 0),
         vol_out = replace(vol_out, is.na(vol_out), 0),
         volume = replace(volume, is.na(volume), 0))

panel[order(rank(year)), volume := cumsum(volume), by = cd_mun]
panel[order(rank(year)), vol_out := cumsum(vol_out), by = cd_mun]
panel[order(rank(year)), volTimber := cumsum(volTimber), by = cd_mun]
panel[order(rank(year)), defor_prodes := cumsum(defor_prodes), by = cd_mun]
panel[order(rank(year)), defor_private := cumsum(defor_private), by = cd_mun]
panel[order(rank(year)), defor_nodest := cumsum(defor_nodest), by = cd_mun]
panel[order(rank(year)), defor_protected := cumsum(defor_protected), by = cd_mun]

df <- panel %>% 
  filter(year == j) %>% 
  mutate(area_km2 = log(area_km2),
         l_volume = log(volume + 0.01),
         l_vol_out = log(vol_out + 0.01),
         l_volTimber = log(volTimber + 0.01))

df <- as.data.frame(df)

return(df)
}

build_table <- function(a,b){
  load(file.path(dir, "03_data_out/panel.Rda"))
  panel <- panel %>%
    filter(year >= a, year <=b) %>% 
    mutate(volTimber = replace(volTimber, is.na(volTimber), 0))
  
  panel <- as.data.table(panel)
  
  panel[order(rank(year)), vol_out_acum := cumsum(volTimber), by = cd_mun]
  panel[order(rank(year)), defor_prodes_acum := cumsum(defor_prodes), by = cd_mun]
  panel[order(rank(year)), defor_private_acum := cumsum(defor_private), by = cd_mun]
  panel[order(rank(year)), defor_nodest_acum := cumsum(defor_nodest), by = cd_mun]
  panel[order(rank(year)), defor_protected_acum := cumsum(defor_protected), by = cd_mun]
  
  df <- panel %>% 
    filter(year == b)
  
  df <- as.data.frame(df)
  
  models <- list(feols(log(defor_prodes+0.01) ~ log(l_vol_out_acum+0.01), df),
                 feols(log(defor_prodes+0.01) ~ log(l_vol_out_acum+0.01) | uf, df),
                 feols(log(defor_protected+0.01) ~ log(l_vol_out_acum+0.01), df),
                 feols(log(defor_protected+0.01) ~ log(l_vol_out_acum+0.01) | uf, df),
                 feols(log(defor_nodest+0.01) ~ log(l_vol_out_acum+0.01), df),
                 feols(log(defor_nodest+0.01) ~ log(l_vol_out_acum+0.01) | uf, df),
                 feols(log(defor_private+0.01) ~ log(l_vol_out_acum+0.01), df),
                 feols(log(defor_private+0.01) ~ log(l_vol_out_acum+0.01) | uf, df))
  
  etable(models,
         tex = TRUE,
         digits = "r4",
         digits.stats = 2,
         #   se.row = T,
         coef.just = "c",
         se.below = T,
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), 
         depvar = F,
         headers = list(":_:" = c(rep("Total", 2), rep("Áreas Protegidas",2), rep("FPND", 2), rep("Privadas",2))),
         
         placement = "h!", 
         #label = auxLabel,
         #postprocess.tex = newAdjustbox_etable, # adjustbox only for table
         adjustbox = TRUE, # argument for newAdjustbox_etabl
         file = file.path(dir,paste0("04_outputs/relatfin/04_results/tables/regs_robust_", a, "_", b, ".tex")),
         replace = T
         
  )
  
}
## MAIN REGRESSION -------------------------------------------
df <- builder(2012,2022)
models <- list(feols(log(defor_prodes+0.01) ~ l_volume + area_km2, df),
               feols(log(defor_prodes+0.01) ~ l_volume + area_km2| uf, df),
               feols(log(defor_protected+0.01) ~ l_volume + area_km2, df),
               feols(log(defor_protected+0.01) ~ l_volume + area_km2 | uf, df),
               feols(log(defor_nodest+0.01) ~ l_volume + area_km2, df),
               feols(log(defor_nodest+0.01) ~ l_volume + area_km2| uf, df),
               feols(log(defor_private+0.01) ~ l_volume + area_km2, df),
               feols(log(defor_private+0.01) ~ l_volume + area_km2| uf, df))

etable(models,
       tex = TRUE,
       digits = "r4",
       digits.stats = 2,
       drop = c("Constant", "area_km2"),
       #   se.row = T,
       coef.just = "c",
       se.below = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), 
       depvar = FALSE,
       headers = list(":_:" = c(rep("Total", 2), rep("Áreas Protegidas",2), 
                                rep("FPND", 2), rep("Privadas",2))),
       extralines = list("-log(Área)" = rep("Yes", 8)),
                         #"-Price*(GD + IGD)" = rep(c("", "", "", "Yes"), 2)
       placement = "h!", 
       adjustbox = TRUE, # argument for newAdjustbox_etabl
       file = file.path(dir,"04_outputs/relatfin/tables/regs_all.tex"),
       replace = TRUE)

df2 <- data.frame(vol_out = rep(df$volume,3) , 
                  defor = c(df$defor_private,df$defor_protected,df$defor_nodest),
                  type = c(rep("Privadas",808),rep("Protegidas",808), 
                           rep("FPND",808)))

ggplot(df2, aes(x = log(vol_out+0.01), y = log(defor+0.01), color = type)) +
  geom_point(alpha=0.2) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_manual(values=c("Privadas" = "red","Protegidas" = "darkgreen",
                              "FPND" = "blue")) +
  labs(y = "Área Desmatada",
       x = "Sáidas de Toras")+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))
ggsave(plot = last_plot(),file.path(dir, "04_outputs/relatfin/plots/the_scatter.png"),
       width = 9, height = 5, dpi = 600)

## ROBUSTNESS DOF -----------------------------------------------------------------
df <- builder(2018,2022)
models <- list(feols(log(defor_prodes+0.01) ~ vol_out + area_km2, df[!(df$uf == "MT"),]),
               feols(log(defor_prodes+0.01) ~ vol_out + area_km2| uf, df[!(df$uf == "MT"),]),
               feols(log(defor_protected+0.01) ~ vol_out + area_km2, df[!(df$uf == "MT"),]),
               feols(log(defor_protected+0.01) ~ vol_out + area_km2 | uf, df[!(df$uf == "MT"),]),
               feols(log(defor_nodest+0.01) ~ vol_out + area_km2, df[!(df$uf == "MT"),]),
               feols(log(defor_nodest+0.01) ~ vol_out + area_km2 | uf, df[!(df$uf == "MT"),]),
               feols(log(defor_private+0.01) ~ vol_out + area_km2, df[!(df$uf == "MT"),]),
               feols(log(defor_private+0.01) ~ vol_out + area_km2 | uf, df[!(df$uf == "MT"),]))

etable(models,
       tex = TRUE,
       drop = c("Constant", "area_km2"),
       digits = "r4",
       digits.stats = 2,
       #   se.row = T,
       coef.just = "c",
       se.below = T,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), 
       depvar = F,
       headers = list(":_:" = c(rep("Total", 2), rep("Áreas Protegidas",2), rep("FPND", 2), rep("Privadas",2))),
       
       placement = "h!", 
       #label = auxLabel,
       #postprocess.tex = newAdjustbox_etable, # adjustbox only for table
       adjustbox = TRUE, # argument for newAdjustbox_etabl
       file = file.path(dir,"04_outputs/relatfin/tables/regs_dof.tex"),
       replace = T
       
)


## ROBUSTNESS TIMBERFLOW -------------------------------------------------------------
df <- builder(2012,2022) %>% filter(!(uf %in% c("MA", "TO")))

models <- list(feols(log(defor_prodes+0.01) ~ l_volTimber + area_km2, df),
               feols(log(defor_prodes+0.01) ~ l_volTimber + area_km2| uf, df),
               feols(log(defor_protected+0.01) ~ l_volTimber + area_km2, df),
               feols(log(defor_protected+0.01) ~ l_volTimber + area_km2 | uf, df),
               feols(log(defor_nodest+0.01) ~ l_volTimber + area_km2, df),
               feols(log(defor_nodest+0.01) ~ l_volTimber + area_km2 | uf, df),
               feols(log(defor_private+0.01) ~ l_volTimber + area_km2, df),
               feols(log(defor_private+0.01) ~ l_volTimber + area_km2 | uf, df))

etable(models,
       tex = TRUE,
       drop = c("Constant", "area_km2"),
       digits = "r4",
       digits.stats = 2,
       #   se.row = T,
       coef.just = "c",
       se.below = T,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), 
       depvar = F,
       headers = list(":_:" = c(rep("Total", 2), rep("Áreas Protegidas",2), rep("FPND", 2), rep("Privadas",2))),
       
       placement = "h!", 
       #label = auxLabel,
       #postprocess.tex = newAdjustbox_etable, # adjustbox only for table
       adjustbox = TRUE, # argument for newAdjustbox_etabl
       file = file.path(dir,"04_outputs/relatfin/tables/regs_timber.tex"),
       replace = T
       
)




## ROBUSTNESS LEVEL PLOT -------------------------------------------------------

df3 <- data.frame(vol_out = rep(df$volume,3) , 
                  defor = c(df$defor_private/df$area_private,df$defor_protected/df$area_protected,
                            df$defor_nodest/df$area_nodest),
                  type = c(rep("Privadas",808),rep("Protegidas",808), rep("FPND",808))) %>% 
                  filter(defor != "NaN")
                  

ggplot(df3, aes(x = vol_out/10000, y = defor, color = type)) +
  geom_point(alpha=0.2) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_manual(values=c("Privadas" = "red","Protegidas" = "darkgreen",
                              "FPND" = "blue")) +
  labs(y = "Desmatamento Proporcional (%)",
       x = expression("Volume de Toras" ~ (m^3)))+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(linetype = 3, color = "grey"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))
ggsave(plot = last_plot(),file.path(dir, "04_outputs/relatfin/plots/the_scatter_level.png"),
       width = 9, height = 5, dpi = 600)

## ROBUSTNESS YEAR-BY-YEAR