rm(list = ls()) # cleaning the environment

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(caret)
library(DHARMa)
library(performance)
library(jtools)
library(interactions)
library(corrplot)
library(PerformanceAnalytics)
library(GGally)

source("./modelling/utils_mm.R")
source("./modelling/model_diag.R")
source("./modelling/model_pipeline.R")
source("./modelling/model_result_table.R")

options(na.action = na.fail)

################## census level data ##################  

data_forests_census <- read.table("census_level.csv",
     header = TRUE, dec = ".", sep = ",")

# dataset for the models
data_mod <- pipeline_modelling(data_forests_census, is_census = TRUE)

predictors_ny = data_mod %>% 
  select(site_area_ha_log, soil_fertily, aluminum_phosphorus, phosphorus_magnesium, 
         warm_dry_climate, shannon_hill, wd_sd, sla_sd,
         cwm_wd, cwm_sla, sespd, year)

dados_cor = predictors_ny

names(dados_cor)=c("site_area_log", "soil_fertility", "al_p", 
                   "p_mg", 
                   "climate", "shannon_hill", "wd_sd", "sla_sd", 
                   "cwm_wd", "cwm_sla",
                   "sespd", "year")

corplot_gg <- ggpairs(dados_cor, diag =
                       list(continuous = wrap("densityDiag", fill="palegreen1")),
                     lower = list(continuous = 
                                    wrap("points", color = "red", alpha = 0.5))) + 
  theme_bw()    

ggsave("figureS9.tiff", corplot_gg, he = 30, wi = 30, un = "cm", dpi = 600)
system("open figureS9.tiff")

################## census interval level data ##################  

data_forests_census <- read.table("census_int_level.csv",
                                  header = TRUE, dec = ".", sep = ",")

# dataset for the models
data_mod <- pipeline_modelling(data_forests_census, is_census = TRUE)

predictors_ny = data_mod %>% 
  select(site_area_ha_log, soil_fertily, aluminum_phosphorus, phosphorus_magnesium, 
         warm_dry_climate, shannon_hill, wd_sd, sla_sd,
         cwm_wd, cwm_sla, sespd, year)

dados_cor = predictors_ny

names(dados_cor)=c("site_area_log", "soil_fertility", "al_p", 
                   "p_mg", 
                   "climate", "shannon_hill", "wd_sd", "sla_sd", 
                   "cwm_wd", "cwm_sla",
                   "sespd", "year")

corplot_gg <- ggpairs(dados_cor, diag =
                        list(continuous = wrap("densityDiag", fill="palegreen1")),
                      lower = list(continuous = 
                                     wrap("points", color = "red", alpha = 0.5))) + 
  theme_bw()    

ggsave("figureS10.tiff", corplot_gg, he = 30, wi = 30, un = "cm", dpi = 600)
system("open figureS10.tiff")

