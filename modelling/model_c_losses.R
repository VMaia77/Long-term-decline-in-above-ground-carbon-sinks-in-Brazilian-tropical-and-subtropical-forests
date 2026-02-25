rm(list=ls()) # cleaning the environment

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

data_forests_census_int <- read.table("census_int_level.csv",
                                      header = TRUE, dec = ".", sep = ",")

# dataset for the models
data_mod <- pipeline_modelling(data_forests_census_int, is_census = FALSE)

# maximum number of predictors per model ~ 10 observations for each predictor
n_max <- round(nrow(data_mod) / 10)

# number of neighbours for SAC
k_n <- 20

# global model
m_global_c_losses <- lmer(log(c_losses) ~
                              site_area_ha_log_scaled +
                              soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled +
                              (warm_dry_climate_scaled +
                                 shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                                 cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                              (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

# check the model
# vif(m_global_c_losses)
check_collinearity(m_global_c_losses)

func_mult_diag(m_global_c_losses, data_mod$long, data_mod$lat, k_n)

# model selection
# run all possible models including year as predictor and n_max predictors
models_ranked_c_losses <- dredge(m_global_c_losses,
                                   m.lim = c(0, n_max),
                                   fixed =
                                     c("year_scaled", "site_area_ha_log_scaled", "soil_fertily_scaled", "phosphorus_magnesium_scaled",
                                       "aluminum_phosphorus_scaled", "warm_dry_climate_scaled"),
                                   trace = 2) # subset = abs(corr) < 0.7, 

# select models with delta AICc <= 4
selected_models_c_losses <-
  (subset(models_ranked_c_losses, subset = delta <= 4))
selected_models_c_losses

# average the selected models
averaged_model_c_losses <- summary(model.avg(selected_models_c_losses))
averaged_model_c_losses

model_result_table(averaged_model_c_losses, "c_losses")

# R2 of the best model
best_model_c_losses <- get.models(selected_models_c_losses, subset = 1)[[1]] # best model
r.squaredGLMM(best_model_c_losses)

check_collinearity(best_model_c_losses)

#################################################

# save.image(file = "models_c_losses.RData")
