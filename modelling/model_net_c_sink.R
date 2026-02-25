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
m_global_net_c_sink <- lmer(net_c_sink ~
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled +
                            (warm_dry_climate_scaled +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

# check the model
# vif(m_global_net_c_sink)
check_collinearity(m_global_net_c_sink)

func_mult_diag(m_global_net_c_sink, data_mod$long, data_mod$lat, k_n)

# model selection
# run all possible models including year as predictor and n_max predictors
models_ranked_net_c_sink <- dredge(m_global_net_c_sink,
                                 m.lim = c(0, n_max),
                                 fixed =
                                   c("year_scaled", "site_area_ha_log_scaled", "soil_fertily_scaled", "phosphorus_magnesium_scaled",
                                     "aluminum_phosphorus_scaled", "warm_dry_climate_scaled"),
                                 trace = 2) # subset = abs(corr) < 0.7, 

# select models with delta AICc <= 4
selected_models_net_c_sink <-
  (subset(models_ranked_net_c_sink, subset = delta <= 4))
selected_models_net_c_sink

# average the selected models
averaged_model_net_c_sink <- summary(model.avg(selected_models_net_c_sink))
averaged_model_net_c_sink

model_result_table(averaged_model_net_c_sink, "net_c_sink")

# R2 of the best model
best_model_net_c_sink <- get.models(selected_models_net_c_sink, subset = 1)[[1]] # best model
r.squaredGLMM(best_model_net_c_sink)

check_collinearity(best_model_net_c_sink)

#################################################

# save.image(file = "models_net_c_sink.RData")
