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

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
                              header = TRUE, dec = ".", sep = ",")

data_forests_census <- read.table("census_level.csv",
                                  header = TRUE, dec = ".", sep = ",")

# dataset for the models
data_mod <- pipeline_modelling(data_forests_census, is_census = TRUE)

vegetation_data <- left_join(vegetation_data, 
                             data_mod %>%
                               select(site_year, warm_dry_climate_scaled),
                             by = "site_year")
                             
vegetation_data <- vegetation_data[vegetation_data$dbhq > 0,]                      


mod1 <- lm(agwb_h ~ warm_dry_climate_scaled, data = vegetation_data)
r2_heigh <- summary(mod1)$r.squared

mod2 <- lm(agwb ~ warm_dry_climate_scaled, data = vegetation_data)
r2_stress <- summary(mod2)$r.squared

r2_heigh > r2_stress

cor(vegetation_data$agwb_h, vegetation_data$agwb)


mean(vegetation_data$agwb_h)
mean(vegetation_data$agwb)

plot(vegetation_data$agwb_h, vegetation_data$agwb)


