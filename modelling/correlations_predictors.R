# load packages
library(dplyr)
library(tidyr)
library(corrplot)
library(GGally)

source("./modelling/model_pipeline.R")

####################################################

data_forests_census <- read.table("census_level.csv",
     header = TRUE, dec = ".", sep = ",")

data_forests_census <- pipeline_modelling(data_forests_census, is_census = TRUE)

# candidate predictors
predictors_raw_census <- data_forests_census %>% select(year, # nolint
    shannon_hill, # nolint
    wd_sd, sla_sd, cwm_wd, cwm_sla, # nolint
    sespd, # nolint
    warm_dry_climate, # nolint
    site_area_ha_log, # nolint
    soil_fertily, aluminum_phosphorus, phosphorus_magnesium) # nolint

# save
tiff("corr_candidate_preds_census.tiff",
    units = "cm", width = 26, height = 26, res = 177)

corrplot(predictors_raw_census %>% cor(), method = "number", order = "hclust")

dev.off()
system("open corr_candidate_preds_census.tiff")

# ####### selected predictors
# predictors_selected_census <- predictors_raw_census

# corplot_gg_census <- ggpairs(predictors_selected_census,
#     diag = list(continuous = wrap("densityDiag", fill = "palegreen1")),
#     lower = list(continuous = wrap("points", color = "red", alpha = 0.5))) +
#     theme_bw()

# ggsave("figureS10.tiff",
#     corplot_gg, he = 30, wi = 30, un = "cm", dpi = 600)
# system("open figureS10.tiff")


####################################################

data_forests <- read.table("census_int_level.csv",
     header = TRUE, dec = ".", sep = ",")

data_forests <- pipeline_modelling(data_forests, is_census = TRUE)

# candidate predictors
predictors_raw <- data_forests %>% select(year, # nolint
    shannon_hill, # nolint
    wd_sd, sla_sd, cwm_wd, cwm_sla, # nolint
    sespd, # nolint
    warm_dry_climate, # nolint
    site_area_ha_log, # nolint
    soil_fertily, aluminum_phosphorus, phosphorus_magnesium) # nolint

# save
tiff("corr_candidate_preds_census_int.tiff",
    units = "cm", width = 26, height = 26, res = 177)

corrplot(predictors_raw %>% cor(), method = "number", order = "hclust")

dev.off()
system("open corr_candidate_preds_census_int.tiff")

# ####### selected predictors
# predictors_selected <- predictors_raw

# corplot_gg <- ggpairs(predictors_selected,
#     diag = list(continuous = wrap("densityDiag", fill = "palegreen1")),
#     lower = list(continuous = wrap("points", color = "red", alpha = 0.5))) +
#     theme_bw()

# ggsave("figureS11.tiff",
#     corplot_gg, he = 30, wi = 30, un = "cm", dpi = 600)
# system("open figureS11.tiff")