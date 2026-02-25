# load packages

library(dplyr)
library(tidyr)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)
library(ggsci)

# read census level data

data_forests1 <-
    read.table("census_level.csv", header = TRUE, dec = ".", sep = ",")
head(data_forests1)

# correlation between functional and phylogenetic diversities with sampling effort and species richness

# figure S5

diversity_cor <- data_forests1 %>%
    select(richness_hill, shannon_hill, simpson_hill, wd_sd, sla_sd,
        richness_obs,
        pd, sespd, sampled_area_ha)

# save
tiff("figureS5.tiff", units = "cm", width = 20, height = 20, res = 177)

corrplot(cor(diversity_cor),
    method = "number", order = "hclust")

dev.off()
system("open figureS5.tiff")

# correlation between environmental variables and site area

# figure S8

env_var <- data_forests1 %>%
    select(site_area_ha,
        ph, p, k, ca, mg, sb, al,
        map, cwd, mat, max_temp, elevation)

corr3 <- cor(env_var)

# save
tiff("figureS8.tiff", units = "cm", width = 23, height = 23, res = 177)

corrplot(corr3, method = "number", order = "hclust")

dev.off()
system("open figureS8.tiff")
