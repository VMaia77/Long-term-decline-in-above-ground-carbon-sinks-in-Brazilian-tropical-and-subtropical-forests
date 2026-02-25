
library(dplyr)
library(tidyr)


# read census interval level data
data_forests1 <-
    read.table("census_level.csv", header = TRUE, dec = ".", sep = ",")
head(data_forests1)

attach(data_forests1)

hist(lat)
hist(long)
hist(site_area_ha)
hist(ph)
hist(p)
hist(k)
hist(ca)
hist(mg)
hist(al)
hist(sb)
hist(elevation)
hist(map)
hist(site_area_ha)
hist(cwd)
hist(mat)
hist(max_temp)
hist(richness_hill)
hist(shannon_hill)
hist(simpson_hill)
hist(wd_sd)
hist(sla_sd)
hist(cwm_wd)
hist(cwm_sla)
hist(sespd)
hist(sesmpd)
hist(sesmntd)
