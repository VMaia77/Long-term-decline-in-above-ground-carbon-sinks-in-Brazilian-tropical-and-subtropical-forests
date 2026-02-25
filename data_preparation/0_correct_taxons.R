library(flora)
library(dplyr)
library(tidyr)

########## correct species names

# vegetation
vegetation_data <- read.table("./_vegetation_raw_data/vegetation_raw_data.csv",
    header = TRUE, dec = ".", sep = ";")

# gwdb
gwddb <- read.table("./_traits_raw_data/GlobalWoodDensityDatabase.csv",
    header = TRUE, dec = ".", sep = ";")

# wd data
wd_data <- read.table("./_traits_raw_data/traits_wd.csv",
    header = TRUE, dec = ".", sep = ";")

# sla data
sla_data <- read.table("./_traits_raw_data/traits_sla.csv",
    header = TRUE, dec = ".", sep = ";")

###### get taxa ######
result_vegetation_data <- get.taxa(unique(vegetation_data$species))

result_vegetation_data[result_vegetation_data$search.str !=
    result_vegetation_data$original.search, ]
# none has changed

result_gwddb_data <- get.taxa(unique(gwddb$species))

result_wd_data <- get.taxa(unique(wd_data$species))

result_sla_data <- get.taxa(unique(sla_data$species))

###### joins ######
gwddb_coor <- left_join(gwddb,
    result_gwddb_data %>%
    select(family, search.str, original.search),
        by = c("species" = "original.search")) %>%
        rename(species_corr = search.str) %>%
        mutate(species_corr = coalesce(species_corr, species))

wd_data_coor <- left_join(wd_data,
    result_wd_data %>%
    select(family, search.str, original.search),
        by = c("species" = "original.search")) %>%
        rename(species_corr = search.str) %>%
        mutate(species_corr = coalesce(species_corr, species))

sla_data_coor <- left_join(sla_data,
    result_sla_data %>%
    select(family, search.str, original.search),
        by = c("species" = "original.search")) %>%
        rename(species_corr = search.str) %>%
        mutate(species_corr = coalesce(species_corr, species))

##################################################################

write.csv(gwddb_coor, "./outputs/GlobalWoodDensityDatabase_corr_spp.csv")
write.csv(wd_data_coor, "./outputs/wd_data_corr_spp.csv")
write.csv(sla_data_coor, "./outputs/sla_data_corr_spp.csv")
