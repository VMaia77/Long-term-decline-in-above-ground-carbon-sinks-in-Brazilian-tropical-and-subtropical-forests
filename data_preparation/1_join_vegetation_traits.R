library(dplyr)
library(tidyr)

# vegetation
vegetation_data <- read.table("./_vegetation_raw_data/vegetation_raw_data.csv",
    header = TRUE, dec = ".", sep = ";")

# gwdb
gwddb <- read.table("./outputs/GlobalWoodDensityDatabase_corr_spp.csv",
    header = TRUE, dec = ".", sep = ",")

# wd data
wd_data <- read.table("./outputs/wd_data_corr_spp.csv",
    header = TRUE, dec = ".", sep = ",")

# sla data
sla_data <- read.table("./outputs/sla_data_corr_spp.csv",
    header = TRUE, dec = ".", sep = ",")


###### assign collected data WD
names(wd_data)
unique(wd_data$referencia)

wd_data_collected <- wd_data %>%
    mutate(is_collected = ifelse(referencia == "Pedro", 1, 0)) %>%
    filter(is_collected == 1)

vegetation_data <- vegetation_data %>%
    mutate(wd_is_collected =
        ifelse(species %in% wd_data_collected$species_corr, 1, 0))

###### assign collected data WD
names(sla_data)
unique(sla_data$referencia)

sla_data_collected <- sla_data %>%
    mutate(is_collected = ifelse(referencia == "Pedro" |
    referencia == "Nathalle" | referencia == "Diego e Jamir" |
    referencia == "Fernanda" , 1, 0)) %>%
    filter(is_collected == 1)

vegetation_data <- vegetation_data %>%
    mutate(sla_is_collected =
        ifelse(species %in% sla_data_collected$species_corr, 1, 0))

#############################################################################

gwddb <- gwddb %>% mutate(specis_split = species_corr)
gwddb <- separate(gwddb, specis_split, c("genus", "epipeto"), " ",
    fill = "left")

wd_data <- wd_data %>% mutate(specis_split = species_corr)
wd_data <- separate(wd_data, specis_split, c("genus", "epipeto"), " ",
    fill = "left")

sla_data <- sla_data %>% mutate(specis_split = species_corr)
sla_data <- separate(sla_data, specis_split, c("genus", "epipeto"), " ",
    fill = "left")

##### bind wd data

gwddb_temp <- gwddb %>% select(WD, family, genus, species_corr)
wd_data_temp <- wd_data %>% select(WD, family, genus, species_corr)

wd_df <- rbind.data.frame(gwddb_temp, wd_data_temp)

#
sla_df <- sla_data %>% select(sla, family, genus, species_corr)

########## aggregate by species, genus, family ##########

# wd
wd_spp_mean <- wd_df %>%
    group_by(species_corr) %>%
    summarise(wd_spp = mean(WD, na.rm = TRUE)) %>%
    filter(!is.na(species_corr)) %>%
    rename(species = species_corr)

wd_genus_mean <- wd_df %>%
    group_by(genus) %>%
    summarise(wd_genus = mean(WD, na.rm = TRUE)) %>%
    filter(!is.na(genus))

wd_family_mean <- wd_df %>%
    group_by(family) %>%
    summarise(wd_family = mean(WD, na.rm = TRUE)) %>%
    filter(!is.na(family))

# sla
sla_spp_mean <- sla_df %>%
    group_by(species_corr) %>%
    summarise(sla_spp = mean(sla, na.rm = TRUE)) %>%
    filter(!is.na(species_corr)) %>%
    rename(species = species_corr)

sla_genus_mean <- sla_df %>%
    group_by(genus) %>%
    summarise(sla_genus = mean(sla, na.rm = TRUE)) %>%
    filter(!is.na(genus))

sla_family_mean <- sla_df %>%
    group_by(family) %>%
    summarise(sla_family = mean(sla, na.rm = TRUE)) %>%
    filter(!is.na(family))

##################### joins #####################

# wd
vegetation_data <- left_join(vegetation_data,
    wd_spp_mean, by = "species")

vegetation_data <- left_join(vegetation_data,
    wd_genus_mean, by = "genus")

vegetation_data <- left_join(vegetation_data,
    wd_family_mean, by = "family")

# sla
vegetation_data <- left_join(vegetation_data,
    sla_spp_mean, by = "species")

vegetation_data <- left_join(vegetation_data,
    sla_genus_mean, by = "genus")

vegetation_data <- left_join(vegetation_data,
    sla_family_mean, by = "family")


############ assign values levels ############

# wd
vegetation_data <- vegetation_data %>%
    mutate(wd_level = ifelse(!is.na(wd_spp), "species",
    ifelse(!is.na(wd_genus), "genus",
    ifelse(!is.na(wd_family), "family", "overall"))),
    wd = ifelse(!is.na(wd_spp), wd_spp,
    ifelse(!is.na(wd_genus), wd_genus,
    ifelse(!is.na(wd_family), wd_family, NA))))

# sla
vegetation_data <- vegetation_data %>%
    mutate(sla_level = ifelse(!is.na(sla_spp), "species",
    ifelse(!is.na(sla_genus), "genus",
    ifelse(!is.na(sla_family), "family", "overall"))),
    sla = ifelse(!is.na(sla_spp), sla_spp,
    ifelse(!is.na(sla_genus), sla_genus,
    ifelse(!is.na(sla_family), sla_family, NA))))


# save
vegetation_data <- vegetation_data %>%
    select(-c(wd_spp, wd_genus, wd_family, sla_spp, sla_genus, sla_family))

# replace wd and sla values
vegetation_data$wd[which(is.na(vegetation_data$wd))] <-
    mean(vegetation_data$wd, na.rm = TRUE)

vegetation_data$sla[which(is.na(vegetation_data$sla))] <-
    mean(vegetation_data$sla, na.rm = TRUE)

write.csv(vegetation_data, "vegetation_data.csv", row.names = FALSE)
