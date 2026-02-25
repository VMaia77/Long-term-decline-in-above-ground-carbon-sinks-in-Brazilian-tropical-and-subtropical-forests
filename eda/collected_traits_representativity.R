library(dplyr)
library(tidyr)

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

vegetation_data %>% names()

########################

data_wd_collected <- vegetation_data[vegetation_data$wd_is_collected == 1, ]

data_sla_collected <- vegetation_data[vegetation_data$sla_is_collected == 1, ]

######## species %

n_species_total <- length(vegetation_data$species %>% unique())

# wd

(length(data_wd_collected$species %>% unique()) / n_species_total) * 100

# sla

(length(data_sla_collected$species %>% unique()) / n_species_total) * 100

############### biomass %

basal_area_total <- sum(vegetation_data$basal_area)

# wd

(sum(data_wd_collected$basal_area) / basal_area_total) * 100

# sla

(sum(data_sla_collected$basal_area) / basal_area_total) * 100
