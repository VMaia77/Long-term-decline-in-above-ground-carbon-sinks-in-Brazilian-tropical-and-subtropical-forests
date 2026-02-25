library(dplyr)
library(tidyr)
library(tidyverse)


# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

species_basal_area <- vegetation_data %>% group_by(species) %>% summarise(basal_area = sum(basal_area, na.rm=TRUE)) # nolint

species_basal_area$species = gsub(" ", "_", species_basal_area$species)

basal_area_total <- sum(species_basal_area$basal_area)

###############

spp <- read.table("./outputs/phylo_raw_output_tpl.csv",
    header = TRUE, dec = ".", sep = ",")

spp_tpl <- spp %>%
    select(Taxon, Family, New.Genus, New.Species) %>%
    mutate(New.Taxon = str_c(New.Genus, "_", New.Species))

spp_tpl$species <- spp_tpl$Taxon
spp_tpl$Taxon <- gsub(" ", "_", spp_tpl$Taxon)

# spp_tpl %>% head()

# sum(spp_tpl$Taxon %in% gsub(" ", "_", vegetation_data$species))

species_basal_area <- left_join(species_basal_area, spp_tpl %>% select(Taxon, New.Taxon), by = c("species"="Taxon"))

# species_basal_area %>% filter(species != New.Taxon)

missingtips <- read.table("./outputs/phylo_missing_tips.csv",
    header = TRUE, dec = ".", sep = ",")

#### %

not_in_tree <- species_basal_area %>% filter(New.Taxon %in% missingtips$x)

# species

n_species_total <- length(species_basal_area$New.Taxon %>% unique())

n_species_not_in_tree <- length(not_in_tree$New.Taxon %>% unique())

n_species_not_in_tree / n_species_total

species_in_tree <- 1 - n_species_not_in_tree / n_species_total

species_in_tree

# basal area

not_in_tree_basal_area <- sum(not_in_tree$basal_area)

not_in_tree_basal_area / basal_area_total

in_tree_basal_area <- 1 - not_in_tree_basal_area / basal_area_total

in_tree_basal_area
