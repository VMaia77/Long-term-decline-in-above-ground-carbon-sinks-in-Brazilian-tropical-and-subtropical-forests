library(dplyr)
library(tidyr)

vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

data_str_biomass <- vegetation_data %>%
  select(site_year, agwb, plot_size_m2, plot_key)

data_str_biomass1 <- data_str_biomass %>%
  group_by(site_year) %>%
  summarise(n_plots = length(unique(plot_key)),
    plot_size = mean(plot_size_m2)) %>%
  mutate(sampled_area_ha = (n_plots * plot_size) / 10000)

agwb_to_carbon <- 0.456

data_str_biomass2 <- vegetation_data %>%
  group_by(site_year) %>%
  summarise(c_stocks = sum(agwb) * agwb_to_carbon)

data_biomass3 <- cbind.data.frame(data_str_biomass1, data_str_biomass2[, -1])

data_biomass3$c_stocks_ha <- data_biomass3$c_stocks /
  data_biomass3$sampled_area_ha

head(data_biomass3)

data_biomass4 <- data_biomass3 %>% select(site_year, c_stocks_ha)

data_biomass4 <- left_join(data_biomass4, vegetation_data %>%
  select(plot_code, site_year) %>%
    unique(), by = "site_year") %>%
    select(plot_code, site_year, c_stocks_ha)

data_biomass4 <- left_join(data_biomass4, vegetation_data %>%
  select(site_year, yr_round, census_data) %>%
  unique(), by = "site_year") %>%
  rename(year = yr_round)

head(data_biomass4)

################

metadata <- read.table("./sites_attributes/metadata.csv",
    header <- TRUE, dec = ".", sep = ";")

elevation <- read.csv("./outputs/elevation.csv",
  header = TRUE, dec = ".", sep = ",")

climate <- read.csv("./outputs/climate_str.csv",
  header = TRUE, dec = ".", sep = ";")

taxonomicdiv <- read.csv("./outputs/taxonomic_diversity_site_level.csv",
  header = TRUE, dec = ".", sep = ",") %>%
  select(site, richness, shannon, simpson)

names(taxonomicdiv) <-
  c("site_year", "richness_hill", "shannon_hill", "simpson_hill")

functionaldiv <- read.csv(
    "./outputs/functional_diversity_site_level_bootraref.csv",
    header = TRUE, dec = ".", sep = ",")

functionalcwm <- read.csv(
    "./outputs/functional_cwm.csv",
    header = TRUE, dec = ".", sep = ",")

phylo_sespd <- read.csv("./outputs/phylo_sespd.csv",
  header = TRUE, dec = ".", sep = " ")
phylo_sesmpd <- read.csv("./outputs/phylo_sesmpd.csv",
  header = TRUE, dec = ".", sep = " ")
phylo_sesmntd <- read.csv("./outputs/phylo_sesmntd.csv",
  header = TRUE, dec = ".", sep = " ")

phylo <- cbind.data.frame(phylo_sespd %>%
  select(ntaxa, pd.obs, pd.obs.z), phylo_sesmpd %>%
  select(mpd.obs, mpd.obs.z), phylo_sesmntd %>%
  select(mntd.obs, mntd.obs.z)) %>%
  rename(richness_obs = ntaxa)

phylo <- cbind(site_year = rownames(phylo), phylo)
rownames(phylo) <- NULL

########
data_biomass <- left_join(data_biomass4, metadata, by = "plot_code")
data_biomass <- left_join(data_biomass, elevation, by = "plot_code")
data_biomass <- left_join(data_biomass, climate, by = "site_year")
data_biomass <- left_join(data_biomass, taxonomicdiv, by = "site_year")
data_biomass <- left_join(data_biomass, functionaldiv, by = "site_year")
data_biomass <- left_join(data_biomass, functionalcwm, by = "site_year")
data_biomass <- left_join(data_biomass, phylo, by = "site_year")

dim(data_biomass)

write.csv(data_biomass, "census_level.csv", row.names = FALSE)
