library(dplyr)
library(tidyr)


data_biomass <- read.table("census_level.csv",
  header = TRUE, dec = ".", sep = ",")

data_biomass <- data_biomass %>%
  rename(pd = pd.obs,  sespd = pd.obs.z,
    mpd = mpd.obs, sesmpd = mpd.obs.z,
    mntd = mntd.obs, sesmntd = mntd.obs.z)

write.csv(data_biomass, "census_level.csv", row.names = FALSE)

###

data_dynamics <- read.table("census_int_level.csv",
  header = TRUE, dec = ".", sep = ",")

data_dynamics <- data_dynamics %>%
  rename(pd = pd.obs,  sespd = pd.obs.z,
    mpd = mpd.obs, sesmpd = mpd.obs.z,
    mntd = mntd.obs, sesmntd = mntd.obs.z)

write.csv(data_dynamics, "census_int_level.csv", row.names = FALSE)
