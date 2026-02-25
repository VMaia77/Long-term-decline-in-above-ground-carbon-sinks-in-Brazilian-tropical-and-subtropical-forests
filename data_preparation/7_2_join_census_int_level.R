library(dplyr)
library(tidyr)

# get from here, so we dont need to aggreagate dataset by dataset etc

data_aggregated_structure <- read.table("census_level.csv",
  header = TRUE, dec = ".", sep = ",")

names(data_aggregated_structure)

# get lag values and then: middle points

data_structure_lags <- data_aggregated_structure %>%

  select(plot_code, site_year, year, census_data, richness_hill, shannon_hill, simpson_hill,
    wd_sd, sla_sd, cwm_wd, cwm_sla, richness_obs,
    pd.obs, pd.obs.z, mpd.obs, mpd.obs.z, mntd.obs, mntd.obs.z) %>%

  group_by(plot_code) %>%

  arrange(site_year, year) %>%

  mutate(lag.value.yr_round = lag(year), lag.value.census_data = lag(census_data),
    lag.value.richness_hill = lag(richness_hill),
    lag.value.shannon_hill = lag(shannon_hill),
    lag.value.simpson_hill = lag(simpson_hill),
    lag.value.wd_sd = lag(wd_sd),
    lag.value.sla_sd = lag(sla_sd),
    lag.value.cwm_wd = lag(cwm_wd),
    lag.value.cwm_sla = lag(cwm_sla),
    lag.value.richness_obs = lag(richness_obs),
    lag.value.pd.obs = lag(pd.obs),
    lag.value.pd.obs.z = lag(pd.obs.z),
    lag.value.mpd.obs = lag(mpd.obs),
    lag.value.mpd.obs.z = lag( mpd.obs.z),
    lag.value.mntd.obs = lag(mntd.obs),
    lag.value.mntd.obs.z = lag(mntd.obs.z)) %>%

  ungroup()

##################

data_structure_lags3 <-
  data_structure_lags[!is.na(data_structure_lags$lag.value.yr_round), ]

data_structure_lags4 <- data_structure_lags3 %>%
  mutate(plot_code_interval =
    paste(plot_code, lag.value.yr_round, year, sep = "_"))

data_structure_lags5 <- as.data.frame(data_structure_lags4 %>%
  relocate(plot_code_interval, .before = richness_hill))

data_structure_lags6 <-
  data_structure_lags5 %>%
    relocate(lag.value.yr_round, .before = year)

# View(data_structure_lags6)

# mid points

data_final2 <- data_structure_lags6 %>%
  mutate(interval_length = census_data - lag.value.census_data) %>%
  mutate(mid_year = (year + lag.value.yr_round) / 2,
  mid_census_data = (census_data + lag.value.census_data) / 2, 
    mid_richness_hill = (richness_hill + lag.value.richness_hill) / 2,
    mid_shannon_hill = (shannon_hill + lag.value.shannon_hill) / 2,
    mid_simpson_hill = (simpson_hill + lag.value.simpson_hill) / 2,
    mid_wd_sd = (wd_sd + lag.value.wd_sd) / 2,
    mid_sla_sd = (sla_sd + lag.value.sla_sd) / 2,
    mid_cwm_wd = (cwm_wd + lag.value.cwm_wd) / 2,
    mid_cwm_sla = (cwm_sla + lag.value.cwm_sla) / 2,
    mid_richness_obs = (richness_obs + lag.value.richness_obs) / 2,
    mid_pd.obs = (pd.obs + lag.value.pd.obs) / 2,
    mid_pd.obs.z = (pd.obs.z + lag.value.pd.obs.z) / 2,
    mid_mpd.obs = (mpd.obs + lag.value.mpd.obs) / 2,
    mid_mpd.obs.z = (mpd.obs.z + lag.value.mpd.obs.z) / 2,
    mid_mntd.obs = (mntd.obs + lag.value.mntd.obs) / 2,
    mid_mntd.obs.z = (mntd.obs.z + lag.value.mntd.obs.z) / 2)

# View(data_final2)

# add metadata
metadata <- read.table("./sites_attributes/metadata.csv",
    header <- TRUE, dec = ".", sep = ";")

data_final2 <- left_join(data_final2, metadata, by = "plot_code")

elevation <- read.csv("./outputs/elevation.csv",
  header = TRUE, dec = ".", sep = ",")

data_final2 <- left_join(data_final2, elevation, by = "plot_code")

climate <- read.csv("./outputs/climate.csv",
  header = TRUE, dec = ".", sep = ";")

data_final71 <- left_join(data_final2, climate, by = "plot_code_interval")

############### add dyn data

data_dynamics0 <- read.table("./outputs/dyn_raw.csv",
  header = TRUE, dec = ".", sep = ",")

data_final71$plot_code_interval == data_dynamics0$plot_code_interval

data_final71$lag.value.yr_round == data_dynamics0$year_i

data_final71$year == data_dynamics0$year_f

data_final73 <- left_join(data_dynamics0 %>%
    select(-c(year_i, year_f, site_year)), data_final71,
      by = "plot_code_interval")

# View(data_final73)

###########

agwb_to_carbon <- 0.456

# head(data_final73)

data_final73 <- data_final73 %>%
  mutate(
        c_stocks_i = (lag.value_agwb / sampled_area_ha) * agwb_to_carbon,

        c_stocks_f = (agwb / sampled_area_ha) * agwb_to_carbon,

        c_gains =
          (((gain_surv + gain_rec) / sampled_area_ha) / interval_length) *
            agwb_to_carbon,

        c_losses =
          (((losses_surv + losses_dead) / sampled_area_ha) / interval_length) *
            agwb_to_carbon,

        net_c_sink = ((net_prod / sampled_area_ha) / interval_length) *
          agwb_to_carbon)

data_final73

# Talbot et al 2014 correction
# prod_r + 0.0091 * prod_r * ilength

beta_param <- 0.0091

data_final73f <- data_final73 %>%
  mutate(c_gains = c_gains + beta_param * c_gains * interval_length,

    c_losses = c_losses + beta_param * c_losses * interval_length,

    net_c_sink = net_c_sink + beta_param * net_c_sink * interval_length) %>%

    mutate(crt = c_stocks_f / c_gains)

head(data_final73f)
names(data_final73f)
###########

data_aggregated_final <- data_final73f %>%

  select(plot_code, site_year,
    lat, long, n_plots, plot_size_m2,
    sampled_area_ha, n_census, site_area_ha, forest_type,
    year_i = lag.value.yr_round, year_f = year, year = mid_year,
    census_data_i = lag.value.census_data, census_data_f = census_data, census_data = mid_census_data,
    interval_length, plot_code_interval,
    c_stocks_i, c_stocks_f,
    c_gains, c_losses, net_c_sink, crt,
    richness_hill = mid_richness_hill,
    shannon_hill = mid_shannon_hill, simpson_hill = mid_simpson_hill,
    wd_sd = mid_wd_sd,
    sla_sd = mid_sla_sd,
    cwm_wd = mid_cwm_wd, cwm_sla = mid_cwm_sla,
    richness_obs = mid_richness_obs,
    pd.obs = mid_pd.obs,
    pd.obs.z = mid_pd.obs.z,
    mpd.obs = mid_mpd.obs,
    mpd.obs.z = mid_mpd.obs.z,
    mntd.obs = mid_mntd.obs,
    mntd.obs.z = mid_mntd.obs.z,
    ph, p, k, ca, mg, al, sb,
    map, cwd, mat, max_temp, elevation)

write.csv(data_aggregated_final, "census_int_level.csv", row.names = FALSE)
