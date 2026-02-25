library(dplyr)
library(tidyr)


pipeline_modelling <- function(data_forests_raw, is_census = FALSE) {

    data_forests <- mutate_if(data_forests_raw, is.character, as.factor)

    if (is_census == FALSE) {
        data_forests <- data_forests %>%
            mutate(gains_perc =
                data_forests$c_gains / data_forests$c_stocks_i,
            losses_perc =
                data_forests$c_losses / data_forests$c_stocks_i,
            net_perc =
                data_forests$net_c_sink / data_forests$c_stocks_i)

        # compute the empirical weights
        weig <- data_forests$interval_length ^ (1 / 3) +
            data_forests$sampled_area_ha ^ (1 / 4) - 1

        climate_pca <- read.table("./outputs/clim_pca_scores_census_int.csv",
            header = TRUE, dec = ".", sep = ",") %>%
            dplyr::select(Dim.1, plot_code_interval) # nolint
        colnames(climate_pca)[1] <- "warm_dry_climate"
        data_forests <- data_forests %>% left_join(climate_pca,
            by = "plot_code_interval")

    } else {
        weig <- data_forests$sampled_area_ha ^ (1 / 3)

        climate_pca <- read.table("./outputs/clim_pca_scores_census.csv",
            header = TRUE, dec = ".", sep = ",") %>%
            dplyr::select(Dim.1, site_year) # nolint
        colnames(climate_pca)[1] <- "warm_dry_climate"
        data_forests <- data_forests %>% left_join(climate_pca,
            by = "site_year")

    }

    data_forests <- data_forests %>%
        mutate(site_area_ha_log = log(data_forests_raw$site_area_ha))

    data_forests$weig <- weig

    soil_pca <- read.table("./outputs/soil_pca_scores.csv",
        header = TRUE, dec = ".", sep = ",") %>%
        dplyr::select(Dim.1, Dim.2, Dim.3, plot_code) # nolint

    colnames(soil_pca)[1] <- "soil_fertily"
    colnames(soil_pca)[2] <- "aluminum_phosphorus"
    colnames(soil_pca)[3] <- "phosphorus_magnesium"

    data_forests <- data_forests %>% left_join(soil_pca, by = "plot_code")

    # candidate predictors
    predictors_raw <- data_forests %>% dplyr::select(year, site_area_ha_log,# nolint
        richness_hill, shannon_hill, simpson_hill, # nolint
        wd_sd, sla_sd, cwm_wd, cwm_sla, # nolint
        sespd, # nolint
        warm_dry_climate, elevation, # nolint
        site_area_ha_log, # nolint
        soil_fertily, aluminum_phosphorus, phosphorus_magnesium) # nolint

    predictors_scaled <- as.data.frame(scale(predictors_raw))
    for (i in seq_len(ncol(predictors_scaled))) {
        names(predictors_scaled)[i] <-
            paste(names(predictors_scaled[i]), "scaled", sep = "_")
    }

    # dataset for the models
    data_mod <- cbind.data.frame(data_forests, predictors_scaled)

    return(data_mod)
}