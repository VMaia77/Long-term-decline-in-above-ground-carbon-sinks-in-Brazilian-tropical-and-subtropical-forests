library(dplyr)
library(tidyr)
library(Hmisc)

source("./utils/boot_raref.R")

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

vegetation_data <- vegetation_data[vegetation_data$dbhq > 0, ]

censuses <- unique(vegetation_data$site_year)

result_df <- cbind.data.frame()

n_sim <- 100
for (census in censuses) {
    df <- vegetation_data %>% filter(site_year == census)
    results_temp <- cbind.data.frame(wd_sd = NULL, sla_sd = NULL)
    for (i in seq_len(n_sim)) {
        sampled_df <- individual_sampling(df, minimum_n = minimum_n)

        wd_sd_i <- sqrt(wtd.var(sampled_df$wd,
            sampled_df$basal_area * 100, na.rm = TRUE))

        sla_sd_i <- sqrt(wtd.var(sampled_df$sla,
            sampled_df$basal_area * 100, na.rm = TRUE))

        results_temp[i, "wd_sd"] <- wd_sd_i
        results_temp[i, "sla_sd"] <- sla_sd_i
    }

    result_df_temp <- cbind.data.frame(site_year = census,
        wd_sd = mean(results_temp$wd_sd),
        sla_sd = mean(results_temp$sla_sd))

    result_df <- rbind(result_df, result_df_temp)
}

head(result_df)

write.csv(result_df,
    "./outputs/functional_diversity_site_level_bootraref.csv",
    row.names = FALSE)