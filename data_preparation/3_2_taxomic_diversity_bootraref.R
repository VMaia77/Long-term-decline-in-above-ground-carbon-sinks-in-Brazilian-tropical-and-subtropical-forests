library(dplyr)
library(tidyr)

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
    results_temp <- cbind.data.frame(diversity_metric = NULL)
    for (i in seq_len(n_sim)) {
        sampled_df <- individual_sampling(df, minimum_n = minimum_n)

        diversity_metric_i <- length(unique(sampled_df$species))

        results_temp[i, "diversity_metric"] <- diversity_metric_i
    }

    result_df_temp <- cbind.data.frame(site_year = census,
        diversity_metric = mean(results_temp$diversity_metric))

    result_df <- rbind(result_df, result_df_temp)
}

inext_div <- read.table("./outputs/taxonomic_diversity_site_level.csv",
    header = TRUE, dec = ".", sep = ",") %>% arrange(site)

result_df <- result_df %>% arrange(site_year)

cor(result_df$diversity_metric, inext_div$richness)

result_df <- result_df %>% rename(richness = diversity_metric)

write.csv(result_df,
    "./outputs/taxonomic_diversity_site_level_bootraref.csv", row.names = FALSE)