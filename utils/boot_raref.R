library(dplyr)
library(tidyr)

# # vegetation
# data_rarefaction <- read.table("vegetation_data.csv",
#     header = TRUE, dec = ".", sep = ",")

# data_rarefaction <- data_rarefaction[data_rarefaction$dbhq > 0, ]

# unique_ind <- data_rarefaction %>%
#     select(site_year, ind_year_key)

# n_ind_by_census <- unique_ind %>% group_by(site_year) %>% summarise(n = n())

# minimum_n <- min(n_ind_by_census$n)
minimum_n <- 100 # easy the interpretability

individual_sampling <- function(df, minimum_n = minimum_n) {

    sampled_ind <- sample(df$ind_year_key, size = minimum_n, replace = FALSE)

    sampled_df <- df %>% filter(df$ind_year_key %in% sampled_ind)

    return(sampled_df)
}