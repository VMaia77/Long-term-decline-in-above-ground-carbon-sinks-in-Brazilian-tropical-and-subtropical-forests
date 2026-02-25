library(iNEXT)
library(dplyr)
library(tidyr)

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

data_rarefaction <- vegetation_data[vegetation_data$dbhq > 0, ]

# site level
matrix_rarefaction <- as.data.frame(data_rarefaction %>%
                                    select(site_year, species) %>%
                                    group_by(site_year, species) %>%
                                    summarise(n = n()) %>%
                                    spread(species, n))

site_year <- matrix_rarefaction[, 1]
matrix_rarefaction <- matrix_rarefaction[, -1]

matrix_rarefaction_list <- list()

for (i in seq_len(nrow(matrix_rarefaction))) {
    spp_comm <- unname(unlist(matrix_rarefaction[i, ]))
    matrix_rarefaction_list[[i]] <- spp_comm[!is.na(spp_comm)]
}

length(matrix_rarefaction_list)
length(matrix_rarefaction_list[[1]])

names(matrix_rarefaction_list) <- site_year

########################

rarefaction_hill <- estimateD(matrix_rarefaction_list,
    datatype = "abundance", base = "size", level = NULL, conf = NULL)

rarefaction_cov <- estimateD(matrix_rarefaction_list,
    datatype = "abundance", base = "coverage", level = 0.90, conf = NULL)

cor(rarefaction_cov[, "q = 0"], rarefaction_hill[, "q = 0"])

rarefaction_hill_all <- rarefaction_cov %>%
    rename(richness = "q = 0", shannon = "q = 1", simpson = "q = 2")

write.csv(rarefaction_hill_all,
    "./outputs/taxonomic_diversity_site_level.csv", row.names = FALSE)