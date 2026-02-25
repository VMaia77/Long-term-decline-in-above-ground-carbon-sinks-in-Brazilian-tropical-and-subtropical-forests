library(dplyr)
library(tidyr)

ba_matrix <- function(df) {

    matrix_ <- df %>%
        select(site_year, species, basal_area) %>% # nolint
        group_by(site_year, species) %>%
        summarise(basal_area = sum(basal_area)) %>%
        spread(species, basal_area) %>%
        as.data.frame()
    matrix_[is.na(matrix_)] <- 0
    return(matrix_)

}