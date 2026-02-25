library(dplyr)
library(tidyr)

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

vegetation_data <- vegetation_data[vegetation_data$dbhq > 0, ]

data_cwm <- vegetation_data %>%
    group_by(site_year) %>%
    summarise(cwm_wd = weighted.mean(wd, basal_area, na.rm = TRUE),
        cwm_sla = weighted.mean(sla, basal_area, na.rm = TRUE))

write.csv(data_cwm,
    "./outputs/functional_cwm.csv",
    row.names = FALSE)
