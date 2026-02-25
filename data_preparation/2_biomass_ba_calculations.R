library(dplyr)
library(tidyr)
library(BIOMASS)

basal_area_calc = function(d) {
  ba = (pi * (d^2)) / 40000
  return(ba)
}

get_agb_using_h <- function(d, wd) {
  height_hat <- retrieveH(D = d, region = "SAmerica")$H
  agb_hat <- computeAGB(D = d, H = height_hat, WD = wd)
  return(agb_hat)
}

get_agb <- function(d, wd, long, lat) {
  agb_hat <- computeAGB(D = d, WD = wd, coord = cbind(long, lat))
  return(agb_hat)
}

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

###
vegetation_data <- vegetation_data %>%
    mutate(agwb_h = get_agb_using_h(dbhq, wd)) %>%
    mutate(agwb = get_agb(dbhq, wd, long, lat))

mod1 <- lm(agwb_h ~ lat + long, data = vegetation_data)
summary(mod1)

mod2 <- lm(agwb ~ lat + long, data = vegetation_data)
summary(mod2)

cor(vegetation_data$agwb_h, vegetation_data$agwb)

mean(vegetation_data$agwb_h)
mean(vegetation_data$agwb)

# add basal area
vegetation_data$basal_area <- basal_area_calc(vegetation_data$dbhq)

###
write.csv(vegetation_data, "vegetation_data.csv", row.names = FALSE)
