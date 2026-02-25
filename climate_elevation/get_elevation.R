library(sp)
library(sf)#vectors
library(raster)#grid
library(dplyr)

data_base <- read.table("data_base.csv", header = TRUE, dec = ".", sep = ";")
head(data_base)

data_base <- data_base %>%
    group_by(plot_code) %>%
    summarise(lat = mean(lat, na.rm = TRUE), long = mean(long, na.rm=TRUE))

coord <- subset(data_base, select = c("long", "lat"))
names(coord) <- c("x", "y")

#elevation
relevation <- raster("wc2.1_30s_elev.tif")

elevation <- extract(relevation, coord, method = "simple")
elevation <- cbind.data.frame(plot_code = data_base$plot_code, elevation)

write.csv(elevation, "elevation.csv")
