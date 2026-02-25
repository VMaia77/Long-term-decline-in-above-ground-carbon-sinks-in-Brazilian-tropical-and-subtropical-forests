# load packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(gridExtra)

# read data at census interval level
data_dyn <-
  read.table("census_int_level.csv", header = TRUE, dec = ".", sep = ",")

data_dyn <- mutate_if(data_dyn,
  is.character, as.factor) # transform character to factor

head(data_dyn)

data_sites <- unique(data_dyn %>%
  group_by(plot_code) %>%
  summarise(lat = mean(lat), long = mean(long),
    forest_type = forest_type, sampled_area_ha = mean(sampled_area_ha),
    n_census = mean(n_census),
    year_i = min(year_i), year_f = max(year_f),
    interval_length = mean(interval_length),
    site_area_ha = mean(site_area_ha),
    mat = mean(mat), map = mean(map), cwd = mean(cwd)))

# write.csv(data_sites, "supplementary file 1.csv", row.names = FALSE) # change col names mannualy

# total monitoring time
data_sites$year_range <- data_sites$year_f - data_sites$year_i

# monitoring time histogram
hist(data_sites$year_range)

# monitoring time mean, std, min and max
mean(data_sites$year_range)
sd(data_sites$year_range)
min(data_sites$year_range)
max(data_sites$year_range)

# sampled area mean, std, min and max
mean(data_sites$sampled_area_ha)
sd(data_sites$sampled_area_ha)
min(data_sites$sampled_area_ha)
max(data_sites$sampled_area_ha)

### plots

# color palette
colors_ <- c("purple3", "seagreen4", "darkorange2")
colfunc <- colorRampPalette(colors_)
colours_palletes <- colfunc(5)

# figure S1 A
# cwd ~ mat, colored by latitude
p4 <- ggplot(data_dyn,
    aes(x = mat, y = map, col = lat,
      size = (sampled_area_ha * interval_length))) +

  geom_point() +

  geom_jitter() +

  theme_classic() +

  labs(x = "MAT (ºC)",
       y = "MAP (mm)",
       colour = "Latitude",
       size = expression(Sampling ~ effort[(ha %*% yr)]), tag = "A") +

  theme(text = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black")) +

  scale_color_gradientn(colours = colours_palletes,
    limits = c(min(data_dyn$lat), max(data_dyn$lat)),
    breaks = c(min(data_dyn$lat) + 0.5, -21.5, max(data_dyn$lat) - 0.5),
    labels = c("-28 (South)", "-21.5", "-15 (North)")) +
    theme(panel.border =
      element_rect(colour = "black", fill = NA, size = 0.7)) +

  scale_size_continuous(breaks = c(5, 15.1, 25.2),
    labels = c("Min", " ", "Max"), limits = c(0, 30))


# figure S1 B
# histogram of the sampled area
plot_sampled_area <- ggplot(data_sites, aes(x = sampled_area_ha)) +
  geom_histogram(binwidth = 0.5, fill = "gray") +
  geom_vline(
    aes(xintercept = mean(sampled_area_ha)),
      color = "blue", linetype = "dashed", size=1) +
  theme_bw() + labs(x = "Spatial sampling effort (ha)", y = "Number of sites") +
  theme(text =
    element_text(size = 15, colour = "black")) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black")) +
  labs(tag = "B")

plot_sampled_area

# figure S1 C
# histogram of the monitoring time
data_sites$year_range <-  data_sites$year_f - data_sites$year_i

plot_time_range <- ggplot(data_sites, aes(x = year_range)) +
  geom_histogram(binwidth = 5, fill = "gray") +
  geom_vline(aes(xintercept = mean(year_range)),
    color = "blue", linetype = "dashed", size = 1) +
  theme_bw() + labs(
    x = "Temporal sampling effort (years)",
    y = "Number of sites") +
  theme(text = element_text(size = 15, colour = "black")) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black")) +
  labs(tag = "C")

plot_time_range

# put the plots together - Figure S1
allplotslist <- align_plots(plot_sampled_area, plot_time_range, align = "hv")

grid2 <- grid.arrange(allplotslist[[1]], allplotslist[[2]], nrow = 2)

grid <- grid.arrange(p4, grid2, ncol = 2, widths = c(1, 0.5))

# save
ggsave("figureS1.tiff", grid, he = 15, wi = 30, un = "cm", dpi = 600)
system("open figureS1.tiff")
