library(dplyr)
library(tidyr)
library(cowplot)
library(ggplot2)

#
data_for_representivity1 <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

head(data_for_representivity1)
summary(data_for_representivity1)
dim(data_for_representivity1)

### individual level
data_for_representivity1

data_for_representivity_ind <- data_for_representivity1#[!duplicated(data_for_representivity1$ind_key), ]

# Calculate the proportion of rows where wd_level and sla_level are both "species"
proportion_ind_wd <- sum(data_for_representivity_ind$wd_level == "species") / nrow(data_for_representivity_ind)
proportion_ind_wd

proportion_ind_sla <- sum(data_for_representivity_ind$sla_level == "species") / nrow(data_for_representivity_ind)
proportion_ind_sla

###############



data_for_representivity <- data_for_representivity1[data_for_representivity1$dbhq > 0,]
dim(data_for_representivity)

# sla
data_biomass_site_sla <- data_for_representivity %>% 
  group_by(site_year, sla_level) %>% 
  summarise(agb_plot = sum(agwb))

data_biomass_site_sla1 <- data_biomass_site_sla %>% 
  spread(key = sla_level, value = agb_plot) %>% data.frame()%>% 
  replace(is.na(.), 0)

data_biomass_site_sla1$total_agb <- rowSums(data_biomass_site_sla1 %>% 
                                              select(family, genus, overall, species), 
                                            na.rm=TRUE) 

# wd
data_biomass_site_wd <- data_for_representivity %>% 
  group_by(site_year, wd_level) %>% 
  summarise(agb_plot = sum(agwb))

data_biomass_site_wd1 <- data_biomass_site_wd %>% 
  spread(key = wd_level, value = agb_plot) %>% data.frame() %>% 
  replace(is.na(.), 0)

data_biomass_site_wd1$total_agb <- rowSums(data_biomass_site_wd1 %>%
                                             select(family, genus, overall, species), 
                                           na.rm=TRUE) 

####################

sla_data <- data_biomass_site_sla1 %>% 
  mutate(family_perc = family * 100 / total_agb,
         genus_perc = genus * 100 / total_agb,
         overall_perc = overall * 100 / total_agb,
         species_perc = species * 100 / total_agb) %>% select(contains("perc"))

wd_data <- data_biomass_site_wd1 %>%   
  mutate(family_perc = family * 100 / total_agb,
         genus_perc = genus * 100 / total_agb,
         overall_perc = overall * 100 / total_agb,
         species_perc = species * 100 / total_agb) %>% select(contains("perc"))


###############################

df = sla_data

plots <- lapply(names(df), function(col) {
  
  mean_col <- mean(df[[col]])
  sd_col <- sd(df[[col]])
  
  ggplot(df, aes_string(col)) +
    geom_histogram(bins=7) +
    geom_vline(xintercept = mean_col, color = "red") +
    ggtitle(paste0("Mean: ", round(mean_col, 2),"\nSD: ", round(sd_col, 2)))+
    xlab(gsub("_perc","",col))+
    ylab("")
})

plot = plot_grid(plotlist = plots, ncol = 3, align = "h", axis = "tb")

ggsave("sla_representativity.tiff", plot, width = 12, height = 8) 
system("open sla_representativity.tiff")

###############################

df = wd_data

plots <- lapply(names(df), function(col) {
  
  mean_col <- mean(df[[col]])
  sd_col <- sd(df[[col]])
  
  ggplot(df, aes_string(col)) +
    geom_histogram(bins=7) +
    geom_vline(xintercept = mean_col, color = "red") +
    ggtitle(paste0("Mean: ", round(mean_col, 2),"\nSD: ", round(sd_col, 2)))+
    xlab(gsub("_perc","",col))+
    ylab("")
})

plot = plot_grid(plotlist = plots, ncol = 3, align = "h", axis = "tb")

ggsave("wd_representativity.tiff", plot, width = 12, height = 8) 
system("open wd_representativity.tiff")









