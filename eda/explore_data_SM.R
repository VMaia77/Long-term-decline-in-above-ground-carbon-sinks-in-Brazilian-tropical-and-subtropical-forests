rm(list = ls()) # cleaning the environment

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(ncf)
library(caret)
library(DHARMa)
library(performance)
library(jtools)
library(interactions)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(ggsci)

# read raw data
data_forests1 <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")
head(data_forests1)

# exploring...
levels(as.factor(data_forests1$plot_code))

length(levels(as.factor(data_forests1$plot_code)))
dim(data_forests1)

data_forests2 <-
  mutate_if(data_forests1,
    is.character, as.factor) # transform str to factors

data_forests3 <- data_forests2
length(levels(as.factor(data_forests3$plot_code)))
dim(data_forests3)

head(data_forests3)

# filter dbh
data_forests4 <- data_forests3[data_forests3$dbhq > 0, ]
dim(data_forests4)

############ plots ############

# figure S3
plot_dbh_class_site_dens <- ggplot(data = data_forests4,
  aes(x = dbhq, fill = plot_code)) +
  geom_density() + facet_wrap(~plot_code) +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Density") +
  theme(legend.position="none")

plot_dbh_class_site_dens

ggsave("figureS3.tiff", he = 23, wi = 37, un = "cm", dpi = 177)
system("open figureS3.tiff")

# create dbh classes
data_forests5 <- data_forests4 %>% 
  mutate(dbh_class = ifelse(dbhq >= 5 & dbhq < 10, "1) 5-10",
                          ifelse(dbhq >= 10 & dbhq < 20, "2) 10-20",
                                 ifelse(dbhq >= 20 & dbhq < 40, "3) 20-40",
                                        ifelse(dbhq >= 40, "4) > 40",
                                          "error")))))

summary(as.factor(data_forests5$dbh_class))

# get tables of the summarised data

dbh_class <- data_forests5 %>%
  group_by(forest_type, plot_code) %>%
  count(dbh_class)

sum_per_site <- dbh_class %>%
  group_by(plot_code) %>%
  summarise(n_total_site = sum(n))

dbh_class2 <- left_join(dbh_class, sum_per_site, by = "plot_code")

sum_per_forest_type <- dbh_class %>%
  group_by(forest_type) %>%
  summarise(n_total_forest_type = sum(n))

dbh_class1 <- left_join(dbh_class2, sum_per_forest_type, by = "forest_type")
dbh_class1

# site proportional values by dbh class
# figure S2
plot_dbh_class_site_perc <- ggplot(data = dbh_class1,
  aes(x=dbh_class, y= (n / n_total_site) * 100)) +
  geom_bar(stat="identity") + facet_wrap(~plot_code) +
  theme_bw() + labs(x = "DBH class (cm)", y = "Percentage of trees") 

plot_dbh_class_site_perc

# save
ggsave("figureS2.tiff", he = 23, wi = 37, un = "cm", dpi = 177)
system("open figureS2.tiff")