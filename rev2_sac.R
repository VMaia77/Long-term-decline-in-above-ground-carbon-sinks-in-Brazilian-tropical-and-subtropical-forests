library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)



data_forests_census_int <- read.table("census_int_level.csv",
                                  header = TRUE, dec = ".", sep = ",")


# Step 1: Aggregate long and lat by plot_code
plot_level_data <- data_forests_census_int %>%
  group_by(plot_code) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  )

# Step 2: Perform k-means clustering with 4 clusters
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(plot_level_data[, c("long", "lat")], centers = 4, nstart = 25)

# Step 3: Add cluster assignments to the aggregated data
plot_level_data$cluster <- kmeans_result$cluster

# Step 4: Merge clusters back into original data using plot_code
data_forests_census_int <- data_forests_census_int %>%
  left_join(plot_level_data %>% select(plot_code, cluster), by = "plot_code")

# Step 5: Convert cluster to factor (optional, for visualization)
data_forests_census_int$cluster <- as.factor(data_forests_census_int$cluster)

# Plot the clusters
ggplot(data_forests_census_int, aes(x = long, y = lat, color = as.factor(cluster))) +
  geom_point(size = 3, alpha = 0.8) +  # Adjust size and transparency
  scale_color_manual(values = c("red", "blue", "green", "purple")) +  # Custom cluster colors
  labs(x = "Longitude", y = "Latitude", color = "Cluster") +
  theme_minimal() +
  ggtitle("K-Means Clustering of Plots (k = 4)")


# Select relevant columns
rf_data <- data_forests_census_int %>%
  select(cluster, map, mat) %>%
  na.omit()  # Remove any rows with missing values

set.seed(123)  # For reproducibility

# # Split data into training (70%) and testing (30%)
train_index <- createDataPartition(rf_data$cluster, p = 0.8, list = FALSE)
train_data <- rf_data[train_index, ]
test_data <- rf_data[-train_index, ]

# Train the Random Forest model
rf_model <- randomForest(cluster ~ map + mat, data = train_data, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions on test set
test_predictions <- predict(rf_model, newdata = test_data)

# Calculate accuracy
conf_matrix <- confusionMatrix(test_predictions, test_data$cluster)
accuracy <- conf_matrix$overall["Accuracy"]

# Print results
print(conf_matrix)
print(paste("Model Accuracy:", round(accuracy, 4)))




###########################################################################################


# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn)
library(lme4)
library(lmerTest)
library(performance)

source("./modelling/utils_mm.R")
source("./modelling/model_pipeline.R")
source("./modelling/model_result_table.R")

options(na.action = na.fail)

# dataset for the models
data_mod <- pipeline_modelling(data_forests_census_int, is_census = FALSE)

# maximum number of predictors per model ~ 10 observations for each predictor
n_max <- round(nrow(data_mod) / 10)

# number of neighbours for SAC
k_n <- 20

# global model
m_global_c_gains <- lmer(log(c_gains) ~
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (warm_dry_climate_scaled + cluster +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

# check the model
# vif(m_global_c_gains)
check_collinearity(m_global_c_gains)



m_global_c_gains_climate <- lmer(log(c_gains) ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (warm_dry_climate_scaled +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_c_gains_climate)



m_global_c_gains_cluster <- lmer(log(c_gains) ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (cluster +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_c_gains_cluster)


##############################################################################################################################

m_global_c_losses_climate <- lmer(log(c_losses) ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (warm_dry_climate_scaled +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_c_losses_climate)

m_global_c_losses_cluster <- lmer(log(c_losses) ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (cluster +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_c_losses_cluster)


m_global_net_c_sink_climate <- lmer(net_c_sink ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (warm_dry_climate_scaled +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_net_c_sink_climate)


m_global_net_c_sink_cluster <- lmer(net_c_sink ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (cluster +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_net_c_sink_cluster)



m_global_crt_climate <- lmer(log(crt) ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (warm_dry_climate_scaled +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_crt_climate)


m_global_crt_cluster <- lmer(log(crt) ~  
                            site_area_ha_log_scaled +
                            soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled + 
                            (cluster +
                               shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
                               cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
                            (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

r.squaredGLMM(m_global_crt_cluster)


#######################################################################################################################################

data_forests_census <- read.table("census_level.csv",
     header = TRUE, dec = ".", sep = ",")

# Step 1: Aggregate long and lat by plot_code
plot_level_data <- data_forests_census %>%
  group_by(plot_code) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  )

# Step 2: Perform k-means clustering with 4 clusters
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(plot_level_data[, c("long", "lat")], centers = 4, nstart = 25)

# Step 3: Add cluster assignments to the aggregated data
plot_level_data$cluster <- kmeans_result$cluster

# Step 4: Merge clusters back into original data using plot_code
data_forests_census <- data_forests_census %>%
  left_join(plot_level_data %>% select(plot_code, cluster), by = "plot_code")

# Step 5: Convert cluster to factor (optional, for visualization)
data_forests_census$cluster <- as.factor(data_forests_census$cluster)

# dataset for the models
data_mod <- pipeline_modelling(data_forests_census, is_census = TRUE)

# maximum number of predictors per model ~ 10 observations for each predictor
n_max <- round(nrow(data_mod) / 10)

# number of neighbours for SAC
k_n <- 30

m_global_c_stocks_climate <- lmer(log(c_stocks_ha) ~
  site_area_ha_log_scaled +
  soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled +
  (warm_dry_climate_scaled +
     shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
    cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
  (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)

# R² for c_stocks climate model
r2_c_stocks_climate <- r.squaredGLMM(m_global_c_stocks_climate)

# Fit the c_stocks cluster model
m_global_c_stocks_cluster <- lmer(log(c_stocks_ha) ~
  site_area_ha_log_scaled +
  soil_fertily_scaled + aluminum_phosphorus_scaled + phosphorus_magnesium_scaled +
  (cluster +
     shannon_hill_scaled + wd_sd_scaled + sla_sd_scaled +
    cwm_wd_scaled + cwm_sla_scaled + sespd_scaled) * year_scaled +
  (1 | plot_code), REML = FALSE, weights = weig, data = data_mod)



# R² for c_stocks cluster model
r2_c_stocks_cluster <- r.squaredGLMM(m_global_c_stocks_cluster)

# Save absolute difference for c_stocks
abs_diff_c_stocks <- abs(r2_c_stocks_climate - r2_c_stocks_cluster)

# Compute R² for other models and calculate absolute differences (as before)

# R² for c_gains climate model
r2_c_gains_climate <- r.squaredGLMM(m_global_c_gains_climate)
# R² for c_gains cluster model
r2_c_gains_cluster <- r.squaredGLMM(m_global_c_gains_cluster)
# Save absolute difference for c_gains
abs_diff_c_gains <- abs(r2_c_gains_climate - r2_c_gains_cluster)

# R² for c_losses climate model
r2_c_losses_climate <- r.squaredGLMM(m_global_c_losses_climate)
# R² for c_losses cluster model
r2_c_losses_cluster <- r.squaredGLMM(m_global_c_losses_cluster)
# Save absolute difference for c_losses
abs_diff_c_losses <- abs(r2_c_losses_climate - r2_c_losses_cluster)

# R² for net_c_sink climate model
r2_net_c_sink_climate <- r.squaredGLMM(m_global_net_c_sink_climate)
# R² for net_c_sink cluster model
r2_net_c_sink_cluster <- r.squaredGLMM(m_global_net_c_sink_cluster)
# Save absolute difference for net_c_sink
abs_diff_net_c_sink <- abs(r2_net_c_sink_climate - r2_net_c_sink_cluster)

# R² for crt climate model
r2_crt_climate <- r.squaredGLMM(m_global_crt_climate)
# R² for crt cluster model
r2_crt_cluster <- r.squaredGLMM(m_global_crt_cluster)
# Save absolute difference for crt
abs_diff_crt <- abs(r2_crt_climate - r2_crt_cluster)

# Calculate the average absolute difference
average_abs_diff <- mean(c(abs_diff_c_gains, abs_diff_c_losses, abs_diff_net_c_sink, abs_diff_crt, abs_diff_c_stocks))

# Print average absolute difference
print(average_abs_diff)
