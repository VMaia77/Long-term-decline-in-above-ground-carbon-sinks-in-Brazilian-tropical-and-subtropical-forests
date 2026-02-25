library(dplyr)
library(tidyr)
library(ggplot2)
library(geosphere)



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

# Aggregate by plot_code to get average coordinates
agg_data <- data_forests_census_int %>%
  group_by(plot_code) %>%
  summarise(long = mean(long, na.rm = TRUE), 
            lat = mean(lat, na.rm = TRUE)) %>%
  ungroup()

# Compute pairwise distances
coords <- as.matrix(agg_data[, c("long", "lat")])
dist_matrix <- distm(coords, fun = distHaversine)  # Haversine distance in meters

# Get the maximum distance in km
max_dist_km <- max(dist_matrix) / 1000

print(max_dist_km)



# Initialize a data frame to store results
cluster_distances <- data.frame(cluster = integer(), mean_dist_km = numeric(), max_dist_km = numeric())

# Loop through each cluster
for (cl in unique(data_forests_census_int$cluster)) {
  
  # Subset data for the current cluster
  cluster_data <- data_forests_census_int %>% filter(cluster == cl)
  
  # Aggregate coordinates by plot_code within the cluster
  agg_cluster_data <- cluster_data %>%
    group_by(plot_code) %>%
    summarise(long = mean(long, na.rm = TRUE), 
              lat = mean(lat, na.rm = TRUE)) %>%
    ungroup()
  
  # Compute pairwise distances
  if (nrow(agg_cluster_data) > 1) {  # Ensure there is more than one point
    coords <- as.matrix(agg_cluster_data[, c("long", "lat")])
    dist_matrix <- distm(coords, fun = distHaversine)  # Haversine distance in meters
    
    # Extract upper triangle of the distance matrix (excluding diagonal)
    dist_values <- dist_matrix[upper.tri(dist_matrix)]
    
    # Compute mean and max distances in km
    mean_dist_km <- mean(dist_values) / 1000
    max_dist_km <- max(dist_values) / 1000
  } else {
    # If only one point in the cluster, distances are NA
    mean_dist_km <- NA
    max_dist_km <- NA
  }
  
  # Store results in the dataframe
  cluster_distances <- rbind(cluster_distances, data.frame(cluster = cl, mean_dist_km = mean_dist_km, max_dist_km = max_dist_km))
}

# Print results
print(cluster_distances)



# # # # checking close plots
# # Aggregate by plot_code to get mean coordinates
# agg_data <- data_forests_census_int %>%
#   group_by(plot_code) %>%
#   summarise(long = mean(long, na.rm = TRUE), 
#             lat = mean(lat, na.rm = TRUE), .groups = "drop") %>%
#   mutate(plot_code = as.character(plot_code))  # Ensure character type

# # Generate all unique plot_code pairs efficiently
# plot_pairs <- as.data.frame(t(combn(agg_data$plot_code, 2))) %>%
#   rename(plot_code1 = V1, plot_code2 = V2)

# # Join coordinates for both plot codes
# plot_pairs <- plot_pairs %>%
#   left_join(agg_data, by = c("plot_code1" = "plot_code")) %>%
#   left_join(agg_data, by = c("plot_code2" = "plot_code"), suffix = c("_1", "_2")) %>%
#   mutate(distance_km = distHaversine(cbind(long_1, lat_1), cbind(long_2, lat_2)) / 1000) %>%  # Convert to km
#   filter(distance_km < 1)  

# # Print final result
# print(plot_pairs)

