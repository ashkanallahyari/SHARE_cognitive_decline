# Clustering KML
# Libraries
library(kml)
library(dplyr)
library(ggplot2)
library(tidyr)


# Load the RDS file
data_w <- readRDS("data/sh.w.s.ext.rds")
data_l <- readRDS("data/sh.l.s.ext.rds")


data_w <- data_w[1:1000, ]



# selecting variables
KML_variables <- c("cf_score_0", "cf_score_2", "cf_score_4", "cf_score_6", "cf_score_8", "cf_score_10", "cf_score_12", "cf_score_14")

id_col <- "mergeid"

# Remove duplicated IDs (if any)
data_unique <- data_w[!duplicated(data_w[[id_col]]), ]

# Create LongData object
traj_matrix <- as.matrix(data_unique[, KML_variables])
ids <- data_unique[[id_col]]

traj_long <- cld(traj_matrix, id = ids, timeInData = TRUE)

# Run KML to determine the best clusters (2-10 clusters as an example)
kml(traj_long, nbClusters = 2:10)

# Plot the results
plot(traj_long)


# Extract the clusters
clustersKML_2 <- getClusters(traj_long, 2)
clustersKML_3 <- getClusters(traj_long, 3)
clustersKML_4 <- getClusters(traj_long, 4)
clustersKML_5 <- getClusters(traj_long, 5)
clustersKML_6 <- getClusters(traj_long, 6)
clustersKML_7 <- getClusters(traj_long, 7)
clustersKML_8 <- getClusters(traj_long, 8)
clustersKML_9 <- getClusters(traj_long, 9)
clustersKML_10 <- getClusters(traj_long, 10)


# Add cluster labels back to the filtered dataset
data_unique$KML_2 <- clustersKML_2
data_unique$KML_3 <- clustersKML_3
data_unique$KML_4 <- clustersKML_4
data_unique$KML_5 <- clustersKML_5
data_unique$KML_6 <- clustersKML_6
data_unique$KML_7 <- clustersKML_7
data_unique$KML_8 <- clustersKML_8
data_unique$KML_9 <- clustersKML_9
data_unique$KML_10 <- clustersKML_10



# Finding the most appropriate cluster size
# Ensure "figures" folder exists
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Save the plot as a JPEG file
jpeg("figures/plot_all_criterion.jpg", width = 800, height = 600)
plotAllCriterion(traj_long)  # Generate the plot
dev.off()  # Close the graphical device


# Save the image as a PNG file
png("figures/kml_choice_plot.png", width = 800, height = 600)
try(kml::choice(traj_long))  # Generate the plot
dev.off()  # Close the graphical device


# Final dataset with clusters
data_w <- data_w %>%
  left_join(data_unique %>%
              select(mergeid, KML_2, KML_3, KML_4, KML_5, KML_6, KML_7, KML_8, KML_9, KML_10), by = "mergeid")


saveRDS(data_w, "data/sh.w.s.ext_cls.rds")
write.csv(data_w, "data/sh.w.s.ext_cls.csv", row.names = FALSE)



# ========== Clusters figures ==========
# Clustering 2 - Separated
# ------------
traj_long_df <- data_w %>%
  select(mergeid, KML_2, all_of(KML_variables)) %>%
  rename(cluster = KML_2) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_2_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 2 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_2, all_of(KML_variables)) %>%
  rename(cluster = KML_2) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_2_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)


# ----------------------------
# Clustering 3 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_3, all_of(KML_variables)) %>%
  rename(cluster = KML_3) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_3_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 3 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_3, all_of(KML_variables)) %>%
  rename(cluster = KML_3) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_3_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)




# ----------------------------
# Clustering 4 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_4, all_of(KML_variables)) %>%
  rename(cluster = KML_4) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_4_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 4 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_4, all_of(KML_variables)) %>%
  rename(cluster = KML_4) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_4_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)





# ----------------------------
# Clustering 5 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_5, all_of(KML_variables)) %>%
  rename(cluster = KML_5) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_5_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 5 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_5, all_of(KML_variables)) %>%
  rename(cluster = KML_5) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_5_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)



# ----------------------------
# Clustering 6 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_6, all_of(KML_variables)) %>%
  rename(cluster = KML_6) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_6_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 6 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_6, all_of(KML_variables)) %>%
  rename(cluster = KML_6) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_6_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)




# ----------------------------
# Clustering 7 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_7, all_of(KML_variables)) %>%
  rename(cluster = KML_7) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_7_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 7 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_7, all_of(KML_variables)) %>%
  rename(cluster = KML_7) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_7_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)



# ----------------------------
# Clustering 8 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_8, all_of(KML_variables)) %>%
  rename(cluster = KML_8) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_8_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 8 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_8, all_of(KML_variables)) %>%
  rename(cluster = KML_8) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_8_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)



# ----------------------------
# Clustering 9 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_9, all_of(KML_variables)) %>%
  rename(cluster = KML_9) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_9_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 9 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_9, all_of(KML_variables)) %>%
  rename(cluster = KML_9) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_9_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)



# ----------------------------
# Clustering 10 - Separated
# ----------------------------
traj_long_df <- data_w %>%
  select(mergeid, KML_10, all_of(KML_variables)) %>%
  rename(cluster = KML_10) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid)) +
  geom_line(alpha = 0.025, color = "black") +
  stat_summary(fun = mean, geom = "line", size = 1.5, color = "red", aes(group = cluster)) +
  facet_wrap(~cluster) +
  theme_minimal() +
  labs(title = "KML Trajectories by Cluster",
       x = "wave", 
       y = "cf_scoreitive Function Score")

# print(traj_long_df)

# Save the plot
ggsave("figures/KML_10_separated.jpg", plot = traj_plot, width = 8, height = 6, dpi = 300)


# Clustering 10 - Combined
# -------------
traj_long_df <- data_w %>%
  select(mergeid, KML_10, all_of(KML_variables)) %>%
  rename(cluster = KML_10) %>%  
  pivot_longer(cols = all_of(KML_variables), names_to = "wave", values_to = "value")

# Convert wave to numeric (e.g., from "cf_score_0", "cf_score_2", ...) to 0, 2, ...
traj_long_df$wave <- as.numeric(gsub("cf_score_", "", traj_long_df$wave))

# Plot individual trajectories and cluster mean lines on one chart
traj_plot <- ggplot(traj_long_df, aes(x = wave, y = value, group = mergeid, color = as.factor(cluster))) +
  geom_line(alpha = 0.025) +  # Individual trajectories with light transparency
  stat_summary(fun = mean, geom = "line", size = 1.5, aes(group = cluster, color = as.factor(cluster))) +  # Bold line for cluster mean
  theme_minimal() +
  labs(title = "Combined Trajectories for All Clusters", 
       x = "Waves", 
       y = "cf_scoreitive Function Score", 
       color = "Cluster")

# print(traj_plot)

# Save the plot to the figures
ggsave("figures/KML_10_combined.jpg", plot = traj_plot, width = 10, height = 8, dpi = 300)






# K Mean clustering (with base values)
kmean_vars <- c(
  "ns_netsize_0", "is_score_0", "ln_loneliness_0", "sprt_social_participation_0",
  "sp_social_support_0", "se_social_engagement_0"
)


# Subset the dataset for K-means clustering using the specified variables
kmean_data <- data_w %>%
  select(mergeid, all_of(kmean_vars))  # Include mergeid for merging later

# Handle missing values
kmean_data <- kmean_data %>%
  filter(complete.cases(.))  # Remove rows with missing values

# Extract the numeric matrix for K-means clustering
kmean_matrix <- as.matrix(kmean_data[, kmean_vars])

# Perform K-means clustering for 2 to 10 clusters
set.seed(123)  # For reproducibility
for (k in 2:8) {
  kmeans_col_name <- paste0("kmean", k)  # Generate column names like kmean2, kmean3, ..., kmean6
  kmean_data[[kmeans_col_name]] <- kmeans(kmean_matrix, centers = k, nstart = 25)$cluster
}

# Now merge the K-means cluster columns back to data_wide_cluster using the mergeid column
data_w_cls <- data_w %>%
  left_join(kmean_data %>%
              select(mergeid, kmean2, kmean3, kmean4, kmean5, kmean6, kmean7, kmean8), by = "mergeid")


saveRDS(data_w_cls, "data/sh.w.s.ext_cls.rds")
write.csv(data_w_cls, "data/sh.w.s.ext_cls.csv", row.names = FALSE)




# adding clusters to long version
data_l <- data_l %>%
  left_join(data_w_cls %>%
              select(mergeid, kmean2, kmean3, kmean4, kmean5, kmean6, kmean7, kmean8,
                     KML_2, KML_3, KML_4, KML_5, KML_6, KML_7, KML_8, KML_9, KML_10), by = "mergeid")

saveRDS(data_l, "data/sh.l.s.ext_cls.rds")
write.csv(data_l, "data/sh.l.s.ext_cls.csv", row.names = FALSE)







