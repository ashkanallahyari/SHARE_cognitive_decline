# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)


##### REEMtree
# ----------------------
# Loading the base dataset
REEMtree_train <- readRDS("data/REEMtree_df_train_cf_score.rds")
REEMtree_test <- readRDS("data/REEMtree_df_test_cf_score.rds")
# Rename the columns
REEMtree_train <- REEMtree_train %>% rename(pred = pred_train)
REEMtree_test <- REEMtree_test %>% rename(pred = pred_test)
# Bind the rows together and adding clusters
REEMtree <- bind_rows(REEMtree_train, REEMtree_test)


# create the summary data
plot_df <- REEMtree %>% 
  group_by(wave) %>% 
  summarise(
    real = mean(cf_score, na.rm = TRUE),
    pred = mean(pred, na.rm = TRUE)
  ) %>% 
  tidyr::pivot_longer(cols = c(real, pred),
                      names_to = "series",
                      values_to = "value")

p <- ggplot(plot_df, aes(x = wave, y = value, colour = series)) +
  geom_line(size = 0.8) +
  geom_point() +
  scale_colour_manual(values = c("real" = "#505050", "pred" = "steelblue"),
                      labels = c("Real", "Predicted")) +
  scale_y_continuous(limits = c(85, 115), expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  labs(title = "Mean cf_scoreitive score (REEMtree)",
       x = "Wave",
       y = "cf_scoreitive Functioning Score") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

# Ensure the folder exists
if (!dir.exists("figures")) dir.create("figures")

# Save as PNG (change width/height/dpi as needed)
ggsave("figures/REEMtree_mean_cf_score_vs_pred.jpg", plot = p, width = 8, height = 4, dpi = 300)


# Prediction Error
p_error <- REEMtree %>%
  mutate(error = cf_score - pred) %>%
  ggplot(aes(x = factor(wave), y = error)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = .2) +
  labs(title = "Prediction error (REEMtree)",
       x = "Wave", y = "Error") +
  theme_minimal(base_size = 14)

# ensure the figures directory exists
if (!dir.exists("figures")) dir.create("figures")

# save the plot
ggsave("figures/REEMtree_prediction_error_by_wave.jpg", plot   = p_error, width  = 8, height = 4, dpi    = 300)




##### MERT Algorithm
# ----------------------
# Loading the base dataset
MERT_train <- readRDS("data/MERT_df_train_cf_score.rds")
MERT_test <- readRDS("data/MERT_df_test_cf_score.rds")

# Rename the columns
MERT_train <- MERT_train %>% rename(pred = pred_train)
MERT_test <- MERT_test %>% rename(pred = pred_test)

# Bind the rows together and add clusters
MERT <- bind_rows(MERT_train, MERT_test)

# Create the summary data
plot_df <- MERT %>% 
  group_by(wave) %>% 
  summarise(
    real = mean(cf_score, na.rm = TRUE),
    pred = mean(pred, na.rm = TRUE)
  ) %>% 
  tidyr::pivot_longer(cols = c(real, pred),
                      names_to = "series",
                      values_to = "value")

p <- ggplot(plot_df, aes(x = wave, y = value, colour = series)) +
  geom_line(size = 0.8) +
  geom_point() +
  scale_colour_manual(values = c("real" = "#505050", "pred" = "steelblue"),
                      labels = c("Real", "Predicted")) +
  scale_y_continuous(limits = c(85, 115), expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  labs(title = "Mean cf_scoreitive score (MERT)",
       x = "Wave",
       y = "cf_scoreitive Functioning Score") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

# Ensure the folder exists
if (!dir.exists("figures")) dir.create("figures")

# Save as PNG (change width/height/dpi as needed)
ggsave("figures/MERT_mean_cf_score_vs_pred.jpg", plot = p, width = 8, height = 4, dpi = 300)

# Prediction Error
p_error <- MERT %>%
  mutate(error = cf_score - pred) %>%
  ggplot(aes(x = factor(wave), y = error)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = .2) +
  labs(title = "Prediction error (MERT)",
       x = "Wave", y = "Error") +
  theme_minimal(base_size = 14)

# Ensure the figures directory exists
if (!dir.exists("figures")) dir.create("figures")

# Save the plot
ggsave("figures/MERT_prediction_error_by_wave.jpg", plot = p_error, width = 8, height = 4, dpi = 300)





##### MERF Algorithm
# ----------------------
# Loading the base dataset
MERF_train <- readRDS("data/MERF_df_train_cf_score.rds")
MERF_test <- readRDS("data/MERF_df_test_cf_score.rds")

# Rename the columns
MERF_train <- MERF_train %>% rename(pred = pred_train)
MERF_test <- MERF_test %>% rename(pred = pred_test)

# Bind the rows together and add clusters
MERF <- bind_rows(MERF_train, MERF_test)

# Create the summary data
plot_df <- MERF %>% 
  group_by(wave) %>% 
  summarise(
    real = mean(cf_score, na.rm = TRUE),
    pred = mean(pred, na.rm = TRUE)
  ) %>% 
  tidyr::pivot_longer(cols = c(real, pred),
                      names_to = "series",
                      values_to = "value")

p <- ggplot(plot_df, aes(x = wave, y = value, colour = series)) +
  geom_line(size = 0.8) +
  geom_point() +
  scale_colour_manual(values = c("real" = "#505050", "pred" = "steelblue"),
                      labels = c("Real", "Predicted")) +
  scale_y_continuous(limits = c(85, 115), expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  labs(title = "Mean cf_scoreitive score (MERF)",
       x = "Wave",
       y = "cf_scoreitive Functioning Score") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

# Ensure the folder exists
if (!dir.exists("figures")) dir.create("figures")

# Save as PNG (change width/height/dpi as needed)
ggsave("figures/MERF_mean_cf_score_vs_pred.jpg", plot = p, width = 8, height = 4, dpi = 300)

# Prediction Error
p_error <- MERF %>%
  mutate(error = cf_score - pred) %>%
  ggplot(aes(x = factor(wave), y = error)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = .2) +
  labs(title = "Prediction error (MERF)",
       x = "Wave", y = "Error") +
  theme_minimal(base_size = 14)

# Ensure the figures directory exists
if (!dir.exists("figures")) dir.create("figures")

# Save the plot
ggsave("figures/MERF_prediction_error_by_wave.jpg", plot = p_error, width = 8, height = 4, dpi = 300)
