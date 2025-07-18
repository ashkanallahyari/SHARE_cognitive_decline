# Load necessary packages
library(dplyr)
library(tidyr)  
library(LongituRF)
library(randomForest)
library(ggplot2)
library(forcats)
library(Metrics)
library(purrr)
library(rpart.plot)
library(glue)
library(mice)

##### Preparing data
# =====================================================
# Load the RDS file
data_l <- readRDS("data/sh.l.s.ext_cls.rds")


# Select variables and outcome
target_var <- c("cf_score")
predictor_vars <- c("dfun", "sp_social_support", "sprt_social_participation", "is_score", "ns_netsize", "ln_loneliness", "ST_satisfaction", "se_social_engagement")
selected_vars <- c("ID", "wave", target_var, predictor_vars)



# Step 1: Store IDs and Waves Where `cf_score` Was Originally Missing
data_l <- data_l %>%
  mutate(ID = as.numeric(factor(mergeid)))

missing_records <- data_l %>%
  filter(is.na(cf_score)) %>%
  select(ID, wave)  # Use consistent ID column

# Step 2: Perform MICE Imputation for `cf_score`, `dfun`, and `sprt_social_participation`
imp <- mice(data_l %>% select(ID, wave, cf_score),
            m = 10,        # Number of imputed datasets
            maxit = 5,   # Number of iterations
            seed = 5)     # For reproducibility)

# Extract the first completed dataset (with imputed values)
data_imputed <- complete(imp, 1)


# Step 3: Count Non-Null `cf_score` Values Per ID Before Imputation
cf_score_counts <- data_l %>%
  group_by(ID) %>%
  summarise(non_null_cf_score_count = sum(!is.na(cf_score))) %>%
  ungroup()

# Merge cf_scoreition counts back into `data_l`
data_l <- data_l %>%
  left_join(cf_score_counts, by = "ID")

# Step 4: Identify IDs With **Exactly 3** Non-Null `cf_score` Values
selected_ids <- cf_score_counts %>%
  filter(non_null_cf_score_count == 3) %>%
  pull(ID)

# Step 5: Keep Only Imputed Values for Records That Were Originally Missing
imputed_missing_records <- data_imputed %>%
  inner_join(missing_records, by = c("ID", "wave")) %>%
  select(ID, wave, cf_score)  # Keep necessary variables

# Step 6: Select the First Imputed Value for IDs With 3 Non-Null `cf_score`
first_imputed <- imputed_missing_records %>%
  filter(ID %in% selected_ids) %>%
  arrange(ID, wave) %>%
  group_by(ID) %>%
  slice_head(n = 1) %>%  # Select first missing wave
  ungroup()

# Step 7: Merge Only Selected Imputed Values Into `data_l`
data_l <- data_l %>%
  left_join(first_imputed, by = c("ID", "wave"), suffix = c("", "_imputed")) %>%
  mutate(cf_score = ifelse(!is.na(cf_score_imputed), cf_score_imputed, cf_score)) %>%
  select(-cf_score_imputed)  # Remove temporary columns



# Creating the dataframe for training and testing ML model
df_model <- data_l %>%
  select(all_of(selected_vars))


# Handle missing values
df_model <- na.omit(df_model)


# Train and test split (assigning at least 20% of data to test set)
# Setting seed for reproducibility
set.seed(5) 

# Compute the number of waves per sprt_social_participationicipant
df_model <- df_model %>%
  group_by(ID) %>%
  mutate(
    non_null_cf_score = sum(!is.na(cf_score)),  # Count non-null cf_score values per ID
    test_waves = ceiling(non_null_cf_score * 0.2)  # Always round up 20% of waves
  ) %>%
  ungroup() %>%
  filter(non_null_cf_score >= 2)  # Remove IDs with non_null_cf_score below 2

# Split data by sprt_social_participationicipant, sampling the required waves for test set
df_test <- df_model %>%
  group_split(ID) %>%
  map_dfr(~ slice_sample(.x, n = unique(.x$test_waves), replace = FALSE))  # Sample waves per sprt_social_participationicipant
# map_dfr(~ slice_sample(.x, n = 1))

# Ensure test waves are removed from the train dataset
df_train <- anti_join(df_model, df_test, by = c("ID", "wave"))


# Drop "test_waves" and "non_null_cf_score" columns from df_model, df_train, and df_test
df_model <- df_model %>%
  select(-test_waves, -non_null_cf_score)

df_train <- df_train %>%
  select(-test_waves, -non_null_cf_score)

df_test <- df_test %>%
  select(-test_waves, -non_null_cf_score)


# Preparing train format
x_train <- df_train %>%
  ungroup() %>%
  select(-ID, -wave, -cf_score)         # Excluding feature that are not predictors



# Transform data to suitable format for LongituRF
df_list_train <- list(
  X = as.matrix(x_train),                    # Convert predictors to matrix
  Y = as.numeric(pull(df_train, cf_score)),      # Outcome variable
  id = as.integer(pull(df_train, ID)),       # Subject ID
  Z = as.matrix(rep(1, nrow(df_train))),     # Random intercept
  time = as.numeric(pull(df_train, wave))    # Wave as time variable
)


##### Training LongituRF model (MERF)
# =====================================================
merf_model <- LongituRF::MERF(
  X = data.frame(df_list_train$X),             # Convert predictors to data frame
  Y = df_list_train$Y,
  id = df_list_train$id,
  Z = df_list_train$Z,
  time = df_list_train$time,
  sto = "none",                                  # "OrnUhl" or "none"
  iter = 100,                                    # Maximum number of iterations (100)
  mtry = ceiling(ncol(df_list_train$X) / 3),     # Number of variables sampled per split (default: p/3)
  ntree = 500,                                   # Number of trees in the forest (500)
  delta = 0.001                                  # Convergence threshold (0.001)
)

# Get model details
summary(merf_model)


# Saving the trained model
# Create "models" folder if it doesn't exist
if (!dir.exists("models")) {
  dir.create("models")
}

# Extract the value from target_var
target_value_name <- target_var[1]

# Generate dynamic file names for ML model
model_filename_rds <- paste0("models/merf_model_", target_value_name, ".rds")
model_filename_rdata <- paste0("models/merf_model_", target_value_name, ".RData")

# Save the ML model with dynamic names
saveRDS(merf_model, file = model_filename_rds)
save(merf_model, file = model_filename_rdata)


##### Extract variable importance from MERF model
# =====================================================
variable_importance <- tibble(
  variables = rownames(merf_model$forest$importance),  
  importance = merf_model$forest$importance[, "%IncMSE"]  # Or use "IncNodePurity" if preferred
)



# Create importance plot
importance_plot_merf <- ggplot(variable_importance) +
  geom_col(aes(y = fct_reorder(variables, importance), x = importance), show.legend = FALSE) +
  labs(y = element_blank(), x = "Importance") +
  ggtitle("MERF Variable Importance") +
  theme_grey(base_size = 15) +
  theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(vjust = -2),
    plot.margin = unit(c(1, 0.40, 1, 0), "cm")
  )

print(importance_plot_merf)

# Create "models" folder if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

ggsave("figures/importance_plot_merf.jpg", plot = importance_plot_merf, width = 8, height = 6, dpi = 300)



##### Calculating the model's perfomance
# =====================================================
# Selecting predictors for test dataset
x_test <- df_test %>%
  ungroup() %>%
  select(-ID, -wave, -cf_score)  # Excluding subject ID, time variable, and outcome

# Transform test data to LongituRF format
df_list_test <- list(
  X = as.matrix(x_test),  # Convert predictors to matrix
  Y = as.numeric(pull(df_test, cf_score)),  # Outcome variable
  id = as.integer(pull(df_test, ID)),  # Subject ID
  Z = as.matrix(rep(1, nrow(df_test))),  # Random intercept
  time = as.numeric(pull(df_test, wave))  # Wave as time variable
)

#df_list_train$Z <- matrix(rep(1, nrow(df_train)), ncol = 1)  # Random intercept as matrix
#df_list_test$Z <- matrix(rep(1, nrow(df_test)), ncol = 1)  # Ensure numeric format


# Making predictions for MERF on train dataset
pred_train <- predict(
  merf_model,
  X = as.data.frame(df_list_train$X),  # Ensure X is a data frame
  Z = df_list_train$Z,  # Ensure Z is a data frame
  id = df_list_train$id,  # Subject ID
  time = df_list_train$time  # Time variable (wave)
)


# Applying predictions to train dataset
df_train$pred_train <- pred_train



# Calculate MAE for train and test sets
train_mae_MERF <- mae(df_train$cf_score, df_train$pred_train)

# Calculate RMSE for both models
train_rmse_MERF <- rmse(df_train$cf_score, df_train$pred_train)

# Calculate R² for both models
train_r2_MERF <- cor(df_train$cf_score, df_train$pred_train)^2




# Making predictions for MERF on test dataset
pred_test <- predict(
  merf_model,
  X = as.data.frame(df_list_test$X),  # Ensure X is a data frame
  Z = df_list_test$Z,  # Ensure Z is a data frame
  id = df_list_test$id,  # Subject ID
  time = df_list_test$time  # Time variable (wave)
)


# Applying predictions to train dataset
df_test$pred_test <- pred_test

# Calculate MAE for both models
test_mae_MERF <- mae(df_test$cf_score, df_test$pred_test)

# Calculate RMSE for both models
test_rmse_MERF <- rmse(df_test$cf_score, df_test$pred_test)

# Calculate R² for both models
test_r2_MERF <- cor(df_test$cf_score, df_test$pred_test)^2


# Print results
print(glue::glue("Train MAE MERF: {train_mae_MERF}   |   Test MAE MERF: {test_mae_MERF}"))
print(glue::glue("Train RMSE MERF: {train_rmse_MERF}   |   Test RMSE MERF: {test_rmse_MERF}"))
print(glue::glue("Train R² MERF: {train_r2_MERF}   |   Test R² MERF: {test_r2_MERF}"))


# Ensure "reports" folder exists
if (!dir.exists("reports")) {
  dir.create("reports")
}

# Define the file path for saving results
report_file <- "reports/MERF_model_performance.txt"

# Prepare performance results
performance_results <- glue::glue("
Train MAE MERF: {train_mae_MERF}   |   Test MAE MERF: {test_mae_MERF}
Train RMSE MERF: {train_rmse_MERF}   |   Test RMSE MERF: {test_rmse_MERF}
Train R² MERF: {train_r2_MERF}   |   Test R² MERF: {test_r2_MERF}
")

# Save results to a text file
writeLines(performance_results, report_file)


# Create "models" folder if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Extract the value from target_var
target_value_name <- target_var[1]

# Generate dynamic file names
train_filename <- paste0("data/MERF_df_train_", target_value_name, ".rds")
test_filename <- paste0("data/MERF_df_test_", target_value_name, ".rds")

# Save the data with dynamic names
saveRDS(df_train, file = train_filename)
saveRDS(df_test, file = test_filename)




