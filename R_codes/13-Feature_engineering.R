# Load necessary packages
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(broom)



# Load the RDS file
data_w <- readRDS("data/sh.w.s.ext.rds")
data_l <- readRDS("data/sh.l.s.ext.rds")



##### Base CF
# ------------------------------------
data_w$cf_score_base <- data_w$cf_score_0


##### Final CF
# ----------------------------------------
# Pivot to long format
cf_long_last <- data_w %>%
  select(mergeid, starts_with("cf_score_")) %>%
  pivot_longer(
    cols = starts_with("cf_score_"),
    names_to = "wave",
    names_pattern = "cf_score_(\\d+)",
    values_to = "cf_score"
  ) %>%
  mutate(wave = as.numeric(wave)) %>%
  filter(!is.na(cf_score))  # Remove NA values

# Get last available score for each participant
cf_last <- cf_long_last %>%
  group_by(mergeid) %>%
  slice_max(order_by = wave, n = 1, with_ties = FALSE) %>%  # Ensures last non-null value
  ungroup() %>%
  select(mergeid, cf_score_final = cf_score)  # Rename the column for clarity

# Join back to wide data
data_w <- data_w %>%
  left_join(cf_last, by = "mergeid")


##### Statistics Measures - cf_scoreitive Functioning
# ------------------------------------
cf_variables <- c("cf_score_0", "cf_score_2", "cf_score_4", "cf_score_6", "cf_score_8", "cf_score_10", "cf_score_12", "cf_score_14")


# For data_w
data_w$cf_score_mean <- rowMeans(data_w[, cf_variables], na.rm = TRUE)
data_w$cf_score_sd <- apply(data_w[, cf_variables], 1, sd, na.rm = TRUE)
data_w$cf_score_min <- apply(data_w[, cf_variables], 1, min, na.rm = TRUE)
data_w$cf_score_max <- apply(data_w[, cf_variables], 1, max, na.rm = TRUE)


##### cf_scoreitive Functioning lag values
# ------------------------------------
# For data_w
data_w$cf_scoreLAG_0 <- NA
data_w$cf_scoreLAG_2 <- data_w$cf_score_0
data_w$cf_scoreLAG_4 <- data_w$cf_score_2
data_w$cf_scoreLAG_6 <- data_w$cf_score_4
data_w$cf_scoreLAG_8 <- data_w$cf_score_6
data_w$cf_scoreLAG_10 <- data_w$cf_score_8
data_w$cf_scoreLAG_12 <- data_w$cf_score_10
data_w$cf_scoreLAG_14 <- data_w$cf_score_12


##### cf_scoreitive Functioning Lead values (one wave ahead)
# ------------------------------------
# For data_w
data_w$cf_scoreLead_0 <- data_w$cf_score_2
data_w$cf_scoreLead_2 <- data_w$cf_score_4
data_w$cf_scoreLead_4 <- data_w$cf_score_6
data_w$cf_scoreLead_6 <- data_w$cf_score_8
data_w$cf_scoreLead_8 <- data_w$cf_score_10
data_w$cf_scoreLead_10 <- data_w$cf_score_12
data_w$cf_scoreLead_12 <- data_w$cf_score_14
data_w$cf_scoreLead_14 <- NA


##### cf_scoreitive Functioning DLTLEAD (one wave back)
# ------------------------------------
# For DATA_w
data_w$cf_scoreDLT_0 <- NA
data_w$cf_scoreDLT_2 <- data_w$cf_score_2 - data_w$cf_score_0
data_w$cf_scoreDLT_4 <- data_w$cf_score_4 - data_w$cf_score_2
data_w$cf_scoreDLT_6 <- data_w$cf_score_6 - data_w$cf_score_4
data_w$cf_scoreDLT_8 <- data_w$cf_score_8 - data_w$cf_score_6
data_w$cf_scoreDLT_10 <- data_w$cf_score_10 - data_w$cf_score_8
data_w$cf_scoreDLT_12 <- data_w$cf_score_12 - data_w$cf_score_10
data_w$cf_scoreDLT_14 <- data_w$cf_score_14 - data_w$cf_score_12


##### cf_scoreitive Functioning DLTLEAD (compared to base value)
# ------------------------------------
# For DATA_w
data_w$cf_scoreDLTbase_0 <- 0
data_w$cf_scoreDLTbase_2 <- data_w$cf_score_2 - data_w$cf_score_0
data_w$cf_scoreDLTbase_4 <- data_w$cf_score_4 - data_w$cf_score_0
data_w$cf_scoreDLTbase_6 <- data_w$cf_score_6 - data_w$cf_score_0
data_w$cf_scoreDLTbase_8 <- data_w$cf_score_8 - data_w$cf_score_0
data_w$cf_scoreDLTbase_10 <- data_w$cf_score_10 - data_w$cf_score_0
data_w$cf_scoreDLTbase_12 <- data_w$cf_score_12 - data_w$cf_score_0
data_w$cf_scoreDLTbase_14 <- data_w$cf_score_14 - data_w$cf_score_0


##### cf_scoreitive Functioning DLTLEAD (one wave ahead)
# ------------------------------------
# For DATA_w
data_w$cf_scoreDLTlead_0 <- data_w$cf_score_2 - data_w$cf_score_0
data_w$cf_scoreDLTlead_2 <- data_w$cf_score_4 - data_w$cf_score_2
data_w$cf_scoreDLTlead_4 <- data_w$cf_score_6 - data_w$cf_score_4
data_w$cf_scoreDLTlead_6 <- data_w$cf_score_8 - data_w$cf_score_6
data_w$cf_scoreDLTlead_8 <- data_w$cf_score_10 - data_w$cf_score_8
data_w$cf_scoreDLTlead_10 <- data_w$cf_score_12 - data_w$cf_score_10
data_w$cf_scoreDLTlead_12 <- data_w$cf_score_14 - data_w$cf_score_12
data_w$cf_scoreDLTlead_14 <- NA


##### Calculating Slop (total waves)
# ------------------------------------
# For data_w
# Select relevant columns and pivot to long format
cf_long_w <- data_w %>%
  select(mergeid, starts_with("cf_score_")) %>%
  pivot_longer(
    cols = starts_with("cf_score_"),
    names_to = "wave",
    names_pattern = "cf_score_(\\d+)",
    values_to = "cf_score"
  ) %>%
  mutate(wave = as.numeric(wave))

# Compute the slope of cf_score ~ wave for data_w (no need to re-scale)
cf_slopes_w <- cf_long_w %>%
  group_by(mergeid) %>%
  filter(n() >= 2, !all(is.na(cf_score))) %>%
  summarise(
    cf_score_slop = coef(lm(cf_score ~ wave))[["wave"]],
    .groups = "drop"
  )

# Join back to wide data
data_w <- data_w %>%
  left_join(cf_slopes_w, by = "mergeid")


##### Calculating Slop (backward)
# ------------------------------------
# For data_w
# Step 1: Pivot cf_score to long format
cf_long <- data_w %>%
  select(mergeid, starts_with("cf_score_")) %>%
  pivot_longer(
    cols = starts_with("cf_score_"),
    names_to = "wave",
    names_pattern = "cf_score_(\\d+)",
    values_to = "cf_score"
  ) %>%
  mutate(wave = as.numeric(wave))


# Step 2: Define target waves for prediction
target_waves <- seq(2, 14, by = 2)


# Step 3: Compute slopes up to each target wave
slopes_list <- map_dfr(target_waves, function(w) {
  cf_long %>%
    filter(wave <= w) %>%
    group_by(mergeid) %>%
    filter(sum(!is.na(cf_score)) >= 2) %>%
    summarise(
      cf_slope = coef(lm(cf_score ~ wave))[["wave"]],
      .groups = "drop"
    ) %>%
    mutate(wave = w)
})

# Step 4: Pivot to wide format: one column per slope wave
cf_slopes_wide <- slopes_list %>%
  pivot_wider(
    names_from = wave,
    names_prefix = "cf_score_slopBack_",
    values_from = cf_slope
  )

# Step 5: Merge back to data_w
data_w <- data_w %>%
  left_join(cf_slopes_wide, by = "mergeid")


##### Adding features to long table
# ----------------------------------
time_series_vars <- c("cf_scoreLAG_0", "cf_scoreLAG_2", "cf_scoreLAG_4", "cf_scoreLAG_6", "cf_scoreLAG_8", "cf_scoreLAG_10", "cf_scoreLAG_12", "cf_scoreLAG_14",
                      "cf_scoreLead_0", "cf_scoreLead_2", "cf_scoreLead_4", "cf_scoreLead_6", "cf_scoreLead_8", "cf_scoreLead_10", "cf_scoreLead_12", "cf_scoreLead_14",
                      "cf_scoreDLT_0", "cf_scoreDLT_2", "cf_scoreDLT_4", "cf_scoreDLT_6", "cf_scoreDLT_8", "cf_scoreDLT_10", "cf_scoreDLT_12", "cf_scoreDLT_14",
                      "cf_scoreDLTbase_0", "cf_scoreDLTbase_2", "cf_scoreDLTbase_4", "cf_scoreDLTbase_6", "cf_scoreDLTbase_8", "cf_scoreDLTbase_10", "cf_scoreDLTbase_12", "cf_scoreDLTbase_14",
                      "cf_scoreDLTlead_0", "cf_scoreDLTlead_2", "cf_scoreDLTlead_4", "cf_scoreDLTlead_6", "cf_scoreDLTlead_8", "cf_scoreDLTlead_10", "cf_scoreDLTlead_12", "cf_scoreDLTlead_14",
                      "cf_score_slopBack_2", "cf_score_slopBack_4", "cf_score_slopBack_6", "cf_score_slopBack_8", "cf_score_slopBack_10", "cf_score_slopBack_12", "cf_score_slopBack_14")

extra_f_stable <- c("cf_score_base", "cf_score_final", "cf_score_mean", "cf_score_sd", "cf_score_min", "cf_score_max", "cf_score_slop")



# Select only relevant columns from data_w
# ---------------------------------------
data_l_new <- data_w %>%
  select(mergeid, all_of(time_series_vars)) %>%
  pivot_longer(
    cols = -mergeid,
    names_to = c("variable", "wave"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "value",
    values_drop_na = FALSE  # Keep NA if needed
  ) %>%
  mutate(
    wave = as.integer(wave),
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    names_from = "variable",
    values_from = "value"
  )

# Select only the non-time series variables for merging
data_stable_features <- data_w %>%
  select(mergeid, all_of(extra_f_stable))  # Keep only stable variables


# Merge long-format features by mergeid and wave
data_l <- data_l %>%
  left_join(data_l_new, by = c("mergeid", "wave")) %>%
  left_join(data_stable_features, by = "mergeid") 




##### Saving data
saveRDS(data_w, "data/sh.w.s.ext.rds")
saveRDS(data_l, "data/sh.l.s.ext.rds")

write.csv(data_w, "data/sh.w.s.ext.csv", row.names = FALSE)
write.csv(data_l, "data/sh.l.s.ext.csv", row.names = FALSE)
