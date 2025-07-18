# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(dplyr)
library(tidyr)
library(zoo)
library(purrr)


# ========== Data Preparation: Creating the main dataset ==========
#                                                         | W1 | W2 | W4 | W5 | W6 | W7 | W8 | W9 |
#                                                          =======================================
# DN: Demographic                                         | x  | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................
# NS: Network Size                                        |    |    | x  |    | x  |    | x  | x  |
# .................................................................................................
# IS: Social Isolation                                    | +  | +  | x  | +  | x  | +  | x  | x  |
    # 1) living with a partner                            |    |    | x  |    | x  |    | x  | x  |
    # 2) belonging to org or groups                       | x  | x  | x  | x  | x  | x  | x  | x  |
    # 3) contact with friends                             |    |    | x  |    | x  |    | x  | x  |
    # 4) contact with family                              |    |    | x  |    | x  |    | x  | x  |
    # 5) contact with children                            | x  | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................
# ST: Satisfaction with social ties                       |    |    | x  |    | x  |    | x  | x  |
# .................................................................................................
# SE: Social Engagement                                   |    |    | x  |    | x  |    | x  | x  |
# .................................................................................................
# SP: Receiving Social Support                            | x  | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................
# SPRT: Social Participation                              | x  | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................
# LN: Loneliness-higher score means less loneliness       |    |    |    | x  | x  | x  | x  | x  |
# .................................................................................................
# DA: Daily activities score                              | x  | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................
# CF: Cognitive Functioning                               | x  | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................
# AZ: Alzheimer's Disease Reported                        |    | x  | x  | x  | x  | x  | x  | x  |
# .................................................................................................




##### Loading tables and merging them with all variables
##### ------------------------------------------------
# Demographic & Marital Status
DN <- readRDS("data/DN.rds")
DN <- DN[, c(
  "mergeid", "country", "dn_birthyear", "dn_gender",
  "dn_age_0", "dn_age_2", "dn_age_6", "dn_age_8", "dn_age_10",
  "dn_age_12", "dn_age_14", "dn_age_16",
  "dn_eduyear_0", "dn_eduyear_2", "dn_eduyear_6", "dn_eduyear_8",
  "dn_eduyear_10", "dn_eduyear_12", "dn_eduyear_14", "dn_eduyear_16",
  "dn_marital_0", "dn_marital_2", "dn_marital_6", "dn_marital_8",
  "dn_marital_10", "dn_marital_12", "dn_marital_14", "dn_marital_16"
)]

# Social Network size
NS <- readRDS("data/NS.rds")

# Social Isolation
IS <- readRDS("data/IS.rds")
IS <- IS[, c(
  "mergeid", "is_score_0", "is_score_2", "is_score_6",
  "is_score_8", "is_score_10", "is_score_12", "is_score_14", "is_score_16"
)]

# Social Satisfaction
ST <- readRDS("data/ST.rds")

# Social Engagement
SE <- readRDS("data/SE.rds")
SE <- SE[, c(
  "mergeid",
  "se_social_engagement_6", "se_social_engagement_10", "se_social_engagement_14", "se_social_engagement_16"
)]

# Receiving social support
SP <- readRDS("data/SP.rds")

# Social Participation
SPRT <- readRDS("data/SPRT.rds")

# Loneliness
LN <- readRDS("data/LN.rds")

# Cognitive Functioning
CF <- readRDS("data/CF.rds")
CF <- CF[, c(
  "mergeid",
  "cf_score_0", "cf_score_2", "cf_score_6", "cf_score_8", "cf_score_10", "cf_score_12", "cf_score_14", "cf_score_16"
)]

# Alzheimer's
AZ <- readRDS("data/AZ.rds")

# Daily activities
DA <- readRDS("data/DA.rds")


##### Merging them with all variables
##### ------------------------------------------------
# Start merging datasets
data_w_full <- DN %>%
  full_join(NS, by = "mergeid") %>%
  full_join(IS, by = "mergeid") %>%
  full_join(LN, by = "mergeid") %>%
  full_join(SPRT, by = "mergeid") %>%
  full_join(ST, by = "mergeid") %>%
  full_join(SP, by = "mergeid") %>%
  full_join(SE, by = "mergeid") %>%
  full_join(DA, by = "mergeid") %>%
  full_join(CF, by = "mergeid") %>%
  full_join(AZ, by = "mergeid")




# ========== Providing data set with Rolling Bases ==========
# Creating long version of data
# -----------------------------
non_time_vars <- c("mergeid", "country", "dn_birthyear", "dn_gender")
factor_vars <- c(
  "dn_marital_0", "dn_marital_2", "dn_marital_6", "dn_marital_8", "dn_marital_10", "dn_marital_12", "dn_marital_14", "dn_marital_16",
  "az_alzheimer_2", "az_alzheimer_6", "az_alzheimer_8", "az_alzheimer_10", "az_alzheimer_12", "az_alzheimer_14", "az_alzheimer_16"
)
time_series_vars <- setdiff(names(data_w_full), c(non_time_vars, factor_vars))


# Pivot time series variables to long format
data_l_full <- data_w_full %>%
  pivot_longer(
    cols = all_of(time_series_vars),
    names_to = c("variable", "wave"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "value",
    values_drop_na = FALSE  # keep NA if needed
  ) %>%
  mutate(
    wave = as.integer(wave),
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    names_from = "variable",
    values_from = "value"
  )


# Handling factor variables as we cannot pivot them simultaneously with other variables
data_l_full <- data_l_full %>%
  mutate(
    dn_marital = case_when(
      wave == 0 ~ dn_marital_0,
      wave == 2 ~ dn_marital_2,
      wave == 6 ~ dn_marital_6,
      wave == 8 ~ dn_marital_8,
      wave == 10 ~ dn_marital_10,
      wave == 12 ~ dn_marital_12,
      wave == 14 ~ dn_marital_14,
      wave == 16 ~ dn_marital_16
    )
  )

data_l_full <- data_l_full %>%
  mutate(
    az_alzheimer = case_when(
      wave == 2 ~ az_alzheimer_2,
      wave == 6 ~ az_alzheimer_6,
      wave == 8 ~ az_alzheimer_8,
      wave == 10 ~ az_alzheimer_10,
      wave == 12 ~ az_alzheimer_12,
      wave == 14 ~ az_alzheimer_14,
      wave == 16 ~ az_alzheimer_16
    )
  )


# Dropping factor variables
data_l_full <- data_l_full %>%
  select(-all_of(factor_vars))

data_l_full <- data_l_full %>%               # Relocating fields for more readability of the table
  relocate(c("dn_age", "dn_eduyear", "dn_marital", "az_alzheimer"), .after = dn_gender)


# Determine the Baseline Per Participant
baseline_wave <- data_l_full %>%
  group_by(mergeid) %>%
  filter(!is.na(cf_score)) %>%
  summarize(baseline_wave = min(as.numeric(wave)))



# Merge Baseline Back and Filter for Post-Baseline Data
# Merge baseline_wave back into the long data set
data_l_full <- data_l_full %>%
  left_join(baseline_wave, by = "mergeid")


data_l_full <- data_l_full %>%
  mutate(
    wave = as.numeric(wave),
    baseline_wave = as.numeric(baseline_wave),
    time = wave - baseline_wave         # Relative time since baseline
  ) %>%
  filter(time >= 0) %>%                    # Keep only waves from baseline onward
  relocate(time, .after = wave) %>%
  select(-baseline_wave)


# Save main_data to an RDS file
saveRDS(data_l_full, "data/data_l_full.rds")
write.csv(data_l_full, "data/data_l_full.csv", row.names = FALSE)




# Updating the  wide version table
# ---------------------------------
# Variables to pivot back to wide format
vars_to_widen <- c(
  "dn_age", "dn_eduyear", "ns_netsize", "is_score", "ln_loneliness",
  "sprt_social_participation", "ST_satisfaction", "sp_social_support",
  "se_social_engagement", "dfun", "cf_score", "dn_marital", "az_alzheimer"
)

stable_vars <- c("mergeid", "country", "dn_birthyear", "dn_gender")


# Merge them back stable variables
data_w_full <- data_l_full %>%
  select(all_of(stable_vars)) %>%
  distinct() %>%
  left_join(
    data_l_full %>%
      select(mergeid, time, all_of(vars_to_widen)) %>%
      pivot_wider(
        names_from = time,
        values_from = all_of(vars_to_widen),
        names_glue = "{.value}_{time}"
      ),
    by = "mergeid"
  )


# Cleaning and ordering the dataset
# ---------------------------------
# Drop unwanted dn_age_* columns
drop_age_cols <- c("dn_age_2", "dn_age_4", "dn_age_6", "dn_age_8", "dn_age_10", 
                   "dn_age_12", "dn_age_14", "dn_age_16")

data_w_full <- data_w_full %>% select(-all_of(drop_age_cols))

data_w_full <- data_w_full %>%
  relocate(dn_eduyear_4, .after = dn_eduyear_2) %>%
  relocate(ns_netsize_4, .after = ns_netsize_2) %>%
  relocate(is_score_4, .after = is_score_2) %>%
  relocate(ln_loneliness_4, .after = ln_loneliness_2) %>%
  relocate(sprt_social_participation_4, .after = sprt_social_participation_2) %>%
  relocate(ST_satisfaction_4, .after = ST_satisfaction_2) %>%
  relocate(sp_social_support_4, .after = sp_social_support_2) %>%
  relocate(se_social_engagement_4, .after = se_social_engagement_2) %>%
  relocate(cf_score_4, .after = cf_score_2) %>%
  relocate(dn_marital_4, .after = dn_marital_2) %>%
  relocate(az_alzheimer_4, .after = az_alzheimer_2)


# Save main_data to an RDS file
saveRDS(data_w_full, "data/data_w_full.rds")
write.csv(data_w_full, "data/data_w_full.csv", row.names = FALSE)



# ========== Primary filtering ==========
# Filtering the dataset and keeping only mergeid
data_filtered <- data_w_full %>%
  filter(
    dn_age_0 > 60,  # Age at time 0 higher than 60
    az_alzheimer_0 == "No Alzheimer",  # No Alzheimer at base time 0
    rowSums(!is.na(select(., cf_score_0:cf_score_14))) >= 3 # Participated in at least 3 waves
  ) %>%
  select(mergeid)  # Keep only mergeid column



# Filtering data_w_full based on mergeids in data_filtered
sh.w.s <- data_w_full %>%
  filter(mergeid %in% data_filtered$mergeid)  # Keep only rows with mergeids in data_filtered


# Filtering data_l_full based on mergeids in data_filtered
sh.l.s.ext <- data_l_full %>%
  filter(mergeid %in% data_filtered$mergeid)  # Keep only rows with mergeids in data_filtered





##### Normalizing the values
# ---------------------------
# Define columns to exclude from normalization
cols_to_normalize <- c(
  "dn_eduyear", "ns_netsize", "is_score", "ln_loneliness", "sprt_social_participation",
  "ST_satisfaction", "sp_social_support", "se_social_engagement", "cf_score"
)

# Apply Z-Score Normalization (Standardization)
sh.l.s.ext <- sh.l.s.ext %>%
  mutate(across(all_of(cols_to_normalize), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

# Rescaling the new variables to mean 100 and sd 15
sh.l.s.ext <- sh.l.s.ext %>%
  mutate(across(all_of(cols_to_normalize), ~ 100 + 15 * (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

# Save main_data to an RDS file
saveRDS(sh.l.s.ext, "data/sh.l.s.ext.rds")
write.csv(sh.l.s.ext, "data/sh.l.s.ext.csv", row.names = FALSE)



##### Updating the wide table
# ---------------------------
# Define the fields to pivot (excluding static variables)
fields_to_pivot <- c("wave", "dn_eduyear", "ns_netsize", "is_score", "ln_loneliness", 
                     "sprt_social_participation", "ST_satisfaction", 
                     "sp_social_support", "se_social_engagement", "dfun", "cf_score")


# Pivot the table to long format, extracting variable name and wave number
sh.w.s.ext <- sh.l.s.ext %>%
  select(mergeid, time, all_of(fields_to_pivot)) %>%
  pivot_wider(names_from = time, values_from = all_of(fields_to_pivot))


# Adding Alheimer variables to the wide table (till wave 8)
az_vars <- c(
  "az_alzheimer_0", "az_alzheimer_2", "az_alzheimer_4", "az_alzheimer_6", "az_alzheimer_8",
  "az_alzheimer_10", "az_alzheimer_12", "az_alzheimer_14"
)



# Removing NaN values in cogn columns
# Identify numeric columns (sh.w.s.ext)
numeric_cols <- names(sh.w.s.ext)[sapply(sh.w.s.ext, is.numeric)]

# Replace NaN values with NA in all numeric columns
sh.w.s.ext[numeric_cols] <- lapply(sh.w.s.ext[numeric_cols], function(col) { 
  col[is.nan(col)] <- NA
  return(col)
})



# Identify numeric columns (sh.l.s.ext)
numeric_cols <- names(sh.l.s.ext)[sapply(sh.l.s.ext, is.numeric)]

# Replace NaN values with NA in all numeric columns (sh.l.s.ext)
sh.l.s.ext[numeric_cols] <- lapply(sh.l.s.ext[numeric_cols], function(col) { 
  col[is.nan(col)] <- NA
  return(col)
})


# Save main_data to an RDS file
saveRDS(sh.w.s.ext, "data/sh.w.s.ext.rds")
write.csv(sh.w.s.ext, "data/sh.w.s.ext.csv", row.names = FALSE)

