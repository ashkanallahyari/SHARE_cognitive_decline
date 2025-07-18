# ========== Data Preparation: Social Engagement ==========
# To measure this variable we used this reference:
# Bourassa, K. J., Memel, M., Woolverton, C., & Sbarra, D. A. (2017). Social participation predicts cognitive functioning in aging adults over time: comparisons with physical health, depression, and physical activity. Aging & mental health, 21(2), 133-146.
# main of z-score of the following variables:
# - social network size
# - social tie satisfaction
# - social closeness 
# - social contact frequency

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

# Loading filtered_ids for filtering irrelevant samples
#filtered_ids <- readRDS("data/filtered_ids.rds")
NS <- readRDS("data/NS.rds")
ST <- readRDS("data/ST.rds")


# Variable 1) social network size
# NS is prepared in NS data Frame Previously 
network_size_6 <- NS %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, ns_netsize_6) %>%
  rename(ns_netsize = ns_netsize_6)

network_size_10 <- NS %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, ns_netsize_10) %>%
  rename(ns_netsize = ns_netsize_10)

network_size_14 <- NS %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, ns_netsize_14) %>%
  rename(ns_netsize = ns_netsize_14)

network_size_16 <- NS %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, ns_netsize_16) %>%
  rename(ns_netsize = ns_netsize_16)


# calculating z-score
network_size_6 <- network_size_6 %>%
  mutate(z_netsize = (ns_netsize - mean(ns_netsize, na.rm = TRUE)) / sd(ns_netsize, na.rm = TRUE))

# For Wave 6
network_size_10 <- network_size_10 %>%
  mutate(z_netsize = (ns_netsize - mean(ns_netsize, na.rm = TRUE)) / sd(ns_netsize, na.rm = TRUE))

# For Wave 8
network_size_14 <- network_size_14 %>%
  mutate(z_netsize = (ns_netsize - mean(ns_netsize, na.rm = TRUE)) / sd(ns_netsize, na.rm = TRUE))

# For Wave 9
network_size_16 <- network_size_16 %>%
  mutate(z_netsize = (ns_netsize - mean(ns_netsize, na.rm = TRUE)) / sd(ns_netsize, na.rm = TRUE))


# Creating long-format data set
network_size <- bind_rows(network_size_6, network_size_10, network_size_14, network_size_16)


# Creating wide table SP, only z-scores
network_size <- network_size %>%
  select(mergeid, wave, z_netsize) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(z_netsize),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_w2...


# Variable 2) Social Tie Satisfaction
# Satisfaction data is prepared in ST data frame previously 

# Step 1: Create wave-specific data
satisfaction_6 <- ST %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, ST_satisfaction_6) %>%
  rename(satisfaction = ST_satisfaction_6)

satisfaction_10 <- ST %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, ST_satisfaction_10) %>%
  rename(satisfaction = ST_satisfaction_10)

satisfaction_14 <- ST %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, ST_satisfaction_14) %>%
  rename(satisfaction = ST_satisfaction_14)

satisfaction_16 <- ST %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, ST_satisfaction_16) %>%
  rename(satisfaction = ST_satisfaction_16)


# Step 2: Calculate z-scores for satisfaction
satisfaction_6 <- satisfaction_6 %>%
  mutate(z_satisfaction = (satisfaction - mean(satisfaction, na.rm = TRUE)) / sd(satisfaction, na.rm = TRUE))

satisfaction_10 <- satisfaction_10 %>%
  mutate(z_satisfaction = (satisfaction - mean(satisfaction, na.rm = TRUE)) / sd(satisfaction, na.rm = TRUE))

satisfaction_14 <- satisfaction_14 %>%
  mutate(z_satisfaction = (satisfaction - mean(satisfaction, na.rm = TRUE)) / sd(satisfaction, na.rm = TRUE))

satisfaction_16 <- satisfaction_16 %>%
  mutate(z_satisfaction = (satisfaction - mean(satisfaction, na.rm = TRUE)) / sd(satisfaction, na.rm = TRUE))

# Step 3: Combine into long-format
satisfaction <- bind_rows(satisfaction_6, satisfaction_10, satisfaction_14, satisfaction_16)

# Step 5: Create wide-format table with z-scores only
satisfaction <- satisfaction %>%
  select(mergeid, wave, z_satisfaction) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = z_satisfaction,
    names_glue = "{.value}_{wave}"        # rename columns like dn014_w1, dn014_w2...
  )


# Variable 3) social closeness
# sn009_X: Network closeness: sn person X

# Loading data
closeness_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_sn.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sn009_1:sn009_7)

closeness_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_sn.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sn009_1:sn009_7)

closeness_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_sn.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sn009_1:sn009_7)

closeness_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_sn.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sn009_1:sn009_7)


# Managing values
closeness_6 <- closeness_6 %>%
  mutate(
    across(c(sn009_1:sn009_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Not very close
      . == 2 ~ 2,   # Somewhat close
      . == 3 ~ 3,   # Very close
      . == 4 ~ 4,   # Extremely close
      TRUE ~ .
    ))
  )

closeness_10 <- closeness_10 %>%
  mutate(
    across(c(sn009_1:sn009_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Not very close
      . == 2 ~ 2,   # Somewhat close
      . == 3 ~ 3,   # Very close
      . == 4 ~ 4,   # Extremely close
      TRUE ~ .
    ))
  )

closeness_14 <- closeness_14 %>%
  mutate(
    across(c(sn009_1:sn009_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Not very close
      . == 2 ~ 2,   # Somewhat close
      . == 3 ~ 3,   # Very close
      . == 4 ~ 4,   # Extremely close
      TRUE ~ .
    ))
  )

closeness_16 <- closeness_16 %>%
  mutate(
    across(c(sn009_1:sn009_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Not very close
      . == 2 ~ 2,   # Somewhat close
      . == 3 ~ 3,   # Very close
      . == 4 ~ 4,   # Extremely close
      TRUE ~ .
    ))
  )


# Calculating the mean of closeness
closeness_6 <- closeness_6 %>%
  mutate(closeness = rowMeans(select(., sn009_1:sn009_7), na.rm = TRUE))

closeness_10 <- closeness_10 %>%
  mutate(closeness = rowMeans(select(., sn009_1:sn009_7), na.rm = TRUE))

closeness_14 <- closeness_14 %>%
  mutate(closeness = rowMeans(select(., sn009_1:sn009_7), na.rm = TRUE))

closeness_16 <- closeness_16 %>%
  mutate(closeness = rowMeans(select(., sn009_1:sn009_7), na.rm = TRUE))


# Step 2: Calculate z-scores
closeness_6 <- closeness_6 %>%
  mutate(z_closeness = (closeness - mean(closeness, na.rm = TRUE)) / sd(closeness, na.rm = TRUE))

closeness_10 <- closeness_10 %>%
  mutate(z_closeness = (closeness - mean(closeness, na.rm = TRUE)) / sd(closeness, na.rm = TRUE))

closeness_14 <- closeness_14 %>%
  mutate(z_closeness = (closeness - mean(closeness, na.rm = TRUE)) / sd(closeness, na.rm = TRUE))

closeness_16 <- closeness_16 %>%
  mutate(z_closeness = (closeness - mean(closeness, na.rm = TRUE)) / sd(closeness, na.rm = TRUE))


# Creating long-format data set for social tie satisfaction
closeness <- bind_rows(closeness_6, closeness_10, closeness_14, closeness_16)


# Step 3: Create wide-format table with z-scores only
closeness <- closeness %>%
  select(mergeid, wave, z_closeness) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = z_closeness,
    names_glue = "{.value}_{wave}"        # rename columns like dn014_w1, dn014_w2...
  )


# Variable 4) social contact frequency
# sn007_X: NetwNetwork contact: sn person X

# Loading data
contact_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_sn.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sn007_1:sn007_7)

contact_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_sn.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sn007_1:sn007_7)

contact_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_sn.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sn007_1:sn007_7)

contact_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_sn.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sn007_1:sn007_7)


# Managing values
contact_6 <- contact_6 %>%
  mutate(
    across(c(sn007_1:sn007_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 7,   # Daily
      . == 2 ~ 6,   # Several times a week
      . == 3 ~ 5,   # About once a week
      . == 4 ~ 4,   # About every two weeks
      . == 5 ~ 3,   # About once a month
      . == 6 ~ 2,   # Less than once a month
      . == 7 ~ 1,   # Never
      TRUE ~ .
    ))
  )

contact_10 <- contact_10 %>%
  mutate(
    across(c(sn007_1:sn007_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 7,   # Daily
      . == 2 ~ 6,   # Several times a week
      . == 3 ~ 5,   # About once a week
      . == 4 ~ 4,   # About every two weeks
      . == 5 ~ 3,   # About once a month
      . == 6 ~ 2,   # Less than once a month
      . == 7 ~ 1,   # Never
      TRUE ~ .
    ))
  )

contact_14 <- contact_14 %>%
  mutate(
    across(c(sn007_1:sn007_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 7,   # Daily
      . == 2 ~ 6,   # Several times a week
      . == 3 ~ 5,   # About once a week
      . == 4 ~ 4,   # About every two weeks
      . == 5 ~ 3,   # About once a month
      . == 6 ~ 2,   # Less than once a month
      . == 7 ~ 1,   # Never
      TRUE ~ .
    ))
  )

contact_16 <- contact_16 %>%
  mutate(
    across(c(sn007_1:sn007_7), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 7,   # Daily
      . == 2 ~ 6,   # Several times a week
      . == 3 ~ 5,   # About once a week
      . == 4 ~ 4,   # About every two weeks
      . == 5 ~ 3,   # About once a month
      . == 6 ~ 2,   # Less than once a month
      . == 7 ~ 1,   # Never
      TRUE ~ .
    ))
  )


# Calculating the mean of social contact
contact_6 <- contact_6 %>%
  mutate(contact = rowMeans(select(., sn007_1:sn007_7), na.rm = TRUE))

contact_10 <- contact_10 %>%
  mutate(contact = rowMeans(select(., sn007_1:sn007_7), na.rm = TRUE))

contact_14 <- contact_14 %>%
  mutate(contact = rowMeans(select(., sn007_1:sn007_7), na.rm = TRUE))

contact_16 <- contact_16 %>%
  mutate(contact = rowMeans(select(., sn007_1:sn007_7), na.rm = TRUE))


# Step 2: Calculate z-scores
contact_6 <- contact_6 %>%
  mutate(z_contact = (contact - mean(contact, na.rm = TRUE)) / sd(contact, na.rm = TRUE))

contact_10 <- contact_10 %>%
  mutate(z_contact = (contact - mean(contact, na.rm = TRUE)) / sd(contact, na.rm = TRUE))

contact_14 <- contact_14 %>%
  mutate(z_contact = (contact - mean(contact, na.rm = TRUE)) / sd(contact, na.rm = TRUE))

contact_16 <- contact_16 %>%
  mutate(z_contact = (contact - mean(contact, na.rm = TRUE)) / sd(contact, na.rm = TRUE))


# Creating long-format data set for social tie satisfaction
contact <- bind_rows(contact_6, contact_10, contact_14, contact_16)


# Step 3: Create wide-format table with z-scores only
contact <- contact %>%
  select(mergeid, wave, z_contact) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = z_contact,
    names_glue = "{.value}_{wave}"        # rename columns like dn014_w1, dn014_w2...
  )


# Preparing Social Engagement variable 
# Joining data
SE <- network_size %>%
  full_join(satisfaction, by = "mergeid") %>%
  full_join(closeness, by = "mergeid") %>%
  full_join(contact, by = "mergeid")

# calculating the mean
SE <- SE %>%
  mutate(
    se_social_engagement_6 = rowMeans(select(., z_netsize_6, z_satisfaction_6, z_closeness_6, z_contact_6), na.rm = TRUE),
    se_social_engagement_10 = rowMeans(select(., z_netsize_10, z_satisfaction_10, z_closeness_10, z_contact_10), na.rm = TRUE),
    se_social_engagement_14 = rowMeans(select(., z_netsize_14, z_satisfaction_14, z_closeness_14, z_contact_14), na.rm = TRUE),
    se_social_engagement_16 = rowMeans(select(., z_netsize_16, z_satisfaction_16, z_closeness_16, z_contact_16), na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.), NA_real_, .)))


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(SE, "data/SE.rds")

