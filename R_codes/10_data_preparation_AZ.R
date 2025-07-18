# ========== Data Preparation: Alzheimer's Disease ==========
# ph006d16: Alzheimer's disease, dementia, senility: ever diagnosed/currently having

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)



# Loading files and Selecting variables
# wave 1 does not have this data

az_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_ph.sav") %>%
  mutate(wave = 2) %>%
  select(mergeid, wave, ph006d16)

az_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_ph.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, ph006d16)

az_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_ph.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, ph006d16)

az_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_ph.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, ph006d16)

az_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_ph.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, ph006d16)

az_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_ph.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, ph006d16)

az_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_ph.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, ph006d16)


# Managing values
az_2 <- az_2 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

az_6 <- az_6 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

az_8 <- az_8 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

az_10 <- az_10 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

az_12 <- az_12 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

az_14 <- az_14 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

az_16 <- az_16 %>%
  mutate(
    ph006d16 = case_when(
      ph006d16 < 0 ~ NA_real_,   # Below zero: missing
      ph006d16 == 0 ~ 0,        # Not selected
      ph006d16 == 1 ~ 1,        # Selected
      TRUE ~ ph006d16
    ),
    ph006d16 = factor(ph006d16, 
                      levels = c(0, 1), 
                      labels = c("No Alzheimer", "Has Alzheimer"))  # Convert to factor
  )

# Renaming the variable 'ph006d16' to 'az_alzheimer' for enhanced clarity and meaningful interpretation
az_2 <- az_2 %>%
  rename(az_alzheimer = ph006d16)

az_6 <- az_6 %>%
  rename(az_alzheimer = ph006d16)

az_8 <- az_8 %>%
  rename(az_alzheimer = ph006d16)

az_10 <- az_10 %>%
  rename(az_alzheimer = ph006d16)

az_12 <- az_12 %>%
  rename(az_alzheimer = ph006d16)

az_14 <- az_14 %>%
  rename(az_alzheimer = ph006d16)

az_16 <- az_16 %>%
  rename(az_alzheimer = ph006d16)


# Preparing data set
# Creating long-format data set
az_bind <- bind_rows(az_2, az_6, az_8, az_10, az_12, az_14, az_16)

# Creating wide table SP
AZ <- az_bind %>%
  select(mergeid, wave, az_alzheimer) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(az_alzheimer),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_2...


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(AZ, "data/AZ.rds")


