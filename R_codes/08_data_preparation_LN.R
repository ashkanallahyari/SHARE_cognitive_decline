# ========== Data Preparation: Loneliness ==========
# mergeid: Person identifier (fix across modules and waves)
# mh037_: Feels lonely
# The variable has been measured from wave 5 onward

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)


# Loading files and Selecting variables
ln_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_mh.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, mh037_)

ln_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_mh.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, mh037_)

ln_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_mh.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, mh037_)

ln_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_mh.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, mh037_)

ln_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_mh.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, mh037_)


# Managing values
ln_8 <- ln_8 %>%
  mutate(
    across(c(mh037_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # Often
      . == 2 ~ 2,   # Some of the time
      . == 3 ~ 3,   # Hardly ever or never
      TRUE ~ .
    ))
  )

ln_10 <- ln_10 %>%
  mutate(
    across(c(mh037_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Often
      . == 2 ~ 2,   # Some of the time
      . == 3 ~ 3,   # Hardly ever or never
      TRUE ~ .
    ))
  )

ln_12 <- ln_12 %>%
  mutate(
    across(c(mh037_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Often
      . == 2 ~ 2,   # Some of the time
      . == 3 ~ 3,   # Hardly ever or never
      TRUE ~ .
    ))
  )

ln_14 <- ln_14 %>%
  mutate(
    across(c(mh037_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Often
      . == 2 ~ 2,   # Some of the time
      . == 3 ~ 3,   # Hardly ever or never
      TRUE ~ .
    ))
  )

ln_16 <- ln_16 %>%
  mutate(
    across(c(mh037_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,   # Often
      . == 2 ~ 2,   # Some of the time
      . == 3 ~ 3,   # Hardly ever or never
      TRUE ~ .
    ))
  )


# Renaming the variables for enhanced clarity and meaningful interpretation
ln_8 <- ln_8 %>%
  rename(ln_loneliness = mh037_)

ln_10 <- ln_10 %>%
  rename(ln_loneliness = mh037_)

ln_12 <- ln_12 %>%
  rename(ln_loneliness = mh037_)

ln_14 <- ln_14 %>%
  rename(ln_loneliness = mh037_)

ln_16 <- ln_16 %>%
  rename(ln_loneliness = mh037_)


# Preparing data set
ln_bind <- bind_rows(ln_8, ln_10, ln_12, ln_14, ln_16)

# Creating wide table SP
LN <- ln_bind %>%
  select(mergeid, wave, ln_loneliness) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(ln_loneliness),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_w2...


# Managing data types
# Remove labels from all columns
LN <- LN %>%
  mutate(across(everything(), zap_labels))


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(LN, "data/LN.rds")



