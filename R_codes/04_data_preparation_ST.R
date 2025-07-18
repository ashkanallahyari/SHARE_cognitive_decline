# ========== Data Preparation: Network Satisfaction ==========
# sn012_: Network satisfaction
# Data is available for wave 4, 6, 8, 9

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)


# Loading files and Selecting variables
st_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_sn.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sn012_)

st_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_sn.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sn012_)

st_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_sn.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sn012_)

st_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_sn.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sn012_)


# Managing values
st_6 <- st_6 %>%
  mutate(
    sn012_ = case_when(
      sn012_ < 0 ~ NA_real_,              # Below 0: set as missing
      TRUE ~ sn012_                       # Keep other values
    )
  )

st_10 <- st_10 %>%
  mutate(
    sn012_ = case_when(
      sn012_ < 0 ~ NA_real_,              # Below 0: set as missing
      TRUE ~ sn012_                       # Keep other values
    )
  )

st_14 <- st_14 %>%
  mutate(
    sn012_ = case_when(
      sn012_ < 0 ~ NA_real_,              # Below 0: set as missing
      TRUE ~ sn012_                       # Keep other values
    )
  )

st_16 <- st_16 %>%
  mutate(
    sn012_ = case_when(
      sn012_ < 0 ~ NA_real_,              # Below 0: set as missing
      TRUE ~ sn012_                       # Keep other values
    )
  )


# Renaming the variable for enhanced clarity and meaningful interpretation
st_6 <- st_6 %>%
  rename(ST_satisfaction = sn012_)

st_10 <- st_10 %>%
  rename(ST_satisfaction = sn012_)

st_14 <- st_14 %>%
  rename(ST_satisfaction = sn012_)

st_16 <- st_16 %>%
  rename(ST_satisfaction = sn012_)


# Preparing data set
# Creating long-format data set
st_bind <- bind_rows(st_6, st_10, st_14, st_16)

# Creating wide table SP
ST <- st_bind %>%
  select(mergeid, wave, ST_satisfaction) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(ST_satisfaction),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_w2...


# Managing data types
# Convert ST_satisfaction variables across waves to numeric in the ST table
ST <- ST %>%
  mutate(
    ST_satisfaction_6 = as.numeric(ST_satisfaction_6),
    ST_satisfaction_10 = as.numeric(ST_satisfaction_10),
    ST_satisfaction_14 = as.numeric(ST_satisfaction_14),
    ST_satisfaction_16 = as.numeric(ST_satisfaction_16)
  )


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(ST, "data/ST.rds")

