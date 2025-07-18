# ========== Data Preparation: Daily activities ==========
# ph0049dXX: Daily activities

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)


# Loading files and Selecting variables
da_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_ph.sav") %>%
  mutate(wave = 0) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_ph.sav") %>%
  mutate(wave = 2) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_ph.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_ph.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_ph.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_ph.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_ph.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, ph049d1:ph049d13)

da_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_ph.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, ph049d1:ph049d13)


# Managing values
da_0 <- da_0 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))


da_2 <- da_2 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))

da_6 <- da_6 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))

da_8 <- da_8 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))

da_10 <- da_10 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))

da_12 <- da_12 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))

da_14 <- da_14 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))

da_16 <- da_16 %>%
  mutate(across(ph049d1:ph049d13, ~ if_else(. < 0, NA_real_, .)))


# calculating daily function score
da_0 <- da_0 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_2 <- da_2 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_6 <- da_6 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_8 <- da_8 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_10 <- da_10 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_12 <- da_12 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_14 <- da_14 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))

da_16 <- da_16 %>%
  mutate(dfun = rowSums(select(., ph049d1:ph049d13) == 0, na.rm = TRUE))


# Preparing data set
# Creating long-format data set
da_bind <- bind_rows(da_0, da_2, da_6, da_8, da_10, da_12, da_14, da_16)

# Creating wide table SP
DA <- da_bind %>%
  select(mergeid, wave, dfun) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(dfun),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_2...


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(DA, "data/DA.rds")

