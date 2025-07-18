# ========== Data Preparation: Cognitive Functioning ==========
# cf008tot: Immediate recall in wave 1 & 2 -> only one set of words was used 
# cf016tot: Delayed recall in wave 1 & 2 -> only one set of words was used 
# cf104tot, cf105tot, cf106tot, cf107tot: Delayed recall in wave 1 & 2 -> Participants randomly assigned to one out of four word lists
# cf113tot, cf114tot, cf115tot, cf116tot: Delayed recall in wave 1 & 2 -> Participants randomly assigned to one out of four word lists
# cf010_: Verbal fluency score: number of animals

# The cognitive decline is calucated based on the following reference:
# Bourassa, K. J., Memel, M., Woolverton, C., & Sbarra, D. A. (2015). A dyadic approach to health, cognition, and quality of life in aging adults. Psychology and aging, 30(2), 449.

# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

# Loading files and Selecting variables
cf_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_cf.sav") %>%
  mutate(wave = 0) %>%
  select(mergeid, wave, cf008tot, cf016tot, cf010_)

cf_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_cf.sav") %>%
  mutate(wave = 2) %>%
  select(mergeid, wave, cf008tot, cf016tot, cf010_)

cf_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_cf.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, cf104tot, cf105tot, cf106tot, cf107tot, cf113tot, cf114tot, cf115tot, cf116tot, cf010_)

cf_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_cf.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, cf104tot, cf105tot, cf106tot, cf107tot, cf113tot, cf114tot, cf115tot, cf116tot, cf010_)

cf_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_cf.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, cf104tot, cf105tot, cf106tot, cf107tot, cf113tot, cf114tot, cf115tot, cf116tot, cf010_)

cf_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_cf.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, cf104tot, cf105tot, cf106tot, cf107tot, cf113tot, cf114tot, cf115tot, cf116tot, cf010_)

cf_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_cf.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, cf104tot, cf105tot, cf106tot, cf107tot, cf113tot, cf114tot, cf115tot, cf116tot, cf010_)

cf_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_cf.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, cf104tot, cf105tot, cf106tot, cf107tot, cf113tot, cf114tot, cf115tot, cf116tot, cf010_)


# Creating single unified immediate recall and delayed recall for wave 4 to 9
# As in these waves, participants assigned randomly to one out of four word lists.
# Also the variable names of wave 1 and 2 renamed to make them unified with other waves

cf_0 <- cf_0 %>%
  rename(
    cf_imm_recall = cf008tot,
    cf_dly_recall = cf016tot
  )

cf_2 <- cf_2 %>%
  rename(
    cf_imm_recall = cf008tot,
    cf_dly_recall = cf016tot
  )

cf_6 <- cf_6 %>%
  mutate(
    cf_imm_recall = coalesce(cf104tot, cf105tot, cf106tot, cf107tot),
    cf_dly_recall = coalesce(cf113tot, cf114tot, cf115tot, cf116tot)
  )

cf_8 <- cf_8 %>%
  mutate(
    cf_imm_recall = coalesce(cf104tot, cf105tot, cf106tot, cf107tot),
    cf_dly_recall = coalesce(cf113tot, cf114tot, cf115tot, cf116tot)
  )

cf_10 <- cf_10 %>%
  mutate(
    cf_imm_recall = coalesce(cf104tot, cf105tot, cf106tot, cf107tot),
    cf_dly_recall = coalesce(cf113tot, cf114tot, cf115tot, cf116tot)
  )

cf_12 <- cf_12 %>%
  mutate(
    cf_imm_recall = coalesce(cf104tot, cf105tot, cf106tot, cf107tot),
    cf_dly_recall = coalesce(cf113tot, cf114tot, cf115tot, cf116tot)
  )

cf_14 <- cf_14 %>%
  mutate(
    cf_imm_recall = coalesce(cf104tot, cf105tot, cf106tot, cf107tot),
    cf_dly_recall = coalesce(cf113tot, cf114tot, cf115tot, cf116tot)
  )

cf_16 <- cf_16 %>%
  mutate(
    cf_imm_recall = coalesce(cf104tot, cf105tot, cf106tot, cf107tot),
    cf_dly_recall = coalesce(cf113tot, cf114tot, cf115tot, cf116tot)
  )

# Renaming the variable 'cf010_' to 'cf_verbal' for enhanced clarity and meaningful interpretation
cf_0 <- cf_0 %>%
  rename(cf_verbal = cf010_)

cf_2 <- cf_2 %>%
  rename(cf_verbal = cf010_)

cf_6 <- cf_6 %>%
  rename(cf_verbal = cf010_)

cf_8 <- cf_8 %>%
  rename(cf_verbal = cf010_)

cf_10 <- cf_10 %>%
  rename(cf_verbal = cf010_)

cf_12 <- cf_12 %>%
  rename(cf_verbal = cf010_)

cf_14 <- cf_14 %>%
  rename(cf_verbal = cf010_)

cf_16 <- cf_16 %>%
  rename(cf_verbal = cf010_)


# Calculating z-score
cf_0 <- cf_0 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_2 <- cf_2 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_6 <- cf_6 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_8 <- cf_8 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_10 <- cf_10 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_12 <- cf_12 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_14 <- cf_14 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )

cf_16 <- cf_16 %>%
  mutate(
    cf_verbal_z = (cf_verbal - mean(cf_verbal, na.rm = TRUE)) / sd(cf_verbal, na.rm = TRUE),
    cf_imm_recall_z = (cf_imm_recall - mean(cf_imm_recall, na.rm = TRUE)) / sd(cf_imm_recall, na.rm = TRUE),
    cf_dly_recall_z = (cf_dly_recall - mean(cf_dly_recall, na.rm = TRUE)) / sd(cf_dly_recall, na.rm = TRUE),
  )


# Calculating the social participant
# Arithmetic means base on the reference paper

cf_0 <- cf_0 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_2 <- cf_2 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_6 <- cf_6 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_8 <- cf_8 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_10 <- cf_10 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_12 <- cf_12 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_14 <- cf_14 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))

cf_16 <- cf_16 %>%
  mutate(cf_score = rowMeans(select(., cf_verbal_z, cf_imm_recall_z, cf_dly_recall_z), na.rm = TRUE))


# Preparing data set
# Creating long-format data set
cf_bind <- bind_rows(cf_0, cf_2, cf_6, cf_8, cf_10, cf_12, cf_14, cf_16)

# Creating wide table SP
CF <- cf_bind %>%
  select(mergeid, wave, cf_imm_recall, cf_dly_recall, cf_verbal, cf_imm_recall_z, cf_dly_recall_z, cf_verbal_z, cf_score) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(cf_imm_recall, cf_dly_recall, cf_verbal, cf_imm_recall_z, cf_dly_recall_z, cf_verbal_z, cf_score),
    names_glue = "{.value}_{wave}") %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.), NA_real_, .)))



# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(CF, "data/CF.rds")

