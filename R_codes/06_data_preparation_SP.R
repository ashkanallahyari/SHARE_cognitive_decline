# ========== Data Preparation: Social Support ==========
# mergeid: Person identifier (fix across modules and waves)
# sp002_: Received help from others (outside hh)
# sp005_1: How often received help: from person 1
# sp005_2: How often received help: from person 2
# sp005_3: How often received help: from person 3


# Load required libraries
library(haven)
library(dplyr)
library(tidyr)


# Loading files and Selecting variables
sp_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_sp.sav") %>%
  mutate(wave = 0) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_sp.sav") %>%
  mutate(wave = 2) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_sp.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_sp.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_sp.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_sp.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_sp.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)

sp_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_sp.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sp002_, sp005_1, sp005_2, sp005_3)


# Managing values
sp_0 <- sp_0 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,  # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_2 <- sp_2 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,   # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_6 <- sp_6 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,   # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_8 <- sp_8 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0, # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_10 <- sp_10 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,  # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_12 <- sp_12 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,  # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_14 <- sp_14 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,  # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

sp_16 <- sp_16 %>%
  mutate(
    across(c(sp002_), ~case_when(
      . < 0 ~ NA_real_,
      . == 1 ~ 1,  # yes
      . == 5 ~ 0,  # no
      TRUE ~ .
    )),
    across(c(sp005_1, sp005_2, sp005_3), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 3,                    # 1: About daily
      . == 2 ~ 2,                    # 2: About every week
      . == 3 ~ 1,                    # 3: About every month
      . == 4 ~ 0,                    # 4: Less often
      TRUE ~ .                       # Keep other values
    ))
  )

# Calculating the receiving social support
sp_0 <- sp_0 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_2 <- sp_2 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_6 <- sp_6 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_8 <- sp_8 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_10 <- sp_10 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_12 <- sp_12 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_14 <- sp_14 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))

sp_16 <- sp_16 %>%
  mutate(sp_social_support = rowSums(select(., sp005_1, sp005_2, sp005_3), na.rm = TRUE))


# Preparing data set
# Creating long-format data set
sp_bind <- bind_rows(sp_0, sp_2, sp_6, sp_8, sp_10, sp_12, sp_14, sp_16)

# Creating wide table SP
SP <- sp_bind %>%
  select(mergeid, wave, sp_social_support) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(sp_social_support),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_0, dn014_2...


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}
# Save file
saveRDS(SP, "data/SP.rds")

