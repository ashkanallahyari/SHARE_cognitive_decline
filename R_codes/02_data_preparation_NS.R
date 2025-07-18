# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

# ========== Data Preparation: Network Size ==========
# mergeid: Person identifier (fix across modules and waves)
# sn_size_wX: SN size wave X
# sizeofsocialnetwork: Sec_SN. Size of social network    # For wave 4


##### Loading files and Selecting variables
##### ------------------------------------------------
# SHARE network data was gathred just in waves 4, 6, 8 and 9
ns_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sizeofsocialnetwork)

ns_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sn_size_w6)

ns_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sn_size_w8)

ns_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sn_size_w9)

##### Managing values
##### ------------------------------------------------
# ns_6 does not need any mutation in values 

ns_10 <- ns_10 %>%
  mutate(
    sn_size_w6 = case_when(
      sn_size_w6 == -9 ~ NA_real_,
      TRUE ~ sn_size_w6
    )
  )

ns_14 <- ns_14 %>%
  mutate(
    sn_size_w8 = case_when(
      sn_size_w8 == -9 ~ NA_real_,
      TRUE ~ sn_size_w8
    )
  )

ns_16 <- ns_16 %>%
  mutate(
    sn_size_w9 = case_when(
      sn_size_w9 == -9 ~ NA_real_,
      TRUE ~ sn_size_w9
    )
  )

##### Renaming the variables for enhanced clarity and meaningful interpretation
##### ------------------------------------------------
ns_6 <- ns_6 %>%
  rename(ns_netsize = sizeofsocialnetwork)

ns_10 <- ns_10 %>%
  rename(ns_netsize = sn_size_w6)

ns_14 <- ns_14 %>%
  rename(ns_netsize = sn_size_w8)

ns_16 <- ns_16 %>%
  rename(ns_netsize = sn_size_w9)


##### Preparing data set
##### ------------------------------------------------
# Creating long-format data set
ns_bind <- bind_rows(ns_6, ns_10, ns_14, ns_16)

# Creating wide table SP, only z-scores
NS <- ns_bind %>%
  select(mergeid, wave, ns_netsize) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(ns_netsize),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_w2...


##### Managing data types
##### ------------------------------------------------
# Convert ns_netsize variables across waves to numeric
NS <- NS %>%
  mutate(
    ns_netsize_6 = as.numeric(ns_netsize_6),
    ns_netsize_10 = as.numeric(ns_netsize_10),
    ns_netsize_14 = as.numeric(ns_netsize_14),
    ns_netsize_16 = as.numeric(ns_netsize_16)
  )


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(NS, "data/NS.rds")


