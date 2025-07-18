# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

# ========== Data Preparation: Demographic & Marital Status ==========
# mergeid: Person identifier (fix across modules and waves)
# country: Country identifier
# dn042_: Male or female
# dn003_: Year of birth
# dn002_: Month of birth
# dn014_: Marital status
# dn041_: Years education - Wave 1 does not have have this variable
# ------ later we can add household income so

# Loading files and Selecting variables
##### ------------------------------------------------
dn_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_dn.sav") %>%       # Wave 1 does not have dn041_
  mutate(wave = 0) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_)

dn_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_dn.sav") %>%
  mutate(wave = 2) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)

dn_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_dn.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)

dn_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_dn.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)

dn_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_dn.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)

dn_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_dn.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)

dn_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_dn.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)

dn_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_dn.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, country, dn042_, dn003_, dn002_, dn014_, dn041_)


##### Adding age in each Wave from table "gv_allwaves"
##### ------------------------------------------------
allwaves <- read_sav("data/rawdata/sharewX_rel9-0-0_gv_allwaves_cv_r.sav")

dn_0 <- dn_0 %>%
  left_join(allwaves %>% select(mergeid, age_int_w1) %>%
              rename(age_int_ = age_int_w1),               # Renamed to make it harmonious with other waves
            by = "mergeid")

dn_2 <- dn_2 %>%
  left_join(allwaves %>% select(mergeid, age_int_w2) %>%
              rename(age_int_ = age_int_w2),               # Renaming to make it harmonious with other waves
            by = "mergeid")

dn_6 <- dn_6 %>%
  left_join(allwaves %>% select(mergeid, age_int_w4) %>%
              rename(age_int_ = age_int_w4),               # Renaming to make it harmonious with other waves
            by = "mergeid")

dn_8 <- dn_8 %>%
  left_join(allwaves %>% select(mergeid, age_int_w5) %>%
              rename(age_int_ = age_int_w5),               # Renaming to make it harmonious with other waves
            by = "mergeid")

dn_10 <- dn_10 %>%
  left_join(allwaves %>% select(mergeid, age_int_w6) %>%
              rename(age_int_ = age_int_w6),               # Renaming to make it harmonious with other waves
            by = "mergeid")

dn_12 <- dn_12 %>%
  left_join(allwaves %>% select(mergeid, age_int_w7) %>%
              rename(age_int_ = age_int_w7),               # Renaming to make it harmonious with other waves
            by = "mergeid")

dn_14 <- dn_14 %>%
  left_join(allwaves %>% select(mergeid, age_int_w8) %>%
              rename(age_int_ = age_int_w8),               # Renaming to make it harmonious with other waves
            by = "mergeid")

dn_16 <- dn_16 %>%
  left_join(allwaves %>% select(mergeid, age_int_w9) %>%
              rename(age_int_ = age_int_w9),               # Renaming to make it harmonious with other waves
            by = "mergeid")


##### Managing values
##### ------------------------------------------------
dn_0 <- dn_0 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_))

dn_2 <- dn_2 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_
    ),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,     # Below zero: missing
      dn041_ > 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))

dn_6 <- dn_6 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,     # Below zero: missing
      dn041_ == 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))

dn_8 <- dn_8 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_
    ),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,     # Below zero: missing
      dn041_ == 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))

dn_10 <- dn_10 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,     # Below zero: missing
      dn041_ == 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))

dn_12 <- dn_12 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,   # Below zero: missing
      dn041_ == 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))

dn_14 <- dn_14 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,   # Below zero: missing
      dn041_ == 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))

dn_16 <- dn_16 %>%
  mutate(
    dn042_ = case_when(
      dn042_ < 0 ~ NA_real_,   # Below zero: missing
      dn042_ == 1 ~ 1,         # 1: male
      dn042_ == 2 ~ 2,         # 2: female
      TRUE ~ dn042_),
    dn042_ = factor(dn042_, levels = c(1, 2), labels = c("Male", "Female")),      # Convert to factor with appropriate labels after the re-coding
    dn003_ = case_when(
      dn003_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn003_),
    dn002_ = case_when(
      dn002_ < 0 ~ NA_real_,   # Below zero: missing
      TRUE ~ dn002_),
    dn014_ = case_when(
      dn014_ < 0 ~ NA_real_,   # Below zero: missing
      dn014_ == 1 ~ 1,         # 1: Married and living together with spouse
      dn014_ == 2 ~ 2,         # 2: Registered partnership
      dn014_ == 3 ~ 3,         # 3: Married, living separated from spouse
      dn014_ == 4 ~ 4,         # 4: Never married
      dn014_ == 5 ~ 5,         # 5: Divorced
      dn014_ == 6 ~ 6,         # 6: Widowed
      TRUE ~ dn014_),
    dn014_ = factor(                            # Convert marital status to factor with labels
      dn014_,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Married & living together", "Registered partnership",
        "Married, living separately", "Never married",
        "Divorced", "Widowed"
      )),  
    age_int_ = case_when(
      age_int_ < 0 ~ NA_real_,  # Below zero: missing
      TRUE ~ age_int_),
    dn041_ = case_when(
      dn041_ < 0 ~ NA_real_,   # Below zero: missing
      dn041_ == 9000 ~ NA_real_, # R never went to school
      dn041_ == 9997 ~ NA_real_, # Still in school/full-time education
      dn041_ > 9000 ~ NA_real_, # Unknown codes
      TRUE ~ dn041_))


##### Renaming variables for enhanced clarity and meaningful interpretation
##### ------------------------------------------------
# dn042_ -> dn_gender: Gender
# dn003_ -> dn_birthyear: Birth Year
# dn002_ -> dn_birthmonth: Birth Month
# dn014_ -> dn_marital: Marital Status
# dn041_ -> dn_eduyear: Years of Education
# age_int_ -> dn_age: Age

dn_0 <- dn_0 %>%            # Wave 1 does not have "dn041_" years of education
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age)

dn_2 <- dn_2 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)

dn_6 <- dn_6 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)

dn_8 <- dn_8 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)

dn_10 <- dn_10 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)

dn_12 <- dn_12 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)

dn_14 <- dn_14 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)

dn_16 <- dn_16 %>%
  rename(
    dn_gender = dn042_,
    dn_birthyear = dn003_,
    dn_birthmonth = dn002_,
    dn_marital = dn014_,
    dn_eduyear = dn041_,
    dn_age = age_int_
  ) %>%
  select(mergeid, wave, country, dn_gender, dn_birthyear, dn_birthmonth, dn_marital, dn_age, dn_eduyear)


##### Concatenating all wave data sets into one long-format data set
##### ------------------------------------------------
dn_bind <- bind_rows(dn_0, dn_2, dn_6, dn_8, dn_10, dn_12, dn_14, dn_16)

# Group by Static variables (country, dn_birthyear, dn_birthmonth, dn_gender) - These fields appear in data set just once (not for each wave)
dn_static <- dn_bind %>%
  group_by(mergeid) %>%
  arrange(wave) %>%
  slice(1) %>%                       # keep only the first row per person
  ungroup() %>%
  select(mergeid, country, dn_birthyear, dn_birthmonth, dn_gender)

# Pivot dynamic variables (dn014_ and dn041_)
dn_dynamic <- dn_bind %>%
  select(mergeid, wave, dn_age, dn_marital, dn_eduyear) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(dn_age, dn_marital, dn_eduyear),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_w1, dn014_w2...

# Creating DN table: Merge static and dynamic parts
DN <- left_join(dn_static, dn_dynamic, by = "mergeid")


##### Managing data types
##### ------------------------------------------------
# Converting other variables from double to factor
DN$dn_birthmonth <- haven::as_factor(DN$dn_birthmonth)

# Convert dn_age variables across waves to numeric
DN <- DN %>%
  mutate(
    dn_age_0 = as.numeric(dn_age_0),
    dn_age_2 = as.numeric(dn_age_2),
    dn_age_6 = as.numeric(dn_age_6),
    dn_age_8 = as.numeric(dn_age_8),
    dn_age_10 = as.numeric(dn_age_10),
    dn_age_12 = as.numeric(dn_age_12),
    dn_age_14 = as.numeric(dn_age_14),
    dn_age_16 = as.numeric(dn_age_16)
  )

# Convert dn_eduyear variables across waves to numeric
DN <- DN %>%
  mutate(
    dn_eduyear_0 = as.numeric(dn_eduyear_0),
    dn_eduyear_2 = as.numeric(dn_eduyear_2),
    dn_eduyear_6 = as.numeric(dn_eduyear_6),
    dn_eduyear_8 = as.numeric(dn_eduyear_8),
    dn_eduyear_10 = as.numeric(dn_eduyear_10),
    dn_eduyear_12 = as.numeric(dn_eduyear_12),
    dn_eduyear_14 = as.numeric(dn_eduyear_14),
    dn_eduyear_16 = as.numeric(dn_eduyear_16)
  )


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save the RDS file
saveRDS(DN, "data/DN.rds")
