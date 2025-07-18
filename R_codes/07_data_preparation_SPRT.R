# ========== Data Preparation: Social Participation ==========
# ac035d1: Activities in last year: done voluntary or charity work 
# ac035d4: Activities in last year: attended an educational or training course
# ac035d5: Activities in last year: gone to a sport, social or other kind of club
# ac035d6: Activities in last year: taken part in activities of a religious organization                 * Wave 6 does not have this variable
# ac035d7: Activities in last year: taken part in a political or community-related organization
# ac035d10: Activities in last year: played cards or games such as chess
# ac035dno: Activities in last year: none of these

# ac036_1: How often done voluntary/charity work the last 12 months
# ac036_4: How often attended an educational or training course the last 12 months
# ac036_5: How often gone to a sport/social/other kind of club the last 12 months
# ac036_7: How often taken part in a political/community-related organization the last 12 m
# ac036_10: How often played cards or games such as chess the last 12 months

# Other measured activities are excluded as follows, since they are not social activities: 
# Read books, magazines or newspapers
# Did word or number games (crossword puzzles/Sudoku...)
# Participating in activities of a religious organization in last year (ac035d6 and ac036_6) has not measured in wave 6 onward. Therefore, this variable is excluded.
# The time frame in waves 1 and 2 is 'last month,' whereas in other waves it is 'last year.' Therefore, these two waves are excluded.
# ac002d1, ac002d4, ac002d5, ac003_6, ac002d7, ac002dno
# ac003_1, ac003_4, ac003_5, ac003_6, ac003_7


# Load required libraries
library(haven)
library(dplyr)
library(tidyr)


# Loading files and Selecting variables
sprt_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_ac.sav") %>%
  mutate(wave = 0) %>%
  select(mergeid, wave, ac002d1, ac002d4, ac002d5, ac002d7, ac002dno, ac003_1, ac003_4, ac003_5, ac003_7)

sprt_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_ac.sav") %>%
  mutate(wave = 2) %>%
  select(mergeid, wave, ac002d1, ac002d4, ac002d5, ac002d7, ac002dno, ac003_1, ac003_4, ac003_5, ac003_7)

sprt_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_ac.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno, ac036_1, ac036_4, ac036_5, ac036_7, ac036_10)

sprt_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_ac.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno, ac036_1, ac036_4, ac036_5, ac036_7, ac036_10)

sprt_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_ac.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno, ac036_1, ac036_4, ac036_5, ac036_7, ac036_10)

sprt_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_ac.sav") %>%
  mutate(wave = 12) %>%
  select(mergeid, wave, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno, ac036_1, ac036_4, ac036_5, ac036_7, ac036_10)

sprt_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_ac.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno, ac036_1, ac036_4, ac036_5, ac036_7, ac036_10)

sprt_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_ac.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno, ac036_1, ac036_4, ac036_5, ac036_7, ac036_10)


# In waves 1 & 2 activities in last month were asked, So the answers are recorded in variables with different names
# Here we rename them to align them with waves 4 onward


sprt_0 <- sprt_0 %>%
  rename(
    ac035d1 = ac002d1,
    ac035d4 = ac002d4,
    ac035d5 = ac002d5,
    ac035d7 = ac002d7,
    ac035dno = ac002dno,
    ac036_1 = ac003_1,
    ac036_4 = ac003_4,
    ac036_5 = ac003_5,
    ac036_7 = ac003_7
  )

sprt_2 <- sprt_2 %>%
  rename(
    ac035d1 = ac002d1,
    ac035d4 = ac002d4,
    ac035d5 = ac002d5,
    ac035d7 = ac002d7,
    ac035dno = ac002dno,
    ac036_1 = ac003_1,
    ac036_4 = ac003_4,
    ac036_5 = ac003_5,
    ac036_7 = ac003_7
  )


# Managing values
sprt_0 <- sprt_0 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 4,                    # 3: Less often -> this wave has 3 levels, we changed the code to harmonize it with other waves
      TRUE ~ .
    ))
  )

sprt_2 <- sprt_2 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 4,                    # 3: Less often -> this wave has 3 levels, we changed the code to harmonize it with other waves
      TRUE ~ .
    ))
  )

sprt_6 <- sprt_6 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7, ac036_10), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 3,                    # 3: Almost every month
      . == 4 ~ 4,                    # 4: Less often
      TRUE ~ .
    ))
  )

sprt_8 <- sprt_8 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7, ac036_10), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 3,                    # 3: Almost every month
      . == 4 ~ 4,                    # 4: Less often
      TRUE ~ .
    ))
  )

sprt_10 <- sprt_10 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7, ac036_10), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 3,                    # 3: Almost every month
      . == 4 ~ 4,                    # 4: Less often
      TRUE ~ .
    ))
  )

sprt_12 <- sprt_12 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7, ac036_10), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 3,                    # 3: Almost every month
      . == 4 ~ 4,                    # 4: Less often
      TRUE ~ .
    ))
  )

sprt_14 <- sprt_14 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7, ac036_10), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 3,                    # 3: Almost every month
      . == 4 ~ 4,                    # 4: Less often
      TRUE ~ .
    ))
  )

sprt_16 <- sprt_16 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10, ac035dno), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    )),
    across(c(ac036_1, ac036_4, ac036_5, ac036_7, ac036_10), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Almost every day
      . == 2 ~ 2,                    # 2: Almost every week
      . == 3 ~ 3,                    # 3: Almost every month
      . == 4 ~ 4,                    # 4: Less often
      TRUE ~ .
    ))
  )


# Calculating the social participant
# Social participation was calculated based on the methodology from the paper:
# Santini, Z. I., Jose, P. E., Koyanagi, A., Meilstrup, C., Nielsen, L., Madsen, K. R., & Koushede, V. (2020). Formal social participation protects physical health through enhanced mental health: A longitudinal mediation analysis using three consecutive waves of the Survey of Health, Ageing and Retirement in Europe (SHARE). Social Science & Medicine, 251, 112906.

# A scale for the frequency of formal social participation was created as follows:
# 4 = active almost daily in at least one activity
# 3 = active almost every week in at least one activity
# 2 = active almost every month in at least one activity
# 1 = active less than monthly in at least one activity
# 0 = no formal social participation

sprt_0 <- sprt_0 %>%    # The variable in this wave has 3 levels
  mutate(
    sprt_social_participation = case_when(
      rowSums(select(., ac036_1, ac036_4, ac036_5, ac036_7) == 1, na.rm = TRUE) >= 1 ~ 4,  # Almost daily
      rowSums(select(., ac036_1, ac036_4, ac036_5, ac036_7) == 2, na.rm = TRUE) >= 1 ~ 3,  # Almost every week
      rowSums(select(., ac036_1, ac036_4, ac036_5, ac036_7) == 4, na.rm = TRUE) >= 1 ~ 1,  # Less than monthly
      TRUE ~ 0  # No participation or only missing values
    ))

sprt_2 <- sprt_2 %>%    # The variable in this wave has 3 levels
  mutate(
    sprt_social_participation = case_when(
      rowSums(select(., ac036_1, ac036_4, ac036_5, ac036_7) == 1, na.rm = TRUE) >= 1 ~ 4,  # Almost daily
      rowSums(select(., ac036_1, ac036_4, ac036_5, ac036_7) == 2, na.rm = TRUE) >= 1 ~ 3,  # Almost every week
      rowSums(select(., ac036_1, ac036_4, ac036_5, ac036_7) == 4, na.rm = TRUE) >= 1 ~ 1,  # Less than monthly
      TRUE ~ 0  # No participation or only missing values
    ))

sprt_6 <- sprt_6 %>%
  mutate(
    sprt_social_participation = case_when(
      ac036_1 == 1 | ac036_4 == 1 | ac036_5 == 1 | ac036_7 == 1 | ac036_10 == 1 ~ 4,
      ac036_1 == 2 | ac036_4 == 2 | ac036_5 == 2 | ac036_7 == 2 | ac036_10 == 2 ~ 3,
      ac036_1 == 3 | ac036_4 == 3 | ac036_5 == 3 | ac036_7 == 3 | ac036_10 == 3 ~ 2,
      ac036_1 == 4 | ac036_4 == 4 | ac036_5 == 4 | ac036_7 == 4 | ac036_10 == 4 ~ 1,
      TRUE ~ 0
  ))

sprt_8 <- sprt_8 %>%
  mutate(
    sprt_social_participation = case_when(
      ac036_1 == 1 | ac036_4 == 1 | ac036_5 == 1 | ac036_7 == 1 | ac036_10 == 1 ~ 4,
      ac036_1 == 2 | ac036_4 == 2 | ac036_5 == 2 | ac036_7 == 2 | ac036_10 == 2 ~ 3,
      ac036_1 == 3 | ac036_4 == 3 | ac036_5 == 3 | ac036_7 == 3 | ac036_10 == 3 ~ 2,
      ac036_1 == 4 | ac036_4 == 4 | ac036_5 == 4 | ac036_7 == 4 | ac036_10 == 4 ~ 1,
      TRUE ~ 0
  ))

sprt_10 <- sprt_10 %>%
  mutate(
    sprt_social_participation = case_when(
      ac036_1 == 1 | ac036_4 == 1 | ac036_5 == 1 | ac036_7 == 1 | ac036_10 == 1 ~ 4,
      ac036_1 == 2 | ac036_4 == 2 | ac036_5 == 2 | ac036_7 == 2 | ac036_10 == 2 ~ 3,
      ac036_1 == 3 | ac036_4 == 3 | ac036_5 == 3 | ac036_7 == 3 | ac036_10 == 3 ~ 2,
      ac036_1 == 4 | ac036_4 == 4 | ac036_5 == 4 | ac036_7 == 4 | ac036_10 == 4 ~ 1,
      TRUE ~ 0
  ))

sprt_12 <- sprt_12 %>%
  mutate(
    sprt_social_participation = case_when(
      ac036_1 == 1 | ac036_4 == 1 | ac036_5 == 1 | ac036_7 == 1 | ac036_10 == 1 ~ 4,
      ac036_1 == 2 | ac036_4 == 2 | ac036_5 == 2 | ac036_7 == 2 | ac036_10 == 2 ~ 3,
      ac036_1 == 3 | ac036_4 == 3 | ac036_5 == 3 | ac036_7 == 3 | ac036_10 == 3 ~ 2,
      ac036_1 == 4 | ac036_4 == 4 | ac036_5 == 4 | ac036_7 == 4 | ac036_10 == 4 ~ 1,
      TRUE ~ 0
  ))

sprt_14 <- sprt_14 %>%
  mutate(
    sprt_social_participation = case_when(
      ac036_1 == 1 | ac036_4 == 1 | ac036_5 == 1 | ac036_7 == 1 | ac036_10 == 1 ~ 4,
      ac036_1 == 2 | ac036_4 == 2 | ac036_5 == 2 | ac036_7 == 2 | ac036_10 == 2 ~ 3,
      ac036_1 == 3 | ac036_4 == 3 | ac036_5 == 3 | ac036_7 == 3 | ac036_10 == 3 ~ 2,
      ac036_1 == 4 | ac036_4 == 4 | ac036_5 == 4 | ac036_7 == 4 | ac036_10 == 4 ~ 1,
      TRUE ~ 0
  ))

sprt_16 <- sprt_16 %>%
  mutate(
    sprt_social_participation = case_when(
      ac036_1 == 1 | ac036_4 == 1 | ac036_5 == 1 | ac036_7 == 1 | ac036_10 == 1 ~ 4,
      ac036_1 == 2 | ac036_4 == 2 | ac036_5 == 2 | ac036_7 == 2 | ac036_10 == 2 ~ 3,
      ac036_1 == 3 | ac036_4 == 3 | ac036_5 == 3 | ac036_7 == 3 | ac036_10 == 3 ~ 2,
      ac036_1 == 4 | ac036_4 == 4 | ac036_5 == 4 | ac036_7 == 4 | ac036_10 == 4 ~ 1,
      TRUE ~ 0
  ))


# Preparing data set
# Creating long-format data set
sprt_bind <- bind_rows(sprt_0, sprt_2, sprt_6, sprt_8, sprt_10, sprt_12, sprt_14, sprt_16)

# Creating wide table SP
SPRT <- sprt_bind %>%
  select(mergeid, wave, sprt_social_participation) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(sprt_social_participation),
    names_glue = "{.value}_{wave}")       # rename columns like dn014_0, dn014_2...


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(SPRT, "data/SPRT.rds")

