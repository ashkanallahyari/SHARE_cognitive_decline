# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

# ========== Data Preparation: Isolation ==========
# Here social isolation is measured using the method mentioned by
# Shankar, A., McMunn, A., Banks, J., & Steptoe, A. (2011). Loneliness, social isolation, and behavioral and biological health indicators in older adults. Health psychology, 30(4), 377.
# Here 5 measures is used:
  # not living with a partner, 
  # not belonging to any organizations, clubs or religious, groups, 
  # having less than monthly contact with friends, 
  # having less than monthly contact with family 
  # having less than monthly contact with children. 


# 1) not living with a partner
# partner: Relationship status, info from waves 1,2,4
# partner_var: Relationship status wave 6, 8 and 9

# Loading files and Selecting variables
is_living_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, partner)

is_living_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, partner_var)

is_living_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, partner_var)

is_living_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_gv_networks.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, partner_var)


# Managing values
is_living_6 <- is_living_6 %>%
  mutate(
    partner = case_when(
      partner < 0 ~ NA_real_,              # Below 0: set as missing
      partner == 1 ~ 1,                    # 1: Partner in household, W4 info
      partner == 2 ~ 0,                    # 2: Partner outside of hh, W4 info
      partner == 3 ~ 0,                    # 3: Partner outside of hh, W2 info
      partner == 4 ~ 1,                    # 4: Married, liv. tog. w/ spouse, W4 info
      partner == 5 ~ 0,                    # 5: Registered partnership, W4 info
      partner == 6 ~ 1,                    # 6: Married, liv. tog. w/ spouse, W2 info
      partner == 7 ~ 0,                    # 7: Registered partnership, W2 info
      partner == 8 ~ 1,                    # 8: Married, liv. tog. w/ spouse, W1 info
      partner == 9 ~ 0,                    # 9: Registered partnership, W1 info
      partner == 10 ~ 0,                   # 10: Partner only in soc. network W4
      TRUE ~ partner                       # Keep other values
    )
  )

is_living_10 <- is_living_10 %>%
  mutate(
    partner_var = case_when(
      partner_var < 0 ~ NA_real_,              # Below 0: set as missing
      partner_var == 0 ~ 0,                    # 0: No partner in data
      partner_var == 1 ~ 1,                    # 1: Partner in household, W6 info
      partner_var == 2 ~ 0,                    # 2: Partner outside of hh, W6 info
      TRUE ~ partner_var                       # Keep other values
    )
  )

is_living_14 <- is_living_14 %>%
  mutate(
    partner_var = case_when(
      partner_var < 0 ~ NA_real_,              # Below 0: set as missing
      partner_var == 0 ~ 0,                    # 0: No partner in data
      partner_var == 1 ~ 1,                    # 1: Partner in household, W8 info
      partner_var == 2 ~ 0,                    # 2: Partner outside of hh, W8 info
      TRUE ~ partner_var                       # Keep other values
    )
  )

is_living_16 <- is_living_16 %>%
  mutate(
    partner_var = case_when(
      partner_var < 0 ~ NA_real_,              # Below 0: set as missing
      partner_var == 0 ~ 0,                    # 0: No partner in data
      partner_var == 1 ~ 1,                    # 1: Partner in household, W9 info
      partner_var == 2 ~ 0,                    # 2: Partner outside of hh, W9 info
      TRUE ~ partner_var                       # Keep other values
    )
  )


# Renaming the variable for enhanced clarity and meaningful interpretation
is_living_6 <- is_living_6 %>%
  rename(is_living_together = partner)

is_living_10 <- is_living_10 %>%
  rename(is_living_together = partner_var)

is_living_14 <- is_living_14 %>%
  rename(is_living_together = partner_var)

is_living_16 <- is_living_16 %>%
  rename(is_living_together = partner_var)



# 2) not belonging to any organizations, clubs or religious, groups, 
# ac002d1: Activities last month: voluntary or charity work
# ac002d4: Activities last month: attended educational or training course
# ac002d5: Activities last month: gone to sport, social or other kind of club
# ac002d6: Activities last month: taken part in religious organization
# ac002d7: Activities last month: taken part in political or community organization
# ac035d1: Activities in last year: done voluntary or charity work
# ac035d4: Activities in last year: attended an educational or training course
# ac035d5: Activities in last year: gone to a sport, social or other kind of club
# ac035d6: Activities in last year: taken part in activities of a religious organization
# ac035d7: Activities in last year: taken part in a political or community-related organization
# ac035d10: Activities in last year: played cards or games such as chess


# Loading files and Selecting variables
is_belong_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_ac.sav") %>%
  mutate(wave = 0,
         is_belong_period_flag = 0) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac002d1, ac002d4, ac002d5, ac002d6, ac002d7)

is_belong_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_ac.sav") %>%
  mutate(wave = 2,
         is_belong_period_flag = 0) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac002d1, ac002d4, ac002d5, ac002d6, ac002d7)

is_belong_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_ac.sav") %>%
  mutate(wave = 6,
         is_belong_period_flag = 1) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10)

is_belong_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_ac.sav") %>%
  mutate(wave = 8,
         is_belong_period_flag = 1) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10)

is_belong_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_ac.sav") %>%
  mutate(wave = 10,
         is_belong_period_flag = 1) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10)

is_belong_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_ac.sav") %>%
  mutate(wave = 12,
         is_belong_period_flag = 1) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10)

is_belong_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_ac.sav") %>%
  mutate(wave = 14,
         is_belong_period_flag = 1) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10)

is_belong_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_ac.sav") %>%
  mutate(wave = 16,
         is_belong_period_flag = 1) %>%         # In wave 1 and 2 they asnwered for the last month, other wave they answer last year
  select(mergeid, wave, is_belong_period_flag, ac035d1, ac035d4, ac035d5, ac035d7, ac035d10)


# In waves 1 & 2 activities in last month were asked, So the answers are recorded in variables with different names
# Here we rename them to align them with waves 4 onward

is_belong_0 <- is_belong_0 %>%
  rename(
    ac035d1 = ac002d1,
    ac035d4 = ac002d4,
    ac035d5 = ac002d5,
    ac035d6 = ac002d6,
    ac035d7 = ac002d7,
  )

is_belong_2 <- is_belong_2 %>%
  rename(
    ac035d1 = ac002d1,
    ac035d4 = ac002d4,
    ac035d5 = ac002d5,
    ac035d6 = ac002d6,
    ac035d7 = ac002d7,
  )

# Managing values
is_belong_0 <- is_belong_0 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )

is_belong_2 <- is_belong_2 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )

is_belong_6 <- is_belong_6 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )


is_belong_8 <- is_belong_8 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )

is_belong_10 <- is_belong_10 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )

is_belong_12 <- is_belong_12 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )

is_belong_14 <- is_belong_14 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )

is_belong_16 <- is_belong_16 %>%
  mutate(
    across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~case_when(
      . < 0 ~ NA_real_,
      . == 0 ~ 0,  # Not selected
      . == 1 ~ 1,  # Selected
      TRUE ~ .
    ))
  )


# Calculating 2) not belonging to any organizations, clubs or religious, groups, 
is_belong_0 <- is_belong_0 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_belong_2 <- is_belong_2 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_belong_6 <- is_belong_6 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_belong_8 <- is_belong_8 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d6, ac035d7, ac035d10), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )


is_belong_10 <- is_belong_10 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_belong_12 <- is_belong_12 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_belong_14 <- is_belong_14 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_belong_16 <- is_belong_16 %>%
  mutate(
    is_belong_any = case_when(
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ac035d1, ac035d4, ac035d5, ac035d7, ac035d10), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )



# 3) having less than monthly contact with friends
# sn005_X: Any more persons with whom you often discuss: sn person X  --> value 21 = friend
# sn007_X: Network contact: sn person X

##### Loading files and Selecting variables
is_friend_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_sn.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)

is_friend_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_sn.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)

is_friend_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_sn.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)

is_friend_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_sn.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)


# Keeping the values of contact if the person is a friend (value 21)
is_friend_6 <- is_friend_6 %>%
  mutate(
    sn007_1 = if_else(sn005_1 == 21, sn007_1, NA_real_),
    sn007_2 = if_else(sn005_2 == 21, sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 == 21, sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 == 21, sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 == 21, sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 == 21, sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 == 21, sn007_7, NA_real_)
  )

is_friend_10 <- is_friend_10 %>%
  mutate(
    sn007_1 = if_else(sn005_1 == 21, sn007_1, NA_real_),
    sn007_2 = if_else(sn005_2 == 21, sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 == 21, sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 == 21, sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 == 21, sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 == 21, sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 == 21, sn007_7, NA_real_)
  )

is_friend_14 <- is_friend_14 %>%
  mutate(
    sn007_1 = if_else(sn005_1 == 21, sn007_1, NA_real_),
    sn007_2 = if_else(sn005_2 == 21, sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 == 21, sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 == 21, sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 == 21, sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 == 21, sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 == 21, sn007_7, NA_real_)
  )

is_friend_16 <- is_friend_16 %>%
  mutate(
    sn007_1 = if_else(sn005_1 == 21, sn007_1, NA_real_),
    sn007_2 = if_else(sn005_2 == 21, sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 == 21, sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 == 21, sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 == 21, sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 == 21, sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 == 21, sn007_7, NA_real_)
  )


# Calculating a new variable 
# if they have relationship with at least one friend in a month or less
is_friend_6 <- is_friend_6 %>%
  mutate(
    is_friend_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_friend_10 <- is_friend_10 %>%
  mutate(
    is_friend_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_friend_14 <- is_friend_14 %>%
  mutate(
    is_friend_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_friend_16 <- is_friend_16 %>%
  mutate(
    is_friend_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )


# 4) having less than monthly contact with family
# sn005_X: Any more persons with whom you often discuss: sn person X  --> value 1:9, 11:20 = family and relatives (except for children)
# sn007_X: Network contact: sn person X

##### Loading files and Selecting variables
is_family_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_sn.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)

is_family_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_sn.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)

is_family_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_sn.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)

is_family_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_sn.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, sn005_1:sn005_7, sn007_1:sn007_7)


# Keeping the values of contact if the person is a friend (value 21)
is_family_6 <- is_family_6 %>%
  mutate(
    sn007_1 = if_else(sn005_1 %in% c(1:9, 11:20), sn007_1, NA_real_),   # All family relatives except for children ("Spouse/Partner", "Mother", "Father", "Mother-in-law", "Father-in-law", "Stepmother", "Stepfather", "Brother", "Sister", "Child", "Step-child/your current partner's child", "Son-in-law", "Daughter-in-law", "Grandchild", "Grandparent", "Aunt", "Uncle", "Niece", "Nephew", "Other relative")
    sn007_2 = if_else(sn005_2 %in% c(1:9, 11:20), sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 %in% c(1:9, 11:20), sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 %in% c(1:9, 11:20), sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 %in% c(1:9, 11:20), sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 %in% c(1:9, 11:20), sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 %in% c(1:9, 11:20), sn007_7, NA_real_)
  )

is_family_10 <- is_family_10 %>%
  mutate(
    sn007_1 = if_else(sn005_1 %in% c(1:9, 11:20), sn007_1, NA_real_),   # All family relatives except for children ("Spouse/Partner", "Mother", "Father", "Mother-in-law", "Father-in-law", "Stepmother", "Stepfather", "Brother", "Sister", "Child", "Step-child/your current partner's child", "Son-in-law", "Daughter-in-law", "Grandchild", "Grandparent", "Aunt", "Uncle", "Niece", "Nephew", "Other relative")
    sn007_2 = if_else(sn005_2 %in% c(1:9, 11:20), sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 %in% c(1:9, 11:20), sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 %in% c(1:9, 11:20), sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 %in% c(1:9, 11:20), sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 %in% c(1:9, 11:20), sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 %in% c(1:9, 11:20), sn007_7, NA_real_)
  )

is_family_14 <- is_family_14 %>%
  mutate(
    sn007_1 = if_else(sn005_1 %in% c(1:9, 11:20), sn007_1, NA_real_),   # All family relatives except for children ("Spouse/Partner", "Mother", "Father", "Mother-in-law", "Father-in-law", "Stepmother", "Stepfather", "Brother", "Sister", "Child", "Step-child/your current partner's child", "Son-in-law", "Daughter-in-law", "Grandchild", "Grandparent", "Aunt", "Uncle", "Niece", "Nephew", "Other relative")
    sn007_2 = if_else(sn005_2 %in% c(1:9, 11:20), sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 %in% c(1:9, 11:20), sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 %in% c(1:9, 11:20), sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 %in% c(1:9, 11:20), sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 %in% c(1:9, 11:20), sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 %in% c(1:9, 11:20), sn007_7, NA_real_)
  )

is_family_16 <- is_family_16 %>%
  mutate(
    sn007_1 = if_else(sn005_1 %in% c(1:9, 11:20), sn007_1, NA_real_),   # All family relatives except for children ("Spouse/Partner", "Mother", "Father", "Mother-in-law", "Father-in-law", "Stepmother", "Stepfather", "Brother", "Sister", "Child", "Step-child/your current partner's child", "Son-in-law", "Daughter-in-law", "Grandchild", "Grandparent", "Aunt", "Uncle", "Niece", "Nephew", "Other relative")
    sn007_2 = if_else(sn005_2 %in% c(1:9, 11:20), sn007_2, NA_real_),
    sn007_3 = if_else(sn005_3 %in% c(1:9, 11:20), sn007_3, NA_real_),
    sn007_4 = if_else(sn005_4 %in% c(1:9, 11:20), sn007_4, NA_real_),
    sn007_5 = if_else(sn005_5 %in% c(1:9, 11:20), sn007_5, NA_real_),
    sn007_6 = if_else(sn005_6 %in% c(1:9, 11:20), sn007_6, NA_real_),
    sn007_7 = if_else(sn005_7 %in% c(1:9, 11:20), sn007_7, NA_real_)
  )


# Calculating a new variable if the have relationship with at least one friend in a month or less
is_family_6 <- is_family_6 %>%
  mutate(
    is_family_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_family_10 <- is_family_10 %>%
  mutate(
    is_family_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    ))

is_family_14 <- is_family_14 %>%
  mutate(
    is_family_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    ))

is_family_16 <- is_family_16 %>%
  mutate(
    is_family_contact = case_when(
      rowSums(across(c(sn007_1:sn007_7), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(sn007_1:sn007_7), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    ))


# 5) having less than monthly contact with children
# ch014_X: Child X contact with child
# ch001_: number of children

##### Loading files and Selecting variables
is_child_0 <- read_sav("data/rawdata/sharew1_rel9-0-0_ch.sav") %>%     # in this wave the question asked up to 4 child
  mutate(wave = 0) %>%
  select(mergeid, wave, ch014_1:ch014_4, ch001_)

is_child_2 <- read_sav("data/rawdata/sharew2_rel9-0-0_ch.sav") %>%     # in this wave the question asked up to 4 child
  mutate(wave = 2) %>%
  select(mergeid, wave, ch014_1:ch014_4, ch001_)

is_child_6 <- read_sav("data/rawdata/sharew4_rel9-0-0_ch.sav") %>%
  mutate(wave = 6) %>%
  select(mergeid, wave, ch014_1:ch014_20, ch001_)

is_child_8 <- read_sav("data/rawdata/sharew5_rel9-0-0_ch.sav") %>%
  mutate(wave = 8) %>%
  select(mergeid, wave, ch014_1:ch014_20,ch001_)

is_child_10 <- read_sav("data/rawdata/sharew6_rel9-0-0_ch.sav") %>%
  mutate(wave = 10) %>%
  select(mergeid, wave, ch014_1:ch014_20, ch001_)

is_child_12 <- read_sav("data/rawdata/sharew7_rel9-0-0_ch.sav") %>%   # name of the variable is different
  mutate(wave = 12) %>%
  select(mergeid, wave, ch014_REG_1:ch014_REG_20, ch001_)

is_child_14 <- read_sav("data/rawdata/sharew8_rel9-0-0_ch.sav") %>%
  mutate(wave = 14) %>%
  select(mergeid, wave, ch014_1:ch014_20, ch001_)

is_child_16 <- read_sav("data/rawdata/sharew9_rel9-0-0_ch.sav") %>%
  mutate(wave = 16) %>%
  select(mergeid, wave, ch014_1:ch014_20, ch001_)


# Managing values
is_child_0 <- is_child_0 %>%
  mutate(
    across(c(ch014_1:ch014_4), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,         # Set < 0 to NA using case_when
      TRUE ~ ch001_
    )
  )

is_child_2 <- is_child_2 %>%
  mutate(
    across(c(ch014_1:ch014_4), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,         # Set < 0 to NA using case_when
      TRUE ~ ch001_
    )
  )

is_child_6 <- is_child_6 %>%
  mutate(
    across(c(ch014_1:ch014_20), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,         # Set < 0 to NA using case_when
      TRUE ~ ch001_
    )
  )

is_child_8 <- is_child_8 %>%
  mutate(
    across(c(ch014_1:ch014_20), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,
      TRUE ~ ch001_
    )
  )

is_child_10 <- is_child_10 %>%
  mutate(
    across(c(ch014_1:ch014_20), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,
      TRUE ~ ch001_
    )
  )

is_child_12 <- is_child_12 %>%
  mutate(
    across(c(ch014_REG_1:ch014_REG_20), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,
      TRUE ~ ch001_
    )
  )

is_child_14 <- is_child_14 %>%
  mutate(
    across(c(ch014_1:ch014_20), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,
      TRUE ~ ch001_
    )
  )

is_child_16 <- is_child_16 %>%
  mutate(
    across(c(ch014_1:ch014_20), ~case_when(
      . < 0 ~ NA_real_,              # Below 0: set as missing
      . == 1 ~ 1,                    # 1: Daily
      . == 2 ~ 2,                    # 2: Several times a week
      . == 3 ~ 3,                    # 3: About once a week
      . == 4 ~ 4,                    # 4: About every two weeks
      . == 5 ~ 5,                    # 5: About once a month
      . == 6 ~ 6,                    # 6: Less than once a month
      . == 7 ~ 7,                    # 7: Never
      TRUE ~ .                       # Keep other values
    )
  ),
    ch001_ = case_when(
      ch001_ < 0 ~ NA_real_,
      TRUE ~ ch001_
    )
  )

# Calculating a new variable if the have relationship with at least one child in a month or less
is_child_0 <- is_child_0 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If ch001_ is 0, set child_contact to 0
      rowSums(across(c(ch014_1:ch014_4), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_child_2 <- is_child_2 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If ch001_ is 0, set child_contact to 0
      rowSums(across(c(ch014_1:ch014_4), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_4), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )


is_child_6 <- is_child_6 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If no children, set contact to 0
      rowSums(across(c(ch014_1:ch014_20), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_child_8 <- is_child_8 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If no children, set contact to 0
      rowSums(across(c(ch014_1:ch014_20), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_child_10 <- is_child_10 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If no children, set contact to 0
      rowSums(across(c(ch014_1:ch014_20), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_child_12 <- is_child_12 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If no children, set contact to 0
      rowSums(across(c(ch014_REG_1:ch014_REG_20), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_REG_1:ch014_REG_20), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_REG_1:ch014_REG_20), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_REG_1:ch014_REG_20), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_REG_1:ch014_REG_20), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_REG_1:ch014_REG_20), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_child_14 <- is_child_14 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If no children, set contact to 0
      rowSums(across(c(ch014_1:ch014_20), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

is_child_16 <- is_child_16 %>%
  mutate(
    is_child_contact = case_when(
      ch001_ == 0 ~ 0,  # If no children, set contact to 0
      rowSums(across(c(ch014_1:ch014_20), ~ . == 1), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 2), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 3), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 4), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ . == 5), na.rm = TRUE) >= 1 ~ 1,
      rowSums(across(c(ch014_1:ch014_20), ~ !is.na(.))) == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

# Calculating the Social Isolation score
# Here we collected all 5 aspects of the social isolation
# Then we created a new variable for Social isolation score

is_0 <- is_belong_0 %>%                          # The variables "is_living, is_family and is_friend" are not provided in this wave.
  full_join(is_child_0 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_belong_any, is_belong_period_flag, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_belong_any, is_child_contact), na.rm = TRUE))

is_2 <- is_belong_2 %>%                          # The variables "is_living, is_family and is_fried" are not provided in this wave.
  full_join(is_child_2 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_belong_any, is_belong_period_flag, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_belong_any, is_child_contact), na.rm = TRUE))

is_6 <- is_belong_6 %>%
  full_join(is_living_6 %>% select(mergeid, is_living_together), by = "mergeid") %>%
  full_join(is_friend_6 %>% select(mergeid, is_friend_contact), by = "mergeid") %>%
  full_join(is_family_6 %>% select(mergeid, is_family_contact), by = "mergeid") %>%
  full_join(is_child_6 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_living_together, is_belong_any, is_friend_contact, is_family_contact, is_child_contact), na.rm = TRUE))

is_8 <- is_belong_8 %>%                          # The variables "is_living, is_family and is_frien" are not provided in this wave.
  full_join(is_child_8 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_belong_any, is_belong_period_flag, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_belong_any, is_belong_period_flag, is_child_contact), na.rm = TRUE))

is_10 <- is_belong_10 %>%
  full_join(is_living_10 %>% select(mergeid, is_living_together), by = "mergeid") %>%
  full_join(is_friend_10 %>% select(mergeid, is_friend_contact), by = "mergeid") %>%
  full_join(is_family_10 %>% select(mergeid, is_family_contact), by = "mergeid") %>%
  full_join(is_child_10 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact), na.rm = TRUE))

is_12 <- is_belong_12 %>%                          # The variables "is_living, is_family and is_frien" are not provided in this wave.
  full_join(is_child_12 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_belong_any, is_belong_period_flag, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_belong_any, is_belong_period_flag, is_child_contact), na.rm = TRUE))

is_14 <- is_belong_14 %>%
  full_join(is_living_14 %>% select(mergeid, is_living_together), by = "mergeid") %>%
  full_join(is_friend_14 %>% select(mergeid, is_friend_contact), by = "mergeid") %>%
  full_join(is_family_14 %>% select(mergeid, is_family_contact), by = "mergeid") %>%
  full_join(is_child_14 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact), na.rm = TRUE))

is_16 <- is_belong_16 %>%
  full_join(is_living_16 %>% select(mergeid, is_living_together), by = "mergeid") %>%
  full_join(is_friend_16 %>% select(mergeid, is_friend_contact), by = "mergeid") %>%
  full_join(is_family_16 %>% select(mergeid, is_family_contact), by = "mergeid") %>%
  full_join(is_child_16 %>% select(mergeid, is_child_contact), by = "mergeid") %>%
  select(mergeid, wave, is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact) %>%
  mutate(is_score = rowMeans(select(., is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact), na.rm = TRUE))



# Preparing data set
is_bind <- bind_rows(is_0, is_2, is_6, is_8, is_10, is_12, is_14, is_16)

# Reordering the columns
is_bind <- is_bind %>%
  select(mergeid, wave, is_living_together, is_belong_any, is_belong_period_flag, is_friend_contact, is_family_contact, is_child_contact, is_score)

# Creating wide table SP
IS <- is_bind %>%
  select(everything()) %>%
  pivot_wider(
    id_cols = mergeid,
    names_from = wave,
    values_from = c(-mergeid, -wave),
    names_glue = "{.value}_{wave}") %>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.), NA_real_, .)))


# Check if the folder exists; create it if not
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save file
saveRDS(IS, "data/IS.rds")


