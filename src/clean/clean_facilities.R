# Dependencies
library(tidyverse)

# Import N-SSATS
nssats_2020 <- read_delim("data/nssats/NSSATS_2020.txt", delim = " ", col_names = FALSE) %>%
  select("X1", "X10")
load("data/nssats/NSSATS_2019.RData")
nssats_2019 <- PUF

# Import N-MHSS
load("data/nmhss/NMHSS_2019.RData")
nmhss_2019 <- PUF

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# CLEAN N-SSATS -----------------------------------------------------------

#* N-SSATS 2020 -----------------------------------------------------------

# Vector of names
names_2020 <- c("region", "lgbtq_total")

# Rename
names(nssats_2020) <- names_2020

# Clean N-SSATS 2020
nssats_2020a <- nssats_2020 %>%
  mutate(
    # Removed underscore
    region = str_replace(region, "_", " "),
    # Transform to lower case
    region = tolower(region)
  ) %>%
  right_join(state_names, by = "region") %>%
  select(region, state, lgbtq_total)
nssats_2020a

#* N-SSATS 2019 -----------------------------------------------------------

# Clean N-SSATS 2019
ssats_2019 <- nssats_2019 %>%
  # Rename most variables for easy identification
  rename(id = CASEID, state = STATE, lgbt = SRVC62, owner = OWNERSHP, sliding_fee = FEESCALE,
         pay_no_low = PAYASST, pay_cash = REVCHK1, pay_private_ins = REVCHK2, pay_free = REVCHK3, 
         pay_medicaid = REVCHK5, pay_medicare = REVCHK8, pay_state_ins = REVCHK10, 
         pay_military_ins = REVCHK15, pay_itu = REVCHK17, accredited = ACCRED,
         supp_women = SRVC34, supp_men = SRVC64, supp_cooccur = SRVC31, supp_hiv = SRVC32,
         supp_sexabuse = SRVC121, supp_ipv = SRVC122, supp_trauma = SRVC116,
         num_assessment = ASSESSMENT, num_test = TESTING, num_medical = MEDICAL,
         num_transition = TRANSITION, num_recovery = RECOVERY, num_education = EDUCATION,
         num_ancillary = ANCILLARY, num_psychopharm = PHARMACOTHERAPIES, residential = CTYPE7,
         outpatient = CTYPE1) %>%
  # Select relevant variables
  select(id, state, lgbt, owner, sliding_fee, accredited, residential, outpatient,
         starts_with("supp_"), starts_with("num")) %>%
  # Recode values
  mutate(
    owner = recode(owner, `1` = "Private, for-profit", `2` = "Private, non-profit", `3` = "State government",
                   `4` = "Local government", `5` = "Tribal government", `6` = "State government"),
    across(sliding_fee:supp_trauma, ~ factor(., levels = c(0, 1), labels = c("No", "Yes")))
  )
ssats_2019

# Transform 
nssats_2019a <- ssats_2019 %>%
  select(state, lgbt) %>%
  count(state, lgbt) %>%
  filter(lgbt == 1) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, everything())
nssats_2019a

# CLEAN N-MHSS ------------------------------------------------------------

# Clean N-MHSS 2019
mhss_2019 <- nmhss_2019 %>%
  # Rename most variables for easy identification
  rename(id = CASEID, state = LST, lgbt = SRVC62) %>%
  # Select relevant variables
  select(id, state, lgbt, everything())

# SAVE TO FILE ------------------------------------------------------------

# For original descriptive stats
write_csv(ssats_2019, file = "data/cleaned/ssats_2019.csv")
write_csv(mhss_2019, file = "data/cleaned/mhss_2019.csv")

# Save new files for structural stigma analysis