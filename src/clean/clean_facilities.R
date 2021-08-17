# Dependencies
library(tidyverse)

# Import
load("data/nssats/NSSATS_2019.RData")
nssats_2019 <- PUF
load("data/nmhss/NMHSS_2019.RData")
nmhss_2019 <- PUF

# CLEAN N-SSATS -----------------------------------------------------------

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

# CLEAN N-MHSS ------------------------------------------------------------

# Clean N-MHSS 2019
mhss_2019 <- nmhss_2019 %>%
  # Rename most variables for easy identification
  rename(id = CASEID, state = LST, lgbt = SRVC62) %>%
  # Select relevant variables
  select(id, state, lgbt, everything())

# SAVE TO FILE ------------------------------------------------------------

# N-SSATS 2019
write_csv(ssats_2019, file = "data/cleaned/ssats_2019.csv")

# N-MHSS 2019
write_csv(mhss_2019, file = "data/cleaned/mhss_2019.csv")
