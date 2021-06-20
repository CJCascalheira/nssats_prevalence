# Dependencies
library(tidyverse)

# Import
load("data/NSSATS_PUF_2019_R.RData")

# Clean dataframe
ssats_2019 <- PUF %>%
  # Rename most variables for easy identification
  rename(id = CASEID, state = STATE, lgbt = SRVC62, owner = OWNERSHP, sliding_fee = FEESCALE,
         pay_no_low = PAYASST, pay_cash = REVCHK1, pay_private_ins = REVCHK2, pay_free = REVCHK3, 
         pay_medicaid = REVCHK5, pay_medicare = REVCHK8, pay_state_ins = REVCHK10, 
         pay_military_ins = REVCHK15, pay_itu = REVCHK17, accredited = ACCRED,
         supp_women = SRVC34, supp_men = SRVC64, supp_cooccur = SRVC31, supp_hiv = SRVC32,
         supp_sexabuse = SRVC121, supp_ipv = SRVC122, supp_trauma = SRVC116,
         num_assessment = ASSESSMENT, num_test = TESTING, num_medical = MEDICAL,
         num_transition = TRANSITION, num_recovery = RECOVERY, num_education = EDUCATION,
         num_ancillary = ANCILLARY, num_psychopharm = PHARMACOTHERAPIES, fips = STFIPS) %>%
  # Select relevant variables
  select(id, state, lgbt, owner, sliding_fee, accredited, starts_with("CTYPE"), 
         starts_with("supp_"), starts_with("num")) %>%
  # Recode values
  mutate(
    owner = recode(owner, `1` = "Private, For-profit", `2` = "Private, Non-profit", `3` = "State Government",
                   `4` = "Local Government", `5` = "Tribal Government", `6` = "State Government")
  )
ssats_2019

# SAVE TO FILE ------------------------------------------------------------

# N-SSATS 2019
write_csv(ssats_2019, file = "data/cleaned/ssats_2019.csv")
