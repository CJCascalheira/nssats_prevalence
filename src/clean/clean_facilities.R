# Dependencies
library(tidyverse)

# Import N-SSATS
nssats_2020 <- read_delim("data/nssats/NSSATS_2020.txt", delim = " ", col_names = FALSE) %>%
  select("X1", "X10")

# 2019
load("data/nssats/NSSATS_2019.RData")
nssats_2019 <- PUF

# 2018
load("data/nssats/nssats-puf-2018-R.RData")
nssats_2018 <- mySASData %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2017
load("data/nssats/nssatspuf_2017.RData")
nssats_2017 <- nssatspuf_2017 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2016
load("data/nssats/nssatspuf_2016.Rda")
nssats_2016 <- n16 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2015
load("data/nssats/N-SSATS-2015-DS0001-data-r.rda")
nssats_2015 <- nssats2015_puf %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2014
load("data/nssats/N-SSATS-2014-DS0001-data-r.rda")
nssats_2014 <- nssatpuf_2014 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2013
load("data/nssats/N-SSATS-2013-DS0001-data-r.rda")
nssats_2013 <- nssatpuf_2013 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2012
load("data/nssats/N-SSATS-2012-DS0001-data-r.rda")
nssats_2012 <- nssatpuf_2012 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2011
load("data/nssats/N-SSATS-2011-DS0001-data-r.rda")
nssats_2011 <- nssatpuf_2011 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2010
load("data/nssats/N-SSATS-2010-DS0001-data-r.rda")
nssats_2010 <- nssatpuf_2010 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# Clean up environment
rm(PUF, mySASData, nssatspuf_2017, n16, nssats2015_puf, nssatpuf_2014,
   nssatpuf_2013, nssatpuf_2012, nssatpuf_2011, nssatpuf_2010)

# Import N-MHSS
load("data/nmhss/NMHSS_2019.RData")
nmhss_2019 <- PUF

# Clean up environment
rm(PUF)

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

#* N-SSATS 2018 -----------------------------------------------------------

# Vector of names
names_nssats <- c("region", "lgbtq")

# Rename
names(nssats_2018) <- names_nssats

# Clean N-SSATS 2018
nssats_2018a <- nssats_2018 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2018a

#* N-SSATS 2017 -----------------------------------------------------------

# Rename
names(nssats_2017) <- names_nssats

# Clean N-SSATS 2017
nssats_2017a <- nssats_2017 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2017a

#* N-SSATS 2016 -----------------------------------------------------------

# Rename
names(nssats_2016) <- names_nssats

# Clean N-SSATS 2016
nssats_2016a <- nssats_2016 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2016a

#* N-SSATS 2015 -----------------------------------------------------------

# Rename
names(nssats_2015) <- names_nssats

# Clean N-SSATS 2015
nssats_2015a <- nssats_2015 %>%
  rename(state = region) %>%
  mutate(
    state = str_trim(state),
    lgbtq = if_else(lgbtq == "No", 0, 1)
  ) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2015a

#* N-SSATS 2014 -----------------------------------------------------------

# Rename
names(nssats_2014) <- names_nssats

# Clean N-SSATS 2014
nssats_2014a <- nssats_2014 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2014a

#* N-SSATS 2013 -----------------------------------------------------------

# Rename
names(nssats_2013) <- names_nssats

# Clean N-SSATS 2013
nssats_2013a <- nssats_2013 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2013a

#* N-SSATS 2012 -----------------------------------------------------------

# Rename
names(nssats_2012) <- names_nssats

# Clean N-SSATS 2012
nssats_2012a <- nssats_2012 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2012a

#* N-SSATS 2011 -----------------------------------------------------------

# Rename
names(nssats_2011) <- names_nssats

# Clean N-SSATS 2011
nssats_2011a <- nssats_2011 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2011a

#* N-SSATS 2010 -----------------------------------------------------------

# Rename
names(nssats_2010) <- names_nssats

# Clean N-SSATS 2010
nssats_2010a <- nssats_2010 %>%
  rename(state = region) %>%
  filter(lgbtq == 1) %>%
  group_by(state) %>%
  count(lgbtq) %>%
  select(state, lgbtq_total = n) %>%
  right_join(state_names, by = "state") %>%
  select(region, state, lgbtq_total)
nssats_2010a

# CLEAN N-MHSS ------------------------------------------------------------

#* N-MHSS 2019 ------------------------------------------------------------

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

# Save new N-SSATS files for structural stigma analysis
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2020a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2019a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2018a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2017a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2016a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2015a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2014a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2013a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2012a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2011a.csv")
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2010a.csv")
