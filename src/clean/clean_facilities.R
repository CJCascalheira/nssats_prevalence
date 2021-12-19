# Dependencies
library(tidyverse)

# IMPORT ------------------------------------------------------------------

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# Import correction to SGM-tailored programming
confirmed_sgm <- read_csv("data/cleaned/confirmed_sgm_tailored_programming.csv") %>%
  select(region, perc_confirmed)

#* IMPORT N-SSATS ---------------------------------------------------------

# 2020
nssats_2020 <- read_delim("data/nssats/NSSATS_2020.txt", delim = " ", col_names = FALSE) %>%
  select("X1", "X2", "X10")
nssats_2020_funding <- read_csv("data/nssats/NSSATS_2020_funding.csv")

# 2019
load("data/nssats/NSSATS_2019.RData")
nssats_2019 <- PUF

# 2018
load("data/nssats/nssats-puf-2018-R.RData")
nssats_2018 <- mySASData %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2017
load("data/nssats/nssatspuf_2017.RData")
nssats_2017 <- nssatspuf_2017 %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2016
load("data/nssats/nssatspuf_2016.Rda")
nssats_2016 <- n16 %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2015
load("data/nssats/N-SSATS-2015-DS0001-data-r.rda")
nssats_2015 <- nssats2015_puf %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2014
load("data/nssats/N-SSATS-2014-DS0001-data-r.rda")
nssats_2014 <- nssatpuf_2014 %>%
  select(STATE, SRVC62) %>%
  as_tibble()

# 2013
load("data/nssats/N-SSATS-2013-DS0001-data-r.rda")
nssats_2013 <- nssatpuf_2013 %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2012
load("data/nssats/N-SSATS-2012-DS0001-data-r.rda")
nssats_2012 <- nssatpuf_2012 %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2011
load("data/nssats/N-SSATS-2011-DS0001-data-r.rda")
nssats_2011 <- nssatpuf_2011 %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# 2010
load("data/nssats/N-SSATS-2010-DS0001-data-r.rda")
nssats_2010 <- nssatpuf_2010 %>%
  select(STATE, SRVC62, EARMARK) %>%
  as_tibble()

# Clean up environment
rm(PUF, mySASData, nssatspuf_2017, n16, nssats2015_puf, nssatpuf_2014,
   nssatpuf_2013, nssatpuf_2012, nssatpuf_2011, nssatpuf_2010)

#* IMPORT N-MHSS ----------------------------------------------------------

# 2019
load("data/nmhss/NMHSS_2019.RData")
nmhss_2019 <- PUF %>%
  select(CASEID, LST, SRVC62, FUNDOTHSTATE, FUNDLOCALGOV, FUNDSMHA, FUNDSTATEWELFARE, 
         FUNDSTATEJUV, FUNDSTATEEDUC)

# 2018
load("data/nmhss/nhmss_puf_2018_r.Rdata")
nmhss_2018 <- nm18 %>%
  select(LST, SRVC62, FUNDOTHSTATE, FUNDLOCALGOV, FUNDSMHA, FUNDSTATEWELFARE, 
         FUNDSTATEJUV, FUNDSTATEEDUC)

# 2017
load("data/nmhss/nmhss_puf_2017.Rdata")
nmhss_2017 <- nmhsspuf_2017 %>%
  select(lst, SRVC62, FUNDOTHSTATE, FUNDLOCALGOV, FUNDSMHA, FUNDSTATEWELFARE, 
         FUNDSTATEJUV, FUNDSTATEEDUC)

# 2016
load("data/nmhss/nmhss_puf_2016.Rdata")
nmhss_2016 <- nmhsspuf_2016 %>%
  select(LST, SRVC62, FUNDOTHSTATE, FUNDLOCALGOV, FUNDSMHA, FUNDSTATEWELFARE, 
         FUNDSTATEJUV, FUNDSTATEEDUC)

# 2015
load("data/nmhss/mh2015_puf.Rda")
nmhss_2015 <- mh2015_puf %>%
  select(LST, SRVC62, FUNDOTHSTATE, FUNDLOCALGOV, FUNDSMHA, FUNDSTATEWELFARE, 
         FUNDSTATEJUV, FUNDSTATEEDUC)

# 2014
load("data/nmhss/mh2014_puf.Rda")
nmhss_2014 <- mh2014_puf %>%
  select(lst, SRVC62, FUNDOTHSTATE, FUNDLOCALGOV, FUNDSMHA, FUNDSTATEWELFARE, 
         FUNDSTATEJUV, FUNDSTATEEDUC)

# Clean up environment
rm(PUF, nm18, nmhsspuf_2017, nmhsspuf_2016, mh2015_puf, mh2014_puf)

# CLEAN N-SSATS -----------------------------------------------------------

#* N-SSATS 2020 -----------------------------------------------------------

# Vector of names
names_2020 <- c("region", "n_facility", "lgbtq_total")

# Rename
names(nssats_2020) <- names_2020

# Prepare funding
nssats_2020_funding_1 <- nssats_2020_funding %>%
  mutate(
    state = if_else(str_detect(data, regex("[a-zA-Z]")), 1, 0)
  ) %>%
  filter(state == 1) %>%
  select(region = data)

nssats_2020_funding_2 <- nssats_2020_funding[seq(4, nrow(nssats_2020_funding), 4), ] %>%
  rename(govt_fund = data) %>%
  bind_cols(nssats_2020_funding_1) %>%
  filter(row_number() != 52) %>%
  mutate(region = tolower(region))

# Clean N-SSATS 2020
nssats_2020a <- nssats_2020 %>%
  mutate(
    # Removed underscore
    region = str_replace(region, "_", " "),
    # Transform to lower case
    region = tolower(region),
    lgbtq_perc = (lgbtq_total / n_facility) * 100
  ) %>%
  right_join(state_names, by = "region") %>%
  left_join(nssats_2020_funding_2, by = "region") %>%
  mutate(govt_fund = as.numeric(govt_fund)) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n_facility) * 100
  ) %>%
  select(region, state, everything())
nssats_2020a

#* N-SSATS 2019 -----------------------------------------------------------

# Clean N-SSATS 2019
ssats_2019 <- nssats_2019 %>%
  # Rename most variables for easy identification
  rename(id = CASEID, state = STATE, lgbt = SRVC62, govt = EARMARK, owner = OWNERSHP, 
         sliding_fee = FEESCALE,
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
  select(id, state, lgbt, govt, owner, sliding_fee, accredited, residential, outpatient,
         starts_with("supp_"), starts_with("num")) %>%
  # Recode values
  mutate(
    owner = recode(owner, `1` = "Private, for-profit", `2` = "Private, non-profit", `3` = "State government",
                   `4` = "Local government", `5` = "Tribal government", `6` = "State government"),
    across(sliding_fee:supp_trauma, ~ factor(., levels = c(0, 1), labels = c("No", "Yes")))
  )
ssats_2019

# Prepare totals - state
ssats_2019_state <- ssats_2019 %>%
  count(state)

# Prepare totals - lgbtq
ssats_2019_lgbtq <- ssats_2019 %>%
  count(state, lgbt) %>%
  filter(lgbt == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
ssats_2019_govt <- ssats_2019 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2019a <- ssats_2019_state %>%
  left_join(ssats_2019_lgbtq) %>%
  left_join(ssats_2019_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2019a

#* N-SSATS 2018 -----------------------------------------------------------

# Vector of names
names_nssats <- c("state", "lgbtq", "govt")

# Rename
names(nssats_2018) <- names_nssats

# Prepare totals - state
nssats_2018_state <- nssats_2018 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2018_lgbtq <- nssats_2018 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2018_govt <- nssats_2018 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2018a <- nssats_2018_state %>%
  left_join(nssats_2018_lgbtq) %>%
  left_join(nssats_2018_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2018a

#* N-SSATS 2017 -----------------------------------------------------------

# Rename
names(nssats_2017) <- names_nssats

# Prepare totals - state
nssats_2017_state <- nssats_2017 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2017_lgbtq <- nssats_2017 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2017_govt <- nssats_2017 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2017a <- nssats_2017_state %>%
  left_join(nssats_2017_lgbtq) %>%
  left_join(nssats_2017_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2017a

#* N-SSATS 2016 -----------------------------------------------------------

# Rename
names(nssats_2016) <- names_nssats

# Prepare totals - state
nssats_2016_state <- nssats_2016 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2016_lgbtq <- nssats_2016 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2016_govt <- nssats_2016 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2016a <- nssats_2016_state %>%
  left_join(nssats_2016_lgbtq) %>%
  left_join(nssats_2016_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2016a

#* N-SSATS 2015 -----------------------------------------------------------

# Rename
names(nssats_2015) <- names_nssats

# Prepare totals - state
nssats_2015_state <- nssats_2015 %>%
  mutate(state = str_trim(state)) %>%
  count(state)

# Prepare totals - lgbtq
nssats_2015_lgbtq <- nssats_2015 %>%
  mutate(state = str_trim(state)) %>%
  count(state, lgbtq) %>%
  filter(lgbtq == "Yes") %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2015_govt <- nssats_2015 %>%
  mutate(state = str_trim(state)) %>%
  count(state, govt) %>%
  filter(govt == "Yes") %>%
  select(state, govt_total = n)

# Transform 
nssats_2015a <- nssats_2015_state %>%
  left_join(nssats_2015_lgbtq) %>%
  left_join(nssats_2015_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2015a

#* N-SSATS 2014 -----------------------------------------------------------

# Rename
names(nssats_2014) <- c("state", "lgbtq")

# Prepare totals - state
nssats_2014_state <- nssats_2014 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2014_lgbtq <- nssats_2014 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Transform 
nssats_2014a <- nssats_2014_state %>%
  left_join(nssats_2014_lgbtq) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = rep(NA_character_, nrow(.)),
    govt_total = rep(NA_character_, nrow(.))
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2014a

#* N-SSATS 2013 -----------------------------------------------------------

# Rename
names(nssats_2013) <- names_nssats

# Prepare totals - state
nssats_2013_state <- nssats_2013 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2013_lgbtq <- nssats_2013 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2013_govt <- nssats_2013 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2013a <- nssats_2013_state %>%
  left_join(nssats_2013_lgbtq) %>%
  left_join(nssats_2013_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2013a

#* N-SSATS 2012 -----------------------------------------------------------

# Rename
names(nssats_2012) <- names_nssats

# Prepare totals - state
nssats_2012_state <- nssats_2012 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2012_lgbtq <- nssats_2012 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2012_govt <- nssats_2012 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2012a <- nssats_2012_state %>%
  left_join(nssats_2012_lgbtq) %>%
  left_join(nssats_2012_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2012a

#* N-SSATS 2011 -----------------------------------------------------------

# Rename
names(nssats_2011) <- names_nssats

# Prepare totals - state
nssats_2011_state <- nssats_2011 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2011_lgbtq <- nssats_2011 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2011_govt <- nssats_2011 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2011a <- nssats_2011_state %>%
  left_join(nssats_2011_lgbtq) %>%
  left_join(nssats_2011_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    # Replace the missing value - AR has no LGBTQ programming
    lgbtq_total = if_else(is.na(lgbtq_total), 0, as.double(lgbtq_total)), 
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2011a

#* N-SSATS 2010 -----------------------------------------------------------

# Rename
names(nssats_2010) <- names_nssats

# Prepare totals - state
nssats_2010_state <- nssats_2010 %>%
  count(state)

# Prepare totals - lgbtq
nssats_2010_lgbtq <- nssats_2010 %>%
  count(state, lgbtq) %>%
  filter(lgbtq == 1) %>%
  select(state, lgbtq_total = n)

# Prepare totals - govt funding
nssats_2010_govt <- nssats_2010 %>%
  count(state, govt) %>%
  filter(govt == 1) %>%
  select(state, govt_total = n)

# Transform 
nssats_2010a <- nssats_2010_state %>%
  left_join(nssats_2010_lgbtq) %>%
  left_join(nssats_2010_govt) %>%
  right_join(state_names, by = "state") %>%
  # Create percentages
  mutate(
    lgbtq_perc = (lgbtq_total / n) * 100,
    govt_fund = (govt_total / n) * 100
  ) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed,
    lgbtq_perc_actual = (lgbtq_actual / n) * 100
  ) %>%
  select(region, state, everything())
nssats_2010a

# CLEAN N-MHSS ------------------------------------------------------------

#* N-MHSS 2019 ------------------------------------------------------------

# Clean N-MHSS 2019
mhss_2019 <- nmhss_2019 %>%
  # Rename most variables for easy identification
  rename(id = CASEID, state = LST, lgbt = SRVC62) %>%
  # Select relevant variables
  select(id, state, lgbt, everything())

# Prepare totals - state
nmhss_2019_state <- nmhss_2019 %>%
  count(LST) %>%
  right_join(state_names, by = c("LST" = "state")) %>%
  select(-LST)

# Prepare totals - lgbtq
nmhss_2019_lgbtq <- nmhss_2019 %>%
  count(LST, SRVC62) %>%
  filter(SRVC62 == 1) %>%
  select(state = LST, lgbtq_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Prepare totals - govt funding
nmhss_2019_govt <- nmhss_2019 %>%
  select(LST, FUNDOTHSTATE:FUNDSTATEEDUC) %>%
  unite("funding", FUNDOTHSTATE:FUNDSTATEEDUC, remove = TRUE) %>%
  mutate(govt = if_else(str_detect(funding, "0_0_0_0_0_0"), 0, 1)) %>%
  filter(govt == 1) %>%
  count(LST, govt) %>%
  select(state = LST, govt_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Combine all data frames
nmhss_2019a <- left_join(nmhss_2019_lgbtq, nmhss_2019_govt) %>%
  left_join(nmhss_2019_state) %>%
  mutate(govt_fund = (govt_total / n) * 100) %>%
  select(state = region, everything(), -n)
nmhss_2019a

#* N-MHSS 2018 ------------------------------------------------------------

# Prepare totals - state
nmhss_2018_state <- nmhss_2018 %>%
  count(LST) %>%
  right_join(state_names, by = c("LST" = "state")) %>%
  select(-LST)

# Prepare totals - lgbtq
nmhss_2018_lgbtq <- nmhss_2018 %>%
  count(LST, SRVC62) %>%
  filter(SRVC62 == 1) %>%
  select(state = LST, lgbtq_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Prepare totals - govt funding
nmhss_2018_govt <- nmhss_2018 %>%
  select(LST, FUNDOTHSTATE:FUNDSTATEEDUC) %>%
  unite("funding", FUNDOTHSTATE:FUNDSTATEEDUC, remove = TRUE) %>%
  mutate(govt = if_else(str_detect(funding, "0_0_0_0_0_0"), 0, 1)) %>%
  filter(govt == 1) %>%
  count(LST, govt) %>%
  select(state = LST, govt_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Combine all data frames
nmhss_2018a <- left_join(nmhss_2018_lgbtq, nmhss_2018_govt) %>%
  left_join(nmhss_2018_state) %>%
  mutate(govt_fund = (govt_total / n) * 100) %>%
  select(state = region, everything(), -n)
nmhss_2018a

#* N-MHSS 2017 ------------------------------------------------------------

# Prepare totals - state
nmhss_2017_state <- nmhss_2017 %>%
  count(lst) %>%
  right_join(state_names, by = c("lst" = "state")) %>%
  select(-lst)

# Prepare totals - lgbtq
nmhss_2017_lgbtq <- nmhss_2017 %>%
  count(lst, SRVC62) %>%
  filter(SRVC62 == 1) %>%
  select(state = lst, lgbtq_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Prepare totals - govt funding
nmhss_2017_govt <- nmhss_2017 %>%
  select(lst, FUNDOTHSTATE:FUNDSTATEEDUC) %>%
  unite("funding", FUNDOTHSTATE:FUNDSTATEEDUC, remove = TRUE) %>%
  mutate(govt = if_else(str_detect(funding, "0_0_0_0_0_0"), 0, 1)) %>%
  filter(govt == 1) %>%
  count(lst, govt) %>%
  select(state = lst, govt_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Combine all data frames
nmhss_2017a <- left_join(nmhss_2017_lgbtq, nmhss_2017_govt) %>%
  left_join(nmhss_2017_state) %>%
  mutate(govt_fund = (govt_total / n) * 100) %>%
  select(state = region, everything(), -n)
nmhss_2017a

#* N-MHSS 2016 ------------------------------------------------------------

# Prepare totals - state
nmhss_2016_state <- nmhss_2016 %>%
  count(LST) %>%
  right_join(state_names, by = c("LST" = "state")) %>%
  select(-LST)

# Prepare totals - lgbtq
nmhss_2016_lgbtq <- nmhss_2016 %>%
  count(LST, SRVC62) %>%
  filter(SRVC62 == 1) %>%
  select(state = LST, lgbtq_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Prepare totals - govt funding
nmhss_2016_govt <- nmhss_2016 %>%
  select(LST, FUNDOTHSTATE:FUNDSTATEEDUC) %>%
  unite("funding", FUNDOTHSTATE:FUNDSTATEEDUC, remove = TRUE) %>%
  mutate(govt = if_else(str_detect(funding, "0_0_0_0_0_0"), 0, 1)) %>%
  filter(govt == 1) %>%
  count(LST, govt) %>%
  select(state = LST, govt_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Combine all data frames
nmhss_2016a <- left_join(nmhss_2016_lgbtq, nmhss_2016_govt) %>%
  left_join(nmhss_2016_state) %>%
  mutate(govt_fund = (govt_total / n) * 100) %>%
  select(state = region, everything(), -n)
nmhss_2016a

#* N-MHSS 2015 ------------------------------------------------------------

# Prepare totals - state
nmhss_2015_state <- nmhss_2015 %>%
  count(LST) %>%
  mutate(LST = str_trim(LST)) %>%
  right_join(state_names, by = c("LST" = "state")) %>%
  select(-LST)

# Prepare totals - lgbtq
nmhss_2015_lgbtq <- nmhss_2015 %>%
  count(LST, SRVC62) %>%
  filter(SRVC62 == "Yes") %>%
  select(state = LST, lgbtq_total = n) %>%
  as_tibble() %>%
  mutate(state = str_trim(state)) %>%
  right_join(state_names) %>%
  select(-state)

# Prepare totals - govt funding
nmhss_2015_govt <- nmhss_2015 %>%
  select(LST, FUNDOTHSTATE:FUNDSTATEEDUC) %>%
  mutate(across(FUNDOTHSTATE:FUNDSTATEEDUC, ~recode(., "No" = 0, "Yes" = 1, 
                                                    "Don't Know" = 2))) %>%
  as_tibble() %>%
  mutate(LST = str_trim(LST)) %>%
  unite("funding", FUNDOTHSTATE:FUNDSTATEEDUC, remove = TRUE) %>%
  mutate(govt = if_else(str_detect(funding, "0_0_0_0_0_0"), 0, 1)) %>%
  filter(govt == 1) %>%
  count(LST, govt) %>%
  select(state = LST, govt_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Combine all data frames
nmhss_2015a <- left_join(nmhss_2015_lgbtq, nmhss_2015_govt) %>%
  left_join(nmhss_2015_state) %>%
  mutate(govt_fund = (govt_total / n) * 100) %>%
  select(state = region, everything(), -n)
nmhss_2015a

#* N-MHSS 2014 ------------------------------------------------------------

# Prepare totals - state
nmhss_2014_state <- nmhss_2014 %>%
  count(lst) %>%
  mutate(lst = str_trim(lst)) %>%
  right_join(state_names, by = c("lst" = "state")) %>%
  select(-lst)

# Prepare totals - lgbtq
nmhss_2014_lgbtq <- nmhss_2014 %>%
  count(lst, SRVC62) %>%
  filter(SRVC62 == "Yes") %>%
  as_tibble() %>%
  mutate(lst = str_trim(lst)) %>%
  select(state = lst, lgbtq_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Prepare totals - govt funding
nmhss_2014_govt <- nmhss_2014 %>%
  select(lst, FUNDOTHSTATE:FUNDSTATEEDUC) %>%
  mutate(across(FUNDOTHSTATE:FUNDSTATEEDUC, ~recode(., "No" = 0, "Yes" = 1, 
                                                    "Don't Know" = 2))) %>%
  as_tibble() %>%
  mutate(lst = str_trim(lst)) %>%
  unite("funding", FUNDOTHSTATE:FUNDSTATEEDUC, remove = TRUE) %>%
  mutate(govt = if_else(str_detect(funding, "0_0_0_0_0_0"), 0, 1)) %>%
  filter(govt == 1) %>%
  count(lst, govt) %>%
  select(state = lst, govt_total = n) %>%
  right_join(state_names) %>%
  select(-state)

# Combine all data frames
nmhss_2014a <- left_join(nmhss_2014_lgbtq, nmhss_2014_govt) %>%
  left_join(nmhss_2014_state) %>%
  mutate(govt_fund = (govt_total / n) * 100) %>%
  select(state = region, everything(), -n)
nmhss_2014a

# SAVE TO FILE ------------------------------------------------------------

# For original descriptive stats
write_csv(ssats_2019, file = "data/cleaned/ssats_2019.csv")
write_csv(mhss_2019, file = "data/cleaned/mhss_2019.csv")

# Save new N-SSATS files for structural stigma analysis
write_csv(nssats_2020a, file = "data/cleaned/nssats/nssats_2020a.csv")
write_csv(nssats_2019a, file = "data/cleaned/nssats/nssats_2019a.csv")
write_csv(nssats_2018a, file = "data/cleaned/nssats/nssats_2018a.csv")
write_csv(nssats_2017a, file = "data/cleaned/nssats/nssats_2017a.csv")
write_csv(nssats_2016a, file = "data/cleaned/nssats/nssats_2016a.csv")
write_csv(nssats_2015a, file = "data/cleaned/nssats/nssats_2015a.csv")
write_csv(nssats_2014a, file = "data/cleaned/nssats/nssats_2014a.csv")
write_csv(nssats_2013a, file = "data/cleaned/nssats/nssats_2013a.csv")
write_csv(nssats_2012a, file = "data/cleaned/nssats/nssats_2012a.csv")
write_csv(nssats_2011a, file = "data/cleaned/nssats/nssats_2011a.csv")
write_csv(nssats_2010a, file = "data/cleaned/nssats/nssats_2010a.csv")

# Save new N-MHSS files for structural stigma analysis
write_csv(nmhss_2019a, file = "data/cleaned/nmhss/nmhss_2019a.csv")
write_csv(nmhss_2018a, file = "data/cleaned/nmhss/nmhss_2018a.csv")
write_csv(nmhss_2017a, file = "data/cleaned/nmhss/nmhss_2017a.csv")
write_csv(nmhss_2016a, file = "data/cleaned/nmhss/nmhss_2016a.csv")
write_csv(nmhss_2015a, file = "data/cleaned/nmhss/nmhss_2015a.csv")
write_csv(nmhss_2014a, file = "data/cleaned/nmhss/nmhss_2014a.csv")
