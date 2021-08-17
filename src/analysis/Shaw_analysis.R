library(psych)
library(apaTables)
library(vctrs)
library(Hmisc)

library(tidyverse)

# These packages load with tidyverse - no need to call separately
#library(dplyr)
#library(readr)
#library(ggplot2)

# You will need to remove the data/ if you run this from your computer
nssats2019 <- read.csv("data/NSSATS_PUF_2019_CSV.csv",
                       na.strings = c("", "-99"))
nmhss2019 <- read.csv("nmhss-puf-2019-csv.csv",
                      na.strings = c("", "-99"))


# SAMPLE AGGREGATION ------------------------------------------------------

# Note that we will want to check with Emily to ensure our aggregation process
# is similar to theirs.

# Let's say we want to aggregate all the military-related services
# SRVC113 - SRVC115

# Emily's number is: Military-focused programs = 3,666

# Currently, our military-focused variable count is 1995
table(nssats2019$SRVC114)

# Aggregate military programs
nssats2019_example <- nssats2019 %>%
  # SRVC114 can be our home base variable with which will we merge the other
  # variables. SRVC114 is arbitrary; starting with any variable is fine.
  mutate(
    SRVC114 = if_else(SRVC113 == "1", "1", SRVC114),
    SRVC114 = if_else(SRVC115 == "1", "1", SRVC114)
  ) %>%
  # Since we aggregate SRVC113 and SRVC115, we could drop these from the data,
  # which is optional here (but best practice) since we are select variables later
  # in the code.
  select(-c(SRVC113, SRVC115))

# Now our aggregated variable matches Emily's
table(nssats2019_example$SRVC114)
  
# CHI-SQUARE ANALYSES -----------------------------------------------------

# types of tailored programs
nsstats.tailored.programs <- nssats2019 %>%
  select(SRVC30, SRVC34, SRVC64, SRVC63, SRVC62, SRVC114, SRVC61, SRVC31, SRVCPAINSA, SRVC32, SRVC116) %>%
  filter(SRVC30 != "M", SRVC34 != "M", SRVC64 != "M",
         SRVC63 != "M", SRVC62 != "M", SRVC114 != "M",
         SRVC61 != "M", SRVC31 != "M", SRVCPAINSA != "M",
         SRVC32 != "M", SRVC116 != "M")

head(nsstats.tailored.programs)

colnames(nsstats.tailored.programs) <- c("Adolescents and/or young adult", "Women-focused programs", 
                                                 "Adult Men", "Seniors older adults", "LGBT clients", "Military-focused programs",
                                                 "Crimminal Justice Clients", 
                                                 "Clients with co-occurring mental and substance use disorders", 
                                         "Clients with co-occurring mental and substance use disorders", "Clients with HIV or AIDS",
                                                 "Clients with trauma history")

# Check our counts
nsstats.tailored.programs %>%
  gather("variable", "score") %>%
  group_by(variable) %>%
  count(score) %>%
  View()
  
# Chi-square analyses
table(nsstats.tailored.programs$"Adolescents and/or young adult", nsstats.tailored.programs$"LGBT clients")
chisq.test(table(nsstats.tailored.programs$"Adolescents and/or young adult", nsstats.tailored.programs$"LGBT clients"))

  table(nsstats.tailored.programs$"Women-focused programs", nsstats.tailored.programs$"LGBT clients")
  chisq.test(table(nsstats.tailored.programs$"Women-focused programs", nsstats.tailored.programs$"LGBT clients"))
  
  table(nsstats.tailored.programs$"Adult Men", nsstats.tailored.programs$"LGBT clients") 
  table(nsstats.tailored.programs$"Seniors older adults", nsstats.tailored.programs$"LGBT clients") 
  table(nsstats.tailored.programs$"LGBT clients", nsstats.tailored.programs$"LGBT clients")
  table(nsstats.tailored.programs$"Military-focused programs", nsstats.tailored.programs$"LGBT clients") 
  table(nsstats.tailored.programs$"Crimminal Justice Clients", nsstats.tailored.programs$"LGBT clients") 
  table(nsstats.tailored.programs$"Clients with co-occurring mental and substance use disorders", nsstats.tailored.programs$"LGBT clients")
  table(nsstats.tailored.programs$"Clients with co-occurring pain and substance use", nsstats.tailored.programs$"LGBT clients")
  table(nsstats.tailored.programs$"Clients with HIV or AIDS", nsstats.tailored.programs$"LGBT clients") 
  table(nsstats.tailored.programs$"Clients with trauma history", nsstats.tailored.programs$"LGBT clients")
  

  
  



  


         
         
