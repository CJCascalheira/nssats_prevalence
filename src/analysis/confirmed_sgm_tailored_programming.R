# Dependencies
library(tidyverse)
library(readxl)

# Import
confirmed_sgm <- read_excel("data/sgm-tailored-confirmed.xlsx", sheet = 2) %>%
  rename(agency = "Agency #", state = State)

nssats_2020 <- read_csv("data/cleaned/nssats/nssats_2020a.csv") %>%
  select(state, lgbtq_total)

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]

# Calculate number of confirmed SGM-tailored programming per state
confirmed_sgm_1 <- confirmed_sgm %>%
  count(state) %>%
  rename(lgbtq_actual = n) %>%
  right_join(nssats_2020, by = "state") %>% 
  left_join(state_names) %>%
  # Replace NA with 0, which indicates no confirmation in that state
  mutate(
    lgbtq_actual = as.double(lgbtq_actual),
    lgbtq_actual = if_else(is.na(lgbtq_actual), 0, lgbtq_actual)
  ) %>%
  # Add percentage of facilities confirmed
  mutate(
    perc_confirmed = lgbtq_actual / lgbtq_total 
  ) %>%
  select(region, state, everything())
confirmed_sgm_1

# Save to file
write_csv(confirmed_sgm_1, "data/cleaned/confirmed_sgm_tailored_programming.csv")
