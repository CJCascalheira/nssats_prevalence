# Load dependencies
library(tidyverse)
library(plm)
library(OrthoPanels)

# IMPORT DATA -------------------------------------------------------------

# Import hate crime
hate_2019 <- read_csv("data/cleaned/hate_crimes/hate_2019.csv")

# Import data - N-SSATS
my_csvs <- list.files("data/cleaned/nssats/")
my_csvs <- paste0("data/cleaned/nssats/", my_csvs)
csv_list <- lapply(my_csvs, read_csv)

# Vector of years
year_vector <- c(2010:2020)

# Add year and create long data
for (i in 1:length(year_vector)) {
  # For each csv file
  csv_list[[i]] <- csv_list[[i]] %>%
    # Create a year column
    mutate(year = rep(year_vector[i], nrow(.)))
}

# Combine data frames
nssats <- bind_rows(csv_list, .id = "column_label") %>%
  select(-column_label)

# Import the CSV files - Equality Tables
my_csvs1 <- list.files("data/equality_tables/transformed/")
my_csvs1 <- paste0("data/equality_tables/transformed/", my_csvs1)
csv_list1 <- lapply(my_csvs1, read_csv)

# Combine data frames
eq_tables <- bind_rows(csv_list1, .id = "column_label") %>%
  select(-column_label)

# Merge the two imported data frames
nssats_eq <- left_join(nssats, eq_tables) %>%
  rename(state_policy = overall_policy_total)

# SINGLE YEAR - DETAILED --------------------------------------------------

# Prepare the data
cross_section <- nssats_eq %>%
  # Select the year 
  filter(year == 2019) %>%
  left_join(hate_2019, by = "region") %>%
  # Replace missing values
  mutate(lgbtq_hate = if_else(is.na(lgbtq_hate), 0, lgbtq_hate))

# Regression
summary(lm(lgbtq_total ~ state_policy + lgbtq_hate, data = cross_section))

# PROOF OF CONCEPT TEST ---------------------------------------------------

# Get data for specific year
nssats_eq_example <- nssats_eq %>%
  filter(year == 2020)

# Simple cross-sectional model
summary(lm(lgbtq_total ~ state_policy, data = nssats_eq_example))
summary(lm(lgbtq_perc ~ state_policy, data = nssats_eq_example))

# From a cross-sectional view, some years are associated with LGBTQ+ programming,
# but some are not. Some years, the prediction is significant. Slight variation
# depending if we examine percent of programming or total programs

# PREPARE PANEL DATA ------------------------------------------------------

# Set panel data
nssats_eq

