# Load dependencies
library(tidyverse)
library(plm)
library(OrthoPanels)

# IMPORT DATA -------------------------------------------------------------

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
nssats_eq <- left_join(nssats, eq_tables)

# PREPARE PANEL DATA ------------------------------------------------------

# Set panel data
nssats_eq