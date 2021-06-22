# Dependencies
library(tidyverse)
library(rvest)
library(stringr)
library(xml2)
library(httr)

# Load the data
lgbt_map <-read_html("https://www.lgbtmap.org/equality-maps/")

# What type of class?
class(lgbt_map)

# EXTRACT A TABLE FROM A WEBPAGE ------------------------------------------

# HTML code for lgbt_map table
section_of_lgbt_map <- html_node(lgbt_map,
                                  xpath='//*[@id="map-4"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_of_lgbt_map)
head(equality_table)

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# Assign names
equality_table_clean <- equality_table[-1, -1] %>%
  mutate(
    # Convert to numeric
    X11 = as.numeric(X11),
    X12 = as.numeric(X12),
    X13 = as.numeric(X13),
    # Lowercase
    X2 = tolower(X2)
  ) %>%
  rename(
    "region" = X2,
    "so_gi" = X3, 
    "relational_recognition" = X4, 
    "non_discrimination" = X5, 
    "religious_exemption" = X6,
    "lgbt_youth" = X7, 
    "healthcare" = X8, 
    "criminal_justic" = X9, 
    "identity_docs" = X10, 
    "so_policy_total" = X11, 
    "gi_policy_total" = X12, 
    "overall_policy_total" = X13
  ) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_table_clean.csv")
