# Dependencies
library(rio)
library(tidyverse)
library(Amelia)

# Import
hate_2019 <- import("data/hate_crimes/table_13_2019.xls") %>%
  as_tibble()

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# CLEAN - 2019 ------------------------------------------------------------

# Select columns
hate_2019_1 <- hate_2019[-c(1:4), c(1, 6, 8)]

# Set names
names(hate_2019_1) <- c("region", "SO_hate", "GI_hate")

# Prepare for export
hate_2019a <- hate_2019_1 %>%
  # Remove NA
  filter(!is.na(region)) %>%
  # Create total count
  mutate(
    region = tolower(region),
    SO_hate = as.numeric(SO_hate),
    GI_hate = as.numeric(GI_hate),
    lgbtq_hate = SO_hate + GI_hate
  ) %>%
  select(region, lgbtq_hate) %>%
  right_join(state_names) %>%
  select(-state) %>%
  arrange(region) %>%
  mutate(temp_id = 1:nrow(.))
hate_2019a

# MISSING DATA ------------------------------------------------------------

# For Amelia, see: https://www.opr.princeton.edu/workshops/Downloads/2018Jan_AmeliaPratt.pdf
# Only conducted once, so do not execute code again

# Need to replace with expectation maximization
# a_out <- amelia(as.data.frame(hate_2019a), m = 5, idvars = c("region"),
#                 emburn = c(50, 100)
# )

# Write to file
# write.amelia(obj = a_out, file.stem = "data/amelia_hate_crimes/amelia_hate_crimes_", format = "csv")

# Import imputed data set
hate_2019b <- read_csv("data/amelia_hate_crimes/amelia_hate_crimes_5.csv") %>%
  select(-X1, -temp_id)
hate_2019b

# Any missing data generally?
gather(hate_2019b, key = "variables", value = "values") %>%
  filter(is.na(values))

# EXPORT DATA -------------------------------------------------------------

write_csv(hate_2019b, "data/cleaned/hate_crimes/hate_2019.csv")
