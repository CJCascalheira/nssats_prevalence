# Dependencies
library(rio)
library(tidyverse)

# Import
hate_2019 <- import("data/hate_crimes/table_13_2019.xls") %>%
  as_tibble()

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
  select(region, lgbtq_hate)
hate_2019a

# EXPORT DATA -------------------------------------------------------------

write_csv(hate_2019a, "data/cleaned/hate_crimes/hate_2019.csv")
