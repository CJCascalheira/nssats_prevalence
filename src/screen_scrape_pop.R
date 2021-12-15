# Dependencies
library(tidyverse)
library(rvest)
library(xml2)
library(httr)

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# GET URLS AND HTML DOCUMENTS ---------------------------------------------

# Set the URLs from the Web Archive and current web

# 02 Jul 2019
url_2019 <- "https://web.archive.org/web/20190702032449/https://www.lgbtmap.org/equality-maps/lgbt_populations"

# 06 July 2020
url_2020 <- "https://web.archive.org/web/20200706172030/https://www.lgbtmap.org/equality-maps/lgbt_populations"

# 22 July 2021
url_2021 <- "https://web.archive.org/web/20210722103500/https://www.lgbtmap.org/equality-maps/lgbt_populations"

# Get the data
lgbt_pop_2019 <- url_2019 %>% GET(., timeout(30)) %>% read_html()
lgbt_pop_2020 <- url_2020 %>% GET(., timeout(30)) %>% read_html()
lgbt_pop_2021 <- url_2021 %>% GET(., timeout(30)) %>% read_html()

# CLEAN TABLES ------------------------------------------------------------

#* 2019 -------------------------------------------------------------------

# HTML code for lgbt population table
section_2019 <- html_node(lgbt_pop_2019,
                          xpath = '//*[@id="map-5"]/table') 

# Covert to table in R
table_2019 <- html_table(section_2019)
head(table_2019)

# Clean the table
pop_table_2019 <- table_2019[-1, ] %>%
  select(region = X1, lgbtq_pop_total = X3, lgbtq_pop_perc = X4) %>%
  mutate(
    # Remove string formatting
    lgbtq_pop_perc = str_extract(lgbtq_pop_perc, regex("\\d\\.\\d")),
    lgbtq_pop_total = str_remove(lgbtq_pop_total, ","),
    lgbtq_pop_total = str_remove(lgbtq_pop_total, ","),
    # Convert to numeric
    lgbtq_pop_perc = as.numeric(lgbtq_pop_perc),
    lgbtq_pop_total = as.numeric(lgbtq_pop_total) / 1000,
    # Prepare the merging column
    region = tolower(region),
    # Add year
    year = rep(2019, nrow(.))
  ) %>%
  right_join(state_names) %>%
  select(region, state, everything())
pop_table_2019

#* 2020 -------------------------------------------------------------------

# HTML code for lgbt population table
section_2020 <- html_node(lgbt_pop_2020,
                          xpath = '//*[@id="map-5"]/table') 

# Covert to table in R
table_2020 <- html_table(section_2020)
head(table_2020)

# Clean the table
pop_table_2020 <- table_2020[-1, ] %>%
  select(region = X1, lgbtq_pop_total = X3, lgbtq_pop_perc = X4) %>%
  mutate(
    # Remove string formatting
    lgbtq_pop_perc = str_extract(lgbtq_pop_perc, regex("\\d\\.\\d")),
    lgbtq_pop_total = str_remove(lgbtq_pop_total, ","),
    lgbtq_pop_total = str_remove(lgbtq_pop_total, ","),
    # Convert to numeric
    lgbtq_pop_perc = as.numeric(lgbtq_pop_perc),
    lgbtq_pop_total = as.numeric(lgbtq_pop_total) / 1000,
    # Prepare the merging column
    region = tolower(region),
    # Add year
    year = rep(2020, nrow(.))
  ) %>%
  right_join(state_names) %>%
  select(region, state, everything())
pop_table_2020

#* 2021 -------------------------------------------------------------------

# HTML code for lgbt population table
section_2021 <- html_node(lgbt_pop_2021,
                          xpath = '//*[@id="map-5"]/table') 

# Covert to table in R
table_2021 <- html_table(section_2021)
head(table_2021)

# Clean the table
pop_table_2021 <- table_2021[-1, ] %>%
  select(region = X1, lgbtq_pop_total = X3, lgbtq_pop_perc = X4) %>%
  mutate(
    # Remove string formatting
    lgbtq_pop_perc = str_extract(lgbtq_pop_perc, regex("\\d\\.\\d")),
    lgbtq_pop_total = str_remove(lgbtq_pop_total, ","),
    lgbtq_pop_total = str_remove(lgbtq_pop_total, ","),
    # Convert to numeric
    lgbtq_pop_perc = as.numeric(lgbtq_pop_perc),
    lgbtq_pop_total = as.numeric(lgbtq_pop_total) / 1000,
    # Prepare the merging column
    region = tolower(region),
    # Add year
    year = rep(2021, nrow(.))
  ) %>%
  right_join(state_names) %>%
  select(region, state, everything())
pop_table_2021

# EXPORT FILES ------------------------------------------------------------

write_csv(pop_table_2019, "data/cleaned/lgbtq_pop/pop_table_2019.csv")
write_csv(pop_table_2020, "data/cleaned/lgbtq_pop/pop_table_2020.csv")
write_csv(pop_table_2021, "data/cleaned/lgbtq_pop/pop_table_2021.csv")
