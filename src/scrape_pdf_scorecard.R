# Resources
# See https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
# See https://rpubs.com/behzod/tabulizer

# Set JAVA path
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_291/")

# Dependencies
library(rJava)
library(tabulizer)
library(tidyverse)

# EXTRACT SENATE DATA -----------------------------------------------------

# Find areas - need to select well above the table to get header
area <- locate_areas(file = "data/hrc_cong_score/116_senate.pdf",
             pages = 1)

# PDF scrape tables
senate <- extract_tables(
  file = "data/hrc_cong_score/116_senate.pdf",
  area = area,
  guess = FALSE, 
  output = "data.frame")

# Clean up list
senate_1 <- map(senate, ~ subset(., select = c(1:2))) %>%
  # Turn into one data frame
  bind_rows() %>%
  rename(senator = X, score = X116th) %>%
  # Remove former column headers
  filter(score != 'SCORE') %>%
  mutate(filter_col = if_else(str_detect(senator, regex("\\(\\w\\)")), 0, 1))

# Move state name to a new column
if (senate_1$filter_col == 1) {
  senate_2 <- senate_1 %>%
    mutate(state = str_extract(senator, regex("([A-Z])+\\s?([A-Z])+"))) %>%
    select(senator, score, state)
}

# Get vector of repeated state names
state_names <- rep(c(na.omit(senate_2$state)), each = 3)

# Get federal senate scores by state
senate_scores <- senate_2 %>%
  # Add repeating state names
  mutate(state = tolower(state_names)) %>%
  # Remove extra row
  filter(!senator %in% state_names) %>%
  # Change to numeric
  mutate(score = as.numeric(score)) %>%
  # Average score for state senators
  group_by(state) %>%
  summarize(
    senate_score = mean(score)
  )
senate_scores

# EXTRACT HOUSE DATA ------------------------------------------------------

# Find areas - need to select well above the table to get header
area <- locate_areas(file = "data/hrc_cong_score/116_house.pdf",
                     pages = 1)

# PDF scrape tables
house <- extract_tables(
  file = "data/hrc_cong_score/116_house.pdf",
  area = area,
  guess = FALSE, 
  output = "data.frame")

# Clean up list
house_1 <- map(house, ~ subset(., select = c(1:2))) %>%
  # Turn into one data frame
  bind_rows() %>%
  rename(senator = X, score = X116th) %>%
  # Remove former column headers
  filter(score != 'SCORE') %>%
  mutate(filter_col = if_else(str_detect(senator, regex("\\(\\w\\)")), 0, 1))

# Move state name to a new column
if (house_1$filter_col == 1) {
  house_2 <- house_1 %>%
    mutate(state = str_extract(senator, regex("([A-Z])+\\s?([A-Z])+"))) %>%
    select(senator, score, state)
}

# Clean the state names
state_names <- str_remove(c(house_2$state), regex("AL \\w?|TJ$"))

# Prepare a data frame 
filter_df <- data.frame(
  state = state_names,
  num = as.character(seq(1:485))
) %>%
  as_tibble() %>%
  mutate(
    subset_value = if_else(str_detect(state, regex("([A-Z])+\\s?([A-Z])+")), 
                           str_extract(num, regex("\\d+")), NA_character_)
  )
filter_df

# Get the filter sequence
filter_seq <- as.numeric(c(na.omit(filter_df$subset_value), "486"))

# Initialize vector
state_vector <- c()

for (i in 1:length(filter_seq)) {
  # Create another iterator
  j = i + 1
  
  # Set subset start and stop
  start <- filter_seq[i]
  stop <- filter_seq[j] - 1
  
  # Set number of reptitions
  rep_num <- length(state_names[start:stop])
  
  # Save names
  state_vector <- c(state_vector, rep(state_names[start], rep_num))
}

# Get federal house scores by state
house_scores <- house_2 %>%
  # Add repeating state names
  mutate(state = tolower(state_vector)) %>%
  # Remove extra row
  filter(!senator %in% c(na.omit(state_names))) %>%
  # Change to numeric
  mutate(score = as.numeric(score)) %>%
  # Average score for state senators
  group_by(state) %>%
  summarize(
    house_score = mean(score, na.rm = TRUE)
  )
house_scores

# MERGE AND EXPORT --------------------------------------------------------

# Join the data and compute federal scorecard
cong_116_scores <- left_join(senate_scores, house_scores, by = "state") %>%
  mutate(cong_116_score = (senate_score + house_score) / 2)

# Write to file
write_csv(cong_116_scores, "data/hrc_cong_score/cong_116_scores.csv")
