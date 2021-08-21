# Dependencies
library(tidyverse)

# Import
eq_tables <- list.files("data/equality_tables")

for (i in 1:length(eq_tables)) {
  # Select the table
  this_table <- read_csv(paste0("data/equality_tables/", eq_tables[i]))
  
  # Find the table number (i.e., year)
  table_num <- str_extract(eq_tables[i], "\\d{4}")
  
  # Create the table
  assign(paste("eq_table", table_num, sep = "_"), this_table)
}

# TOTAL POLICY SCORES -----------------------------------------------------

# 2021
eq_table_2021a <- eq_table_2021 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2021, nrow(.)))

# 2020
eq_table_2020a <- eq_table_2020 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2020, nrow(.)))

# 2019
eq_table_2019a <- eq_table_2019 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2019, nrow(.)))

# 2018
eq_table_2018a <- eq_table_2018 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2018, nrow(.)))

# 2017
eq_table_2017a <- eq_table_2017 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2017, nrow(.)))

# 2016
eq_table_2016a <- eq_table_2016 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2016, nrow(.)))

# 2015
eq_table_2015a <- eq_table_2015 %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, state, overall_policy_total) %>%
  mutate(year = rep(2015, nrow(.)))

# 2014
eq_table_2014a <- eq_table_2014 %>%
  rowwise() %>%
  mutate(
    overall_policy_total = sum(c_across(relational_recognition:hiv_criminalization))
  ) %>%
  select(region, state, overall_policy_total) %>%
  ungroup() %>%
  mutate(year = rep(2014, nrow(.)))

# 2013
eq_table_2013a <- eq_table_2013 %>%
  rowwise() %>%
  mutate(
    overall_policy_total = sum(c_across(relational_recognition:hiv_criminalization))
  ) %>%
  select(region, state, overall_policy_total) %>%
  ungroup() %>%
  mutate(year = rep(2013, nrow(.)))

# 2012
eq_table_2012a <- eq_table_2012 %>%
  rowwise() %>%
  mutate(
    overall_policy_total = sum(c_across(relational_recognition:medical_leave))
  ) %>%
  select(region, state, overall_policy_total) %>%
  ungroup() %>%
  mutate(year = rep(2012, nrow(.)))

# 2011
eq_table_2011a <- eq_table_2011 %>%
  rowwise() %>%
  mutate(
    overall_policy_total = sum(c_across(relational_recognition:identity_docs))
  ) %>%
  select(region, state, overall_policy_total) %>%
  ungroup() %>%
  mutate(year = rep(2011, nrow(.)))

# 2010
eq_table_2010a <- eq_table_2010 %>%
  rowwise() %>%
  mutate(
    overall_policy_total = sum(c_across(hate_crime:identity_docs))
  ) %>%
  select(region, state, overall_policy_total) %>%
  ungroup() %>%
  mutate(year = rep(2010, nrow(.)))

# TRANSFORMATION FUNCTION -------------------------------------------------

# Cohen et al. (1999), Moeller (2015), Little (2013)
pomp <- function(df) {
  # Theoretical maximum (MAP, 2020)
  map_max_2020 <- 38.5
  
  # Calculate the numerator
  numerator <- (df - min(df))
  
  # Calculate the denominator
  denominator <- (map_max_2020 - min(df))
  
  # Get the POMP values
  pomp_values <- (numerator / denominator) * 100
  
  # Return the POMP values
  return(pomp_values)
}

# APPLY TRANSFORMATION ----------------------------------------------------

# Violates the DRY principle, but works for now!
eq_table_2010a[3] <- pomp(eq_table_2010a[3])
eq_table_2011a[3] <- pomp(eq_table_2011a[3])
eq_table_2012a[3] <- pomp(eq_table_2012a[3])
eq_table_2013a[3] <- pomp(eq_table_2013a[3])
eq_table_2014a[3] <- pomp(eq_table_2014a[3])
eq_table_2015a[3] <- pomp(eq_table_2015a[3])
eq_table_2016a[3] <- pomp(eq_table_2016a[3])
eq_table_2017a[3] <- pomp(eq_table_2017a[3])
eq_table_2018a[3] <- pomp(eq_table_2018a[3])
eq_table_2019a[3] <- pomp(eq_table_2019a[3])
eq_table_2020a[3] <- pomp(eq_table_2020a[3])
eq_table_2021a[3] <- pomp(eq_table_2021a[3])

# CHECK TRANSFORMATION ----------------------------------------------------

# Create long table
eq_table_all <- eq_table_2010a %>%
  bind_rows(eq_table_2011a) %>%
  bind_rows(eq_table_2012a) %>%
  bind_rows(eq_table_2013a) %>%
  bind_rows(eq_table_2014a) %>%
  bind_rows(eq_table_2015a) %>%
  bind_rows(eq_table_2016a) %>%
  bind_rows(eq_table_2017a) %>%
  bind_rows(eq_table_2018a) %>%
  bind_rows(eq_table_2019a) %>%
  bind_rows(eq_table_2020a) %>%
  bind_rows(eq_table_2021a)

# Check trend
eq_table_all %>%
  ggplot(aes(x = year, y = overall_policy_total, color = region)) +
  geom_line()

# SAVE DATA ---------------------------------------------------------------

# List of dataframes
df_list <- list(eq_table_2010a, eq_table_2011a, eq_table_2012a, eq_table_2013a, eq_table_2014a, eq_table_2015a, eq_table_2016a, eq_table_2017a, eq_table_2018a, eq_table_2019a, eq_table_2020a, eq_table_2021a)

# Vector of variables in WD
my_vars <- ls()

# Get names
df_list_names <- my_vars[str_detect(my_vars, regex("a$"))]

# Name the list elements
names(df_list) <- df_list_names

# Save files
for (i in 1:length(df_list)) {
  write_csv(df_list[[i]], 
            file = paste0("data/equality_tables/transformed/", names(df_list[i]), ".csv"))
}