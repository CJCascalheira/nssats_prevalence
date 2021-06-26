# Dependencies
library(tidyverse)
library(rvest)
library(xml2)
library(httr)

# Scoring criteria for images, 2010
# - green dot w/ T = 3
# - green dots = 2
# - orange and green dots = 1
# - orange dot = -1

# Set the URLs from the Web Archive and current web

# 31 January 2010
url_2010 <- "https://web.archive.org/web/20100118020603/http://www.lgbtmap.org/lgbstates/legal_lgb"

# 20 August 2011
url_2011 <- "https://web.archive.org/web/20111007134023/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"


url_2019 <- "https://www.lgbtmap.org/equality-maps/"

# Get the data
lgbt_map_2010 <- url_2010 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2011 <- url_2011 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2019 <- url_2019 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2020 <- url_2020 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2021 <- url_2021 %>% GET(., timeout(30)) %>% read_html()

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# 2010 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2010 <- html_node(lgbt_map_2010,
                          xpath = '//*[@id="body"]/div[2]/table[2]') 

# Prepare to convert to dataframe
equality_table <- tribble(
  ~"region",  ~"hate_crime",  ~"non_discrimination",  ~"relational_recognition",
  ~"parenting",  ~"schools",  ~"identity_docs",
  "0", "0", "0", "0", "0", "0", "0"
  )
equality_table

# Create the first sequence
node_sequence <- seq(1, 364, by = 7)
node_sequence

# Create the second sequence
node_sequence_2 <- seq(1, 364, by = 7) - 1
node_sequence_2 <- append(node_sequence_2[-1], 364)
node_sequence_2

# Extract HTML elements from each row
for (j in 1:length(node_sequence)) {
  col_i <- 0
  for (i in node_sequence[j]:node_sequence_2[j]) {
    col_i <- col_i + 1
    # Transform each table row into a string
    a <- section_2010 %>%
      html_nodes('td') %>%
      nth(i) %>%
      as.character()
    equality_table[j, col_i] <- a
  }
}

# Assign names and clean the table
equality_table_clean <- equality_table[-1, ] %>%
  # Clean columns
  mutate(
    # Clean region column
    region = str_extract(region, regex("(?<=<td height=\"30\">\\s)[[:print:]]*(?=</td>)", 
                                       ignore_case = TRUE)),
    # Clean hate crime column
    hate_crime = if_else(
      str_detect(hate_crime, 
                 regex("\n\t\t\t\n\t\t\t\n\t\t\t\t\t\t\t\t(?!<img)", ignore_case = TRUE)), "0",
      if_else(
        str_detect(hate_crime, regex("gr-dot-yel-bkgd-trans", ignore_case = TRUE)), "3",
        if_else(
          str_detect(hate_crime, regex("gr-dot-yel-bkgd", ignore_case = TRUE)), "2",
          if_else(
            str_detect(hate_crime, regex("gr-dot-wht-bkgd", ignore_case = TRUE)), "2", hate_crime
          )
        )
      )
    ),
    # Clean non-discrimination 
    non_discrimination = if_else(
      str_detect(non_discrimination, 
                 regex("\n\t\t\t\t\t\t\t\t(?!<img)", ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination, regex("gr-dot-yel-bkgd-trans", ignore_case = TRUE)), "3",
        if_else(
          str_detect(non_discrimination, regex("gr-dot-yel-bkgd", ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination, regex("gr-dot-wht-bkgd", ignore_case = TRUE)), "2",
            non_discrimination
          )
        )
      )
    ),
    # Clean relational recognition
    relational_recognition = if_else(
      str_detect(relational_recognition, 
                 regex("\n\t\t\t\t\t\t\t\t(?!<img)", ignore_case = TRUE)), "0",
      if_else(
        str_detect(relational_recognition, regex("gr-dot-yel-bkgd(?=.*20-20-red-wb)", 
                                                 ignore_case = TRUE)), "1",
        if_else(
          str_detect(relational_recognition, regex("gr-dot-wht-bkgd(?=.*20-20-red-dot)", 
                                                   ignore_case = TRUE)), "1",
          if_else(
            str_detect(relational_recognition, regex("gr-dot-yel-bkgd-trans", ignore_case = TRUE)), "3",
            if_else(
              str_detect(relational_recognition, regex("gr-dot-yel-bkgd", ignore_case = TRUE)), "2",
              if_else(
                str_detect(relational_recognition, regex("gr-dot-wht-bkgd", ignore_case = TRUE)), "2",
                if_else(
                  str_detect(relational_recognition, regex("20-20-red-wb", ignore_case = TRUE)), "-1",
                  if_else(
                    str_detect(relational_recognition, regex("20-20-red-dot", ignore_case = TRUE)), "-1",
                    relational_recognition
                  )
                )
              )
            )
          )
        )
      )
    ),
    # Clean parenting
    parenting = if_else(
      str_detect(parenting, 
                 regex("\n\t\t\t\t\t\t\t\t(?!<img)", ignore_case = TRUE)), "0",
      if_else(
        str_detect(parenting, regex("gr-dot-yel-bkgd(?=.*20-20-red-wb)", 
                                                 ignore_case = TRUE)), "1",
        if_else(
          str_detect(parenting, regex("gr-dot-wht-bkgd(?=.*20-20-red-dot)", 
                                                   ignore_case = TRUE)), "1",
          if_else(
            str_detect(parenting, regex("gr-dot-yel-bkgd-trans", ignore_case = TRUE)), "3",
            if_else(
              str_detect(parenting, regex("gr-dot-yel-bkgd", ignore_case = TRUE)), "2",
              if_else(
                str_detect(parenting, regex("gr-dot-wht-bkgd", ignore_case = TRUE)), "2",
                if_else(
                  str_detect(parenting, regex("20-20-red-wb", ignore_case = TRUE)), "-1",
                  if_else(
                    str_detect(parenting, regex("20-20-red-dot", ignore_case = TRUE)), "-1",
                    parenting
                  )
                )
              )
            )
          )
        )
      )
    ),
    # Clean schools
    schools = if_else(
      str_detect(schools, 
                 regex("\n\t\t\t\t\t\t\t\t(?!<img)", ignore_case = TRUE)), "0",
      if_else(
        str_detect(schools, regex("gr-dot-yel-bkgd(?=.*20-20-red-wb)", 
                                    ignore_case = TRUE)), "1",
        if_else(
          str_detect(schools, regex("gr-dot-wht-bkgd(?=.*20-20-red-dot)", 
                                      ignore_case = TRUE)), "1",
          if_else(
            str_detect(schools, regex("gr-dot-yel-bkgd-trans", ignore_case = TRUE)), "3",
            if_else(
              str_detect(schools, regex("gr-dot-yel-bkgd", ignore_case = TRUE)), "2",
              if_else(
                str_detect(schools, regex("gr-dot-wht-bkgd", ignore_case = TRUE)), "2",
                if_else(
                  str_detect(schools, regex("20-20-red-wb", ignore_case = TRUE)), "-1",
                  if_else(
                    str_detect(schools, regex("20-20-red-dot", ignore_case = TRUE)), "-1",
                    schools
                  )
                )
              )
            )
          )
        )
      )
    ),
    # Clean identity documents
    identity_docs = if_else(
      str_detect(identity_docs, 
                 regex("\n\t\t\t\t\t(?!<img)", ignore_case = TRUE)), "0",
      if_else(
        str_detect(identity_docs, regex("gr-dot-yel-bkgd(?=.*20-20-red-wb)", 
                                  ignore_case = TRUE)), "1",
        if_else(
          str_detect(identity_docs, regex("gr-dot-wht-bkgd(?=.*20-20-red-dot)", 
                                    ignore_case = TRUE)), "1",
          if_else(
            str_detect(identity_docs, regex("gr-dot-yel-bkgd-trans", ignore_case = TRUE)), "3",
            if_else(
              str_detect(identity_docs, regex("gr-dot-yel-bkgd", ignore_case = TRUE)), "2",
              if_else(
                str_detect(identity_docs, regex("gr-dot-wht-bkgd", ignore_case = TRUE)), "2",
                if_else(
                  str_detect(identity_docs, regex("20-20-red-wb", ignore_case = TRUE)), "-1",
                  if_else(
                    str_detect(identity_docs, regex("20-20-red-dot", ignore_case = TRUE)), "-1",
                    identity_docs
                  )
                )
              )
            )
          )
        )
      )
    )
  ) %>%
  # Turn scores to numeric
  mutate(across(hate_crime:identity_docs, as.numeric)) %>%
  # Lowercase state names
  mutate(region = tolower(region)) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2010.csv")

# 2011 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2011 <- html_node(lgbt_map_2011,
                          xpath = '//*[@id="data-tables"]/table') 

# Prepare to convert to dataframe
equality_table <- tribble(
  ~"region",  ~"relational_recognition", ~"non_discrimination_employment", ~"non_discrimination_housing", 
  ~"adopt_joint", ~"adopt_2ndparent", ~"adopt_stepparent", ~"schools", ~"hate_crime", ~"identity_docs",
  "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"
)
equality_table

# Create the first sequence
node_sequence <- seq(2, 511, by = 10)
node_sequence

# Create the second sequence
node_sequence_2 <- seq(2, 511, by = 10) - 1
node_sequence_2 <- append(node_sequence_2[-1], 511)
node_sequence_2

# Extract HTML elements from each row
for (j in 1:length(node_sequence)) {
  col_i <- 0
  for (i in node_sequence[j]:node_sequence_2[j]) {
    col_i <- col_i + 1
    # Transform each table row into a string
    a <- section_2011 %>%
      html_nodes('td') %>%
      nth(i) %>%
      as.character()
    equality_table[j, col_i] <- a
  }
}

# Assign names and clean the table
equality_table_clean <- equality_table %>%
  # Clean columns
  mutate(
    # Clean region column
    region = str_extract(region, regex("(?<=<td class=\"state-name\">)[[:print:]]*(?=</td>)", 
                                       ignore_case = TRUE)),
    # Clean relational recognition
    relational_recognition = if_else(
      str_detect(relational_recognition, 
                 regex("\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", ignore_case = TRUE)), "0",
      if_else(
        str_detect(relational_recognition, regex("positive-check(?=.*negative-check)", 
                                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(relational_recognition, regex("positive-check(?!-with-identity)", 
                                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(relational_recognition, regex("positive-check-with-identity", 
                                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(relational_recognition, regex("negative-check", ignore_case = TRUE)), "-1",
              relational_recognition
            )
          )
        )
      )
    ),
    # Clean non discrimination - employment
    non_discrimination_employment = if_else(
      str_detect(non_discrimination_employment, 
                 regex("\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination_employment, regex("positive-check(?=.*negative-check)", 
                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(non_discrimination_employment, regex("positive-check(?!-with-identity)", 
                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination_employment, regex("positive-check-with-identity", 
                                                     ignore_case = TRUE)), "3",
            if_else(
              str_detect(non_discrimination_employment, regex("negative-check", ignore_case = TRUE)), "-1",
              non_discrimination_employment
            )
          )
        )
      )
    ),
    # Clean non discrimination - housing
    non_discrimination_housing = if_else(
      str_detect(non_discrimination_housing, 
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t", ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination_housing, regex("positive-check(?=.*negative-check)", 
                                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(non_discrimination_housing, regex("positive-check(?!-with-identity)", 
                                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination_housing, regex("positive-check-with-identity", 
                                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(non_discrimination_housing, regex("negative-check", ignore_case = TRUE)), "-1",
              non_discrimination_housing
            )
          )
        )
      )
    ),
    # Clean joint adoption
    adopt_joint = if_else(
      str_detect(adopt_joint, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t", ignore_case = TRUE)), "0",
      if_else(
        str_detect(adopt_joint, regex("positive-check(?=.*negative-check)", 
                                                     ignore_case = TRUE)), "1",
        if_else(
          str_detect(adopt_joint, regex("positive-check(?!-with-identity)", 
                                                       ignore_case = TRUE)), "2",
          if_else(
            str_detect(adopt_joint, regex("positive-check-with-identity", 
                                                         ignore_case = TRUE)), "3",
            if_else(
              str_detect(adopt_joint, regex("negative-check", ignore_case = TRUE)), "-1",
              adopt_joint
            )
          )
        )
      )
    ),
    # Clean 2nd parent adoption
    adopt_2ndparent = if_else(
      str_detect(adopt_2ndparent, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t", ignore_case = TRUE)), "0",
      if_else(
        str_detect(adopt_2ndparent, regex("positive-check(?=.*negative-check)", 
                                      ignore_case = TRUE)), "1",
        if_else(
          str_detect(adopt_2ndparent, regex("positive-check(?!-with-identity)", 
                                        ignore_case = TRUE)), "2",
          if_else(
            str_detect(adopt_2ndparent, regex("positive-check-with-identity", 
                                          ignore_case = TRUE)), "3",
            if_else(
              str_detect(adopt_2ndparent, regex("negative-check", ignore_case = TRUE)), "-1",
              adopt_2ndparent
            )
          )
        )
      )
    ),
    # Clean step-parent adoption
    adopt_stepparent = if_else(
      str_detect(adopt_stepparent, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", ignore_case = TRUE)), "0",
      if_else(
        str_detect(adopt_stepparent, regex("positive-check(?=.*negative-check)", 
                                          ignore_case = TRUE)), "1",
        if_else(
          str_detect(adopt_stepparent, regex("positive-check(?!-with-identity)", 
                                            ignore_case = TRUE)), "2",
          if_else(
            str_detect(adopt_stepparent, regex("positive-check-with-identity", 
                                              ignore_case = TRUE)), "3",
            if_else(
              str_detect(adopt_stepparent, regex("negative-check", ignore_case = TRUE)), "-1",
              adopt_stepparent
            )
          )
        )
      )
    ),
    # Clean schools
    schools = if_else(
      str_detect(schools, 
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(schools, regex("positive-check(?=.*negative-check)", 
                                           ignore_case = TRUE)), "1",
        if_else(
          str_detect(schools, regex("positive-check(?!-with-identity)", 
                                             ignore_case = TRUE)), "2",
          if_else(
            str_detect(schools, regex("positive-check-with-identity", 
                                               ignore_case = TRUE)), "3",
            if_else(
              str_detect(schools, regex("negative-check", ignore_case = TRUE)), "-1",
              schools
            )
          )
        )
      )
    ),
    # Clean hate crimes
    hate_crime = if_else(
      str_detect(hate_crime, 
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(hate_crime, regex("positive-check(?=.*negative-check)", 
                                  ignore_case = TRUE)), "1",
        if_else(
          str_detect(hate_crime, regex("positive-check(?!-with-identity)", 
                                    ignore_case = TRUE)), "2",
          if_else(
            str_detect(hate_crime, regex("positive-check-with-identity", 
                                      ignore_case = TRUE)), "3",
            if_else(
              str_detect(hate_crime, regex("negative-check", ignore_case = TRUE)), "-1",
              hate_crime
            )
          )
        )
      )
    ),
    # Clean identity documents
    identity_docs = if_else(
      str_detect(identity_docs, 
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(identity_docs, regex("positive-check(?=.*negative-check)", 
                                     ignore_case = TRUE)), "1",
        if_else(
          str_detect(identity_docs, regex("positive-check(?!-with-identity)", 
                                       ignore_case = TRUE)), "2",
          if_else(
            str_detect(identity_docs, regex("positive-check-with-identity", 
                                         ignore_case = TRUE)), "3",
            if_else(
              str_detect(identity_docs, regex("negative-check", ignore_case = TRUE)), "-1",
              identity_docs
            )
          )
        )
      )
    )
  ) %>%
  # Turn scores to numeric
  mutate(across(relational_recognition:identity_docs, as.numeric)) %>%
  # Lowercase state names
  mutate(region = tolower(region)) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2011.csv")

# 2019 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2019 <- html_node(lgbt_map_2019,
                                  xpath='//*[@id="map-4"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2019)
head(equality_table)

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
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2019.csv")
