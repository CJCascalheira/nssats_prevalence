# Dependencies
library(tidyverse)
library(rvest)
library(xml2)
library(httr)

# Scoring criteria for images, 2010
# - green dot w/ T | green pill w/ LGBT = 3
# - green dots | green pill w/ LGB = 2
# - orange and green dots = 1
# - orange dot = -1

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# GET URLS AND HTML DOCUMENTS ---------------------------------------------

# Set the URLs from the Web Archive and current web

# 31 January 2010
url_2010 <- "https://web.archive.org/web/20100118020603/http://www.lgbtmap.org/lgbstates/legal_lgb"

# 07 October 2011
url_2011 <- "https://web.archive.org/web/20111007134023/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"

# 31 December 2011
url_2012 <- "https://web.archive.org/web/20111231193748/http://lgbtmap.org/equality-maps/legal_equality_by_state"

# 28 February 2014
url_2013 <- "https://web.archive.org/web/20140228011753/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"

# 10 October 2014
url_2014 <- "https://web.archive.org/web/20141010032249/http://lgbtmap.org/equality-maps/legal_equality_by_state"

# 06 September 2015
url_2015 <- "https://web.archive.org/web/20150906221231/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"

# 21 September 2016
url_2016 <- "https://web.archive.org/web/20160921173312/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"

# 03 September 2017
url_2017 <- "https://web.archive.org/web/20170903172034/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"

# 01 October 2018
url_2018 <- "https://web.archive.org/web/20181001234708/http://lgbtmap.org/equality-maps/legal_equality_by_state"

# 29 August 2019
url_2019 <- "https://web.archive.org/web/20190829175625/http://www.lgbtmap.org/equality-maps/legal_equality_by_state"

# 31 August 2020
url_2020 <- "https://web.archive.org/web/20200831163747/https://www.lgbtmap.org/equality-maps/"

# 24 June 2021
url_2021 <- "https://web.archive.org/web/20210624132135/https://www.lgbtmap.org/equality-maps"

# Get the data
lgbt_map_2010 <- url_2010 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2011 <- url_2011 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2012 <- url_2012 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2013 <- url_2013 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2014 <- url_2014 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2015 <- url_2015 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2016 <- url_2016 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2017 <- url_2017 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2018 <- url_2018 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2019 <- url_2019 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2020 <- url_2020 %>% GET(., timeout(30)) %>% read_html()
lgbt_map_2021 <- url_2021 %>% GET(., timeout(30)) %>% read_html()

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

# 2012 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2012 <- html_node(lgbt_map_2012,
                          xpath = '//*[@id="data-tables"]/table[2]') 

# Prepare to convert to dataframe
equality_table <- tribble(
  ~"region",  ~"relational_recognition",  ~"adopt_joint", ~"adopt_2ndparent", ~"adopt_stepparent", ~"foster_care", ~"parental_recognition", ~"de_facto_parenting", ~"non_discrimination_employment", ~"non_discrimination_housing", ~"schools", ~"hate_crime", ~"identity_docs", ~"medical_decisions", ~"medical_leave",
  "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"
)
equality_table

# Create the first sequence
node_sequence <- seq(1, 765, by = 15)
node_sequence

# Create the second sequence
node_sequence_2 <- seq(1, 765, by = 15) - 1
node_sequence_2 <- append(node_sequence_2[-1], 765)
node_sequence_2

# Extract HTML elements from each row
for (j in 1:length(node_sequence)) {
  
  # Set column count to 0
  col_i <- 0
  for (i in node_sequence[j]:node_sequence_2[j]) {
    # Increase the column count to populate each cell
    col_i <- col_i + 1
    
    # Transform each table row into a string
    a <- section_2012 %>%
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
    # Clean foster care
    foster_care = if_else(
      str_detect(foster_care, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t</td>",
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(foster_care, regex("positive-check(?=.*negative-check)", 
                                                 ignore_case = TRUE)), "1",
        if_else(
          str_detect(foster_care, regex("positive-check(?!-with-identity)", 
                                                   ignore_case = TRUE)), "2",
          if_else(
            str_detect(foster_care, regex("positive-check-with-identity", 
                                                     ignore_case = TRUE)), "3",
            if_else(
              str_detect(foster_care, regex("negative-check", ignore_case = TRUE)), "-1",
              foster_care
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
              str_detect(non_discrimination_housing, regex("negative-check", 
                                                           ignore_case = TRUE)), "-1",
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
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t</td>", 
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
    ),
    # Clean medical decision-making
    medical_decisions = if_else(
      str_detect(medical_decisions, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(medical_decisions, regex("positive-check(?=.*negative-check)", 
                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(medical_decisions, regex("positive-check(?!-with-identity)", 
                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(medical_decisions, regex("positive-check-with-identity", 
                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(medical_decisions, regex("negative-check", ignore_case = TRUE)), "-1",
              medical_decisions
            )
          )
        )
      )
    ),
    # Clean state medical leave
    medical_leave = if_else(
      str_detect(medical_leave, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(medical_leave, regex("positive-check(?=.*negative-check)", 
                                            ignore_case = TRUE)), "1",
        if_else(
          str_detect(medical_leave, regex("positive-check(?!-with-identity)", 
                                              ignore_case = TRUE)), "2",
          if_else(
            str_detect(medical_leave, regex("positive-check-with-identity", 
                                                ignore_case = TRUE)), "3",
            if_else(
              str_detect(medical_leave, regex("negative-check", ignore_case = TRUE)), "-1",
              medical_leave
            )
          )
        )
      )
    ),
    # Clean parental recognition
    parental_recognition = if_else(
      str_detect(parental_recognition, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(parental_recognition, regex("positive-check(?=.*negative-check)", 
                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(parental_recognition, regex("positive-check(?!-with-identity)", 
                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(parental_recognition, regex("positive-check-with-identity", 
                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(parental_recognition, regex("negative-check", ignore_case = TRUE)), "-1",
              parental_recognition
            )
          )
        )
      )
    ),
    # Clean de facto parenting
    de_facto_parenting = if_else(
      str_detect(de_facto_parenting, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(de_facto_parenting, regex("positive-check(?=.*negative-check)", 
                                               ignore_case = TRUE)), "1",
        if_else(
          str_detect(de_facto_parenting, regex("positive-check(?!-with-identity)", 
                                                 ignore_case = TRUE)), "2",
          if_else(
            str_detect(de_facto_parenting, regex("positive-check-with-identity", 
                                                   ignore_case = TRUE)), "3",
            if_else(
              str_detect(de_facto_parenting, regex("negative-check", ignore_case = TRUE)), "-1",
              de_facto_parenting
            )
          )
        )
      )
    )
  ) %>%
  # Turn scores to numeric
  mutate(across(relational_recognition:medical_leave, as.numeric)) %>%
  # Lowercase state names
  mutate(region = tolower(region)) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2012.csv")

# 2013 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2013 <- html_node(lgbt_map_2013,
                          xpath = '//*[@id="data-tables"]/div/table[2]') 

# Prepare to convert to dataframe
equality_table <- tribble(
  ~"region",  ~"relational_recognition",  ~"adopt_joint", ~"adopt_2ndparent", ~"adopt_stepparent", ~"foster_care", ~"donor_insemination", ~"de_facto_parenting", ~"schools", ~"hate_crime", ~"non_discrimination_employment", ~"non_discrimination_housing", ~"non_discrimination_insurance", ~"non_discrimination_public_accom", ~"identity_docs", ~"medical_decisions", ~"medical_leave", ~"hiv_criminalization",
  "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"
)
equality_table

# Create the first sequence
node_sequence <- seq(1, 918, by = 18)
node_sequence

# Create the second sequence
node_sequence_2 <- seq(1, 918, by = 18) - 1
node_sequence_2 <- append(node_sequence_2[-1], 918)
node_sequence_2

# Extract HTML elements from each row
for (j in 1:length(node_sequence)) {
  
  # Set column count to 0
  col_i <- 0
  for (i in node_sequence[j]:node_sequence_2[j]) {
    # Increase the column count to populate each cell
    col_i <- col_i + 1
    
    # Transform each table row into a string
    a <- section_2013 %>%
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
    # Clean foster care
    foster_care = if_else(
      str_detect(foster_care, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t</td>",
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(foster_care, regex("positive-check(?=.*negative-check)", 
                                      ignore_case = TRUE)), "1",
        if_else(
          str_detect(foster_care, regex("positive-check(?!-with-identity)", 
                                        ignore_case = TRUE)), "2",
          if_else(
            str_detect(foster_care, regex("positive-check-with-identity", 
                                          ignore_case = TRUE)), "3",
            if_else(
              str_detect(foster_care, regex("negative-check", ignore_case = TRUE)), "-1",
              foster_care
            )
          )
        )
      )
    ),
    # Clean non discrimination - employment
    non_discrimination_employment = if_else(
      str_detect(non_discrimination_employment, 
                 regex("\n\t                                \n\t                            \t                        </td>", ignore_case = TRUE)), "0",
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
              if_else(
                str_detect(non_discrimination_employment, regex("lgbt-pill", 
                                                             ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_employment, regex("lgb-dotted-pill", 
                                                               ignore_case = TRUE)), "2",
                  non_discrimination_employment
                )
              )
            )
          )
        )
      )
    ),
    # Clean non discrimination - housing
    non_discrimination_housing = if_else(
      str_detect(non_discrimination_housing, 
                 regex("\n\t                                \n\t                            \t                        </td>", ignore_case = TRUE)), "0",
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
              str_detect(non_discrimination_housing, regex("negative-check", 
                                                           ignore_case = TRUE)), "-1",
              if_else(
                str_detect(non_discrimination_housing, regex("lgbt-pill", 
                                                                  ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_housing, regex("lgb-dotted-pill", 
                                                                    ignore_case = TRUE)), "2",
                  non_discrimination_housing
                )
              )
            )
          )
        )
      )
    ),
    # Clean non-discrimination insurance
    non_discrimination_insurance = if_else(
      str_detect(non_discrimination_insurance, 
                 regex("\n\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination_insurance, regex("positive-check(?=.*negative-check)", 
                                                          ignore_case = TRUE)), "1",
        if_else(
          str_detect(non_discrimination_insurance, regex("positive-check(?!-with-identity)", 
                                                            ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination_insurance, regex("positive-check-with-identity", 
                                                              ignore_case = TRUE)), "3",
            if_else(
              str_detect(non_discrimination_insurance, regex("negative-check", 
                                                                ignore_case = TRUE)), "-1",
              if_else(
                str_detect(non_discrimination_insurance, regex("lgbt-pill", 
                                                                  ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_insurance, regex("lgb-dotted-pill", 
                                                                    ignore_case = TRUE)), "2",
                  non_discrimination_insurance
                )
              )
            )
          )
        )
      )
    ),
    # Clean non-discrimination public accommodations
    non_discrimination_public_accom = if_else(
      str_detect(non_discrimination_public_accom, 
                 regex("\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination_public_accom, regex("positive-check(?=.*negative-check)", 
                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(non_discrimination_public_accom, regex("positive-check(?!-with-identity)", 
                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination_public_accom, regex("positive-check-with-identity", 
                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(non_discrimination_public_accom, regex("negative-check", 
                                                                ignore_case = TRUE)), "-1",
              if_else(
                str_detect(non_discrimination_public_accom, regex("lgbt-pill", 
                                                                  ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_public_accom, regex("lgb-dotted-pill", 
                                                                    ignore_case = TRUE)), "2",
                  non_discrimination_public_accom
                )
              )
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
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
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
              if_else(
                str_detect(schools, regex("lgbt-pill", ignore_case = TRUE)), "3",
                if_else(
                  str_detect(schools, regex("lgb-dotted-pill", ignore_case = TRUE)), "2",
                  schools
                )
              )
            )
          )
        )
      )
    ),
    # Clean hate crimes
    hate_crime = if_else(
      str_detect(hate_crime, 
                 regex("\n\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
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
              if_else(
                str_detect(hate_crime, regex("lgbt-pill", ignore_case = TRUE)), "3",
                if_else(
                  str_detect(hate_crime, regex("lgb-dotted-pill", ignore_case = TRUE)), "2",
                  hate_crime
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
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t</td>", 
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
              if_else(
                str_detect(identity_docs, regex("lgbt-pill", ignore_case = TRUE)), "3",
                if_else(
                  str_detect(identity_docs, regex("lgb-dotted-pill", ignore_case = TRUE)), "2",
                  identity_docs
                )
              )
            )
          )
        )
      )
    ),
    # Clean medical decision-making
    medical_decisions = if_else(
      str_detect(medical_decisions, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(medical_decisions, regex("positive-check(?=.*negative-check)", 
                                            ignore_case = TRUE)), "1",
        if_else(
          str_detect(medical_decisions, regex("positive-check(?!-with-identity)", 
                                              ignore_case = TRUE)), "2",
          if_else(
            str_detect(medical_decisions, regex("positive-check-with-identity", 
                                                ignore_case = TRUE)), "3",
            if_else(
              str_detect(medical_decisions, regex("negative-check", ignore_case = TRUE)), "-1",
              medical_decisions
            )
          )
        )
      )
    ),
    # Clean state medical leave
    medical_leave = if_else(
      str_detect(medical_leave, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(medical_leave, regex("positive-check(?=.*negative-check)", 
                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(medical_leave, regex("positive-check(?!-with-identity)", 
                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(medical_leave, regex("positive-check-with-identity", 
                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(medical_leave, regex("negative-check", ignore_case = TRUE)), "-1",
              medical_leave
            )
          )
        )
      )
    ),
    # Clean parental recognition
    donor_insemination = if_else(
      str_detect(donor_insemination, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(donor_insemination, regex("positive-check(?=.*negative-check)", 
                                               ignore_case = TRUE)), "1",
        if_else(
          str_detect(donor_insemination, regex("positive-check(?!-with-identity)", 
                                                 ignore_case = TRUE)), "2",
          if_else(
            str_detect(donor_insemination, regex("positive-check-with-identity", 
                                                   ignore_case = TRUE)), "3",
            if_else(
              str_detect(donor_insemination, regex("negative-check", ignore_case = TRUE)), "-1",
              donor_insemination
            )
          )
        )
      )
    ),
    # Clean de facto parenting
    de_facto_parenting = if_else(
      str_detect(de_facto_parenting, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(de_facto_parenting, regex("positive-check(?=.*negative-check)", 
                                             ignore_case = TRUE)), "1",
        if_else(
          str_detect(de_facto_parenting, regex("positive-check(?!-with-identity)", 
                                               ignore_case = TRUE)), "2",
          if_else(
            str_detect(de_facto_parenting, regex("positive-check-with-identity", 
                                                 ignore_case = TRUE)), "3",
            if_else(
              str_detect(de_facto_parenting, regex("negative-check", ignore_case = TRUE)), "-1",
              de_facto_parenting
            )
          )
        )
      )
    ),
    # Clean HIV criminalization
    hiv_criminalization = if_else(
      str_detect(hiv_criminalization, 
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(hiv_criminalization, regex("positive-check(?=.*negative-check)", 
                                             ignore_case = TRUE)), "1",
        if_else(
          str_detect(hiv_criminalization, regex("positive-check(?!-with-identity)", 
                                               ignore_case = TRUE)), "2",
          if_else(
            str_detect(hiv_criminalization, regex("positive-check-with-identity", 
                                                 ignore_case = TRUE)), "3",
            if_else(
              str_detect(hiv_criminalization, regex("negative-check", ignore_case = TRUE)), "-1",
              hiv_criminalization
            )
          )
        )
      )
    )
  ) %>%
  # Turn scores to numeric
  mutate(across(relational_recognition:hiv_criminalization, as.numeric)) %>%
  # Lowercase state names
  mutate(region = tolower(region)) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2013.csv")

# 2014 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2014 <- html_node(lgbt_map_2014,
                          xpath = '//*[@id="data-tables"]/div/table[2]') 

# Prepare to convert to dataframe
equality_table <- tribble(
  ~"region",  ~"relational_recognition",  ~"adopt_joint", ~"adopt_2ndparent", ~"adopt_stepparent", ~"foster_care", ~"donor_insemination", ~"de_facto_parenting", ~"schools", ~"hate_crime", ~"non_discrimination_employment", ~"non_discrimination_housing", ~"non_discrimination_insurance", ~"non_discrimination_public_accom", ~"identity_docs", ~"medical_decisions", ~"medical_leave", ~"hiv_criminalization",
  "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"
)
equality_table

# Create the first sequence
node_sequence <- seq(1, 918, by = 18)
node_sequence

# Create the second sequence
node_sequence_2 <- seq(1, 918, by = 18) - 1
node_sequence_2 <- append(node_sequence_2[-1], 918)
node_sequence_2

# Extract HTML elements from each row
for (j in 1:length(node_sequence)) {
  
  # Set column count to 0
  col_i <- 0
  for (i in node_sequence[j]:node_sequence_2[j]) {
    # Increase the column count to populate each cell
    col_i <- col_i + 1
    
    # Transform each table row into a string
    a <- section_2014 %>%
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
    # Clean foster care
    foster_care = if_else(
      str_detect(foster_care, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t</td>",
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(foster_care, regex("positive-check(?=.*negative-check)", 
                                      ignore_case = TRUE)), "1",
        if_else(
          str_detect(foster_care, regex("positive-check(?!-with-identity)", 
                                        ignore_case = TRUE)), "2",
          if_else(
            str_detect(foster_care, regex("positive-check-with-identity", 
                                          ignore_case = TRUE)), "3",
            if_else(
              str_detect(foster_care, regex("negative-check", ignore_case = TRUE)), "-1",
              foster_care
            )
          )
        )
      )
    ),
    # Clean non discrimination - employment
    non_discrimination_employment = if_else(
      str_detect(non_discrimination_employment, 
                 regex("\n\t                                \n\t                            \t                        </td>", ignore_case = TRUE)), "0",
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
              if_else(
                str_detect(non_discrimination_employment, regex("lgbt-pill", 
                                                                ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_employment, regex("lgb-dotted-pill", 
                                                                  ignore_case = TRUE)), "2",
                  non_discrimination_employment
                )
              )
            )
          )
        )
      )
    ),
    # Clean non discrimination - housing
    non_discrimination_housing = if_else(
      str_detect(non_discrimination_housing, 
                 regex("\n\t                                \n\t                            \t                        </td>", ignore_case = TRUE)), "0",
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
              str_detect(non_discrimination_housing, regex("negative-check", 
                                                           ignore_case = TRUE)), "-1",
              if_else(
                str_detect(non_discrimination_housing, regex("lgbt-pill", 
                                                             ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_housing, regex("lgb-dotted-pill", 
                                                               ignore_case = TRUE)), "2",
                  non_discrimination_housing
                )
              )
            )
          )
        )
      )
    ),
    # Clean non-discrimination insurance
    non_discrimination_insurance = if_else(
      str_detect(non_discrimination_insurance, 
                 regex("\n\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination_insurance, regex("positive-check(?=.*negative-check)", 
                                                       ignore_case = TRUE)), "1",
        if_else(
          str_detect(non_discrimination_insurance, regex("positive-check(?!-with-identity)", 
                                                         ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination_insurance, regex("positive-check-with-identity", 
                                                           ignore_case = TRUE)), "3",
            if_else(
              str_detect(non_discrimination_insurance, regex("negative-check", 
                                                             ignore_case = TRUE)), "-1",
              if_else(
                str_detect(non_discrimination_insurance, regex("lgbt-pill", 
                                                               ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_insurance, regex("lgb-dotted-pill", 
                                                                 ignore_case = TRUE)), "2",
                  non_discrimination_insurance
                )
              )
            )
          )
        )
      )
    ),
    # Clean non-discrimination public accommodations
    non_discrimination_public_accom = if_else(
      str_detect(non_discrimination_public_accom, 
                 regex("\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(non_discrimination_public_accom, regex("positive-check(?=.*negative-check)", 
                                                          ignore_case = TRUE)), "1",
        if_else(
          str_detect(non_discrimination_public_accom, regex("positive-check(?!-with-identity)", 
                                                            ignore_case = TRUE)), "2",
          if_else(
            str_detect(non_discrimination_public_accom, regex("positive-check-with-identity", 
                                                              ignore_case = TRUE)), "3",
            if_else(
              str_detect(non_discrimination_public_accom, regex("negative-check", 
                                                                ignore_case = TRUE)), "-1",
              if_else(
                str_detect(non_discrimination_public_accom, regex("lgbt-pill", 
                                                                  ignore_case = TRUE)), "3",
                if_else(
                  str_detect(non_discrimination_public_accom, regex("lgb-dotted-pill", 
                                                                    ignore_case = TRUE)), "2",
                  non_discrimination_public_accom
                )
              )
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
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
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
              if_else(
                str_detect(schools, regex("lgbt-pill", ignore_case = TRUE)), "3",
                if_else(
                  str_detect(schools, regex("lgb-dotted-pill", ignore_case = TRUE)), "2",
                  schools
                )
              )
            )
          )
        )
      )
    ),
    # Clean hate crimes
    hate_crime = if_else(
      str_detect(hate_crime, 
                 regex("\n\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
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
              if_else(
                str_detect(hate_crime, regex("lgbt-pill", ignore_case = TRUE)), "3",
                if_else(
                  str_detect(hate_crime, regex("lgb-dotted-pill", ignore_case = TRUE)), "2",
                  hate_crime
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
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t</td>", 
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
              if_else(
                str_detect(identity_docs, regex("lgbt-pill", ignore_case = TRUE)), "3",
                if_else(
                  str_detect(identity_docs, regex("lgb-dotted-pill", ignore_case = TRUE)), "2",
                  identity_docs
                )
              )
            )
          )
        )
      )
    ),
    # Clean medical decision-making
    medical_decisions = if_else(
      str_detect(medical_decisions, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(medical_decisions, regex("positive-check(?=.*negative-check)", 
                                            ignore_case = TRUE)), "1",
        if_else(
          str_detect(medical_decisions, regex("positive-check(?!-with-identity)", 
                                              ignore_case = TRUE)), "2",
          if_else(
            str_detect(medical_decisions, regex("positive-check-with-identity", 
                                                ignore_case = TRUE)), "3",
            if_else(
              str_detect(medical_decisions, regex("negative-check", ignore_case = TRUE)), "-1",
              medical_decisions
            )
          )
        )
      )
    ),
    # Clean state medical leave
    medical_leave = if_else(
      str_detect(medical_leave, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(medical_leave, regex("positive-check(?=.*negative-check)", 
                                        ignore_case = TRUE)), "1",
        if_else(
          str_detect(medical_leave, regex("positive-check(?!-with-identity)", 
                                          ignore_case = TRUE)), "2",
          if_else(
            str_detect(medical_leave, regex("positive-check-with-identity", 
                                            ignore_case = TRUE)), "3",
            if_else(
              str_detect(medical_leave, regex("negative-check", ignore_case = TRUE)), "-1",
              medical_leave
            )
          )
        )
      )
    ),
    # Clean parental recognition
    donor_insemination = if_else(
      str_detect(donor_insemination, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(donor_insemination, regex("positive-check(?=.*negative-check)", 
                                             ignore_case = TRUE)), "1",
        if_else(
          str_detect(donor_insemination, regex("positive-check(?!-with-identity)", 
                                               ignore_case = TRUE)), "2",
          if_else(
            str_detect(donor_insemination, regex("positive-check-with-identity", 
                                                 ignore_case = TRUE)), "3",
            if_else(
              str_detect(donor_insemination, regex("negative-check", ignore_case = TRUE)), "-1",
              donor_insemination
            )
          )
        )
      )
    ),
    # Clean de facto parenting
    de_facto_parenting = if_else(
      str_detect(de_facto_parenting, 
                 regex("\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(de_facto_parenting, regex("positive-check(?=.*negative-check)", 
                                             ignore_case = TRUE)), "1",
        if_else(
          str_detect(de_facto_parenting, regex("positive-check(?!-with-identity)", 
                                               ignore_case = TRUE)), "2",
          if_else(
            str_detect(de_facto_parenting, regex("positive-check-with-identity", 
                                                 ignore_case = TRUE)), "3",
            if_else(
              str_detect(de_facto_parenting, regex("negative-check", ignore_case = TRUE)), "-1",
              de_facto_parenting
            )
          )
        )
      )
    ),
    # Clean HIV criminalization
    hiv_criminalization = if_else(
      str_detect(hiv_criminalization, 
                 regex("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t</td>", 
                       ignore_case = TRUE)), "0",
      if_else(
        str_detect(hiv_criminalization, regex("positive-check(?=.*negative-check)", 
                                              ignore_case = TRUE)), "1",
        if_else(
          str_detect(hiv_criminalization, regex("positive-check(?!-with-identity)", 
                                                ignore_case = TRUE)), "2",
          if_else(
            str_detect(hiv_criminalization, regex("positive-check-with-identity", 
                                                  ignore_case = TRUE)), "3",
            if_else(
              str_detect(hiv_criminalization, regex("negative-check", ignore_case = TRUE)), "-1",
              hiv_criminalization
            )
          )
        )
      )
    )
  ) %>%
  # Turn scores to numeric
  mutate(across(relational_recognition:hiv_criminalization, as.numeric)) %>%
  # Lowercase state names
  mutate(region = tolower(region)) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2014.csv")

# 2015 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2015 <- html_node(lgbt_map_2015,
                          xpath='//*[@id="data-tables"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2015)
head(equality_table)
equality_table <- equality_table[, -1]

# Names fo variables
table_names <- c(
  "region",
  "so_gi", 
  "relational_recognition",
  "adoption_parenting",
  "non_discrimination", 
  "lgbt_youth", 
  "healthcare", 
  "identity_docs", 
  "so_policy_total", 
  "gi_policy_total", 
  "overall_policy_total"
)

# Assign names
colnames(equality_table) <- table_names
equality_table

# Prepare the dataframe with state abbrevations for export
equality_table_clean <- equality_table %>%
  mutate(
    # Convert state names to lower case
    region = tolower(region)
  ) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2015.csv")

# 2016 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2016 <- html_node(lgbt_map_2016,
                          xpath='//*[@id="data-tables"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2016)
head(equality_table)
equality_table <- equality_table[, -1]
equality_table

# Names fo variables
table_names <- c(
  "region",
  "so_gi", 
  "relational_recognition",
  "adoption_parenting",
  "non_discrimination", 
  "lgbt_youth", 
  "healthcare", 
  "identity_docs", 
  "so_policy_total", 
  "gi_policy_total", 
  "overall_policy_total"
)

# Assign names
colnames(equality_table) <- table_names
equality_table

# Prepare the dataframe with state abbrevations for export
equality_table_clean <- equality_table %>%
  mutate(
    # Convert state names to lower case
    region = tolower(region)
  ) %>%
  # Abbreviations for states
  left_join(state_names) %>%
  # Rearrange df
  select(region, state, everything())
equality_table_clean

# Export equality table
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2016.csv")

# 2017 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2017 <- html_node(lgbt_map_2017,
                          xpath='//*[@id="map-4"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2017)
head(equality_table)

# Assign names and prepare table for export
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
    "marriage_recognition" = X4,
    "relational_recognition" = X5, 
    "non_discrimination" = X6, 
    "religious_exemption" = X7,
    "lgbt_youth" = X8, 
    "healthcare" = X9, 
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
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2017.csv")

# 2018 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2018 <- html_node(lgbt_map_2018,
                          xpath='//*[@id="map-4"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2018)
head(equality_table)

# Assign names and prepare table for export
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
    "marriage_recognition" = X4,
    "relational_recognition" = X5, 
    "non_discrimination" = X6, 
    "religious_exemption" = X7,
    "lgbt_youth" = X8, 
    "healthcare" = X9, 
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
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2018.csv")

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

# 2020 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2020 <- html_node(lgbt_map_2020,
                          xpath='//*[@id="map-4"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2020)
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
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2020.csv")

# 2021 --------------------------------------------------------------------

# HTML code for lgbt_map table
section_2021 <- html_node(lgbt_map_2021,
                          xpath='//*[@id="map-4"]/div/table') 

# Covert to table in R
equality_table <- html_table(section_2021)
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
write_csv(equality_table_clean, file = "data/equality_tables/equality_table_clean_2021.csv")
