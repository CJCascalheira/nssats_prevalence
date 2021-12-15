# Load dependencies
library(tidyverse)

# IMPORT DATA -------------------------------------------------------------

# Import hate crime
hate_2019 <- read_csv("data/cleaned/hate_crimes/hate_2019.csv")

# Import LGBTQ+ population
pop_table_2019 <- read_csv("data/cleaned/lgbtq_pop/pop_table_2019.csv")

# Import equality table - in original scaling
equality_2019 <- read_csv("data/equality_tables/equality_table_clean_2019.csv") %>%
  select(region, state, overall_policy_total) %>%
  distinct(region, .keep_all = TRUE)

# Import N-SSATS
nssats_2019 <- read_csv("data/cleaned/nssats/nssats_2019a.csv")

# Import state partisan control
partisan_2019 <- read_csv("data/state_partisan_control/partisan_control_2019.csv") %>%
  rename(partisan_state = control)

# Import HRC Congressional score card for the 116th U.S. Congress
cong_116_scores <- read_csv("data/hrc_cong_score/cong_116_scores.csv")

# Combine the data
cross_section <- nssats_2019 %>%
  left_join(equality_2019) %>%
  left_join(pop_table_2019) %>%
  left_join(hate_2019) %>%
  left_join(partisan_2019) %>%
  left_join(cong_116_scores, by = c("region" = "state")) %>%
  rename(state_policy = overall_policy_total)
cross_section

# PRE-PROCESS -------------------------------------------------------------

# Scatterplot of population and LGBTQ+ programming
plot(cross_section$lgbtq_pop_total, cross_section$lgbtq_total)
plot(cross_section$lgbtq_pop_perc, cross_section$lgbtq_total)
plot(log(cross_section$lgbtq_pop_total), cross_section$lgbtq_total)

# Drop the outlier
cross_section_1 <- cross_section %>%
  filter(lgbtq_total < 400)
plot(cross_section_1$lgbtq_pop_total, cross_section_1$lgbtq_total)
plot(cross_section_1$lgbtq_pop_perc, cross_section_1$lgbtq_total)
plot(log(cross_section_1$lgbtq_pop_total), cross_section_1$lgbtq_total)

# REGRESSION --------------------------------------------------------------

# Regression - with outlier
summary(lm(lgbtq_total ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund + 
             partisan_state + cong_116_score, 
           data = cross_section))

# Regression - without outlier
summary(lm(lgbtq_total ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund + 
             partisan_state + cong_116_score, 
           data = cross_section_1))
