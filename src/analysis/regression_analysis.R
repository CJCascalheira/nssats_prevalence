# Dependencies
library(psych)
library(car)
library(MASS)
library(KScorrect)
library(tidyverse)
library(rio)
library(sjPlot)
library(boot)

# Remove scientific notation
options(scipen = 999)

# Reproducibility
set.seed(1234567)

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

# Import N-MHSS
nmhss_2019 <- read_csv("data/cleaned/nmhss/nmhss_2019a.csv") %>%
  rename(region = state, govt_fund = govt_total)

# Import correction to SGM-tailored programming
confirmed_sgm <- read_csv("data/cleaned/confirmed_sgm_tailored_programming.csv") %>%
  select(region, perc_confirmed)

# Import state partisan control
partisan_2019 <- read_csv("data/state_partisan_control/partisan_control_2019.csv") %>%
  rename(partisan_state = control)

# Import HRC Congressional score card for the 116th U.S. Congress
cong_116_scores <- read_csv("data/hrc_cong_score/cong_116_scores.csv")

# Combine the data - N-SSATS
cross_section_nssats <- nssats_2019 %>%
  left_join(equality_2019) %>%
  left_join(pop_table_2019) %>%
  left_join(hate_2019) %>%
  left_join(partisan_2019) %>%
  left_join(cong_116_scores, by = c("region" = "state")) %>%
  rename(state_policy = overall_policy_total) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed
  ) %>%
  select(-senate_score, -house_score, -lgbtq_pop_perc, -perc_confirmed, -state) %>%
  select(region, lgbtq_actual, everything())
cross_section_nssats

# Combine the data - N-MHSS
cross_section_nmhss <- nmhss_2019 %>%
  left_join(equality_2019) %>%
  left_join(pop_table_2019) %>%
  left_join(hate_2019) %>%
  left_join(partisan_2019) %>%
  left_join(cong_116_scores, by = c("region" = "state")) %>%
  rename(state_policy = overall_policy_total) %>%
  select(-senate_score, -house_score, -lgbtq_pop_perc)
cross_section_nmhss

# MISSING VALUES ----------------------------------------------------------

# Any missing values?
nrow(cross_section_nssats) - nrow(complete.cases(cross_section_nssats))
nrow(cross_section_nmhss) - nrow(complete.cases(cross_section_nmhss))

# FUNCTIONS ---------------------------------------------------------------

# To interpret the output
help(summary.boot)

# Function to calculate R^2 
# https://www.statmethods.net/advstats/bootstrapping.html
rsq_function <- function(formula, data, indices) {
  # Allows boot to select sample
  d <- data[indices, ]
  
  # Fit regression model
  fit <- lm(formula, data = d)
  
  # Return R-squared of model
  return(summary(fit)$r.square)
}

# Function to calculate AIC
get_aic <- function(formula, data, indices) {
  # Allows boot to select sample
  d <- data[indices, ]
  
  # Fit regression model
  fit <- lm(formula, data = d)
  
  # Calculate AIC
  aic <- nrow(data) * log((sum(fit$residuals^2)) / (nrow(data))) + (2 * length(fit$coefficients))
  
  # Return the AIC
  return(aic)
}

# REGULAR REGRESSION ------------------------------------------------------

# Regression - N-SSATS
summary(lm(lgbtq_actual ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund + 
             partisan_state + cong_116_score, 
           data = cross_section_nssats))

# Regression - N-MHSS
summary(lm(lgbtq_total ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund + 
             partisan_state + cong_116_score, 
           data = cross_section_nmhss))

# BOOTSTRAP REGRESSION: N-SSATS -------------------------------------------

# Model
model_nssats <- lm(lgbtq_actual ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund +
                     partisan_state + cong_116_score, 
                   data = cross_section_nssats)

# Bootstrap estimation
model_nssats_boot <- Boot(model_nssats, R = 5000, method = "case")

# Summarize the model
model_nssats_sumry <- summary(model_nssats_boot)
model_nssats_sumry

# Confidence intervals for model estimates
confint(model_nssats_boot, level = .95, type = "norm")

# Get t-statistics for each coefficient
model_nssats_tstat <- data.frame(
  # Pull the coefficients and the standard errors
  coeff = model_nssats_sumry$bootMed, 
  se = model_nssats_sumry$bootSE
) %>%
  # Calculate the standard t-statistic
  mutate(t_stat = coeff / se) %>%
  # Calculate the p-value of the t-statistic
  mutate(t_p_value = 2 * pt(q = abs(t_stat), df = (nrow(cross_section_nssats) - 1), lower.tail = FALSE))
model_nssats_tstat

# Get R^2
model_nssats_r2 <- boot(data = cross_section_nssats, statistic = rsq_function, R = 5000, 
                   formula = lgbtq_actual ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund +
                     partisan_state + cong_116_score)
model_nssats_r2
boot.ci(model_nssats_r2, type = "bca")

# Get AIC
model_nssats_aic <- boot(data = cross_section_nssats, statistic = get_aic, R = 5000, 
                    formula = lgbtq_actual ~ state_policy + lgbtq_hate + lgbtq_pop_total + govt_fund +
                      partisan_state + cong_116_score)
model_nssats_aic
