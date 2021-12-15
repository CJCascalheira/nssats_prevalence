# Load dependencies
library(Hmisc)
library(tidyverse)
library(plm)
library(OrthoPanels)
library(Amelia)
library(psych)

# IMPORT DATA -------------------------------------------------------------

# Import data - N-MHSS
my_csvs <- list.files("data/cleaned/nmhss/")
my_csvs <- paste0("data/cleaned/nmhss/", my_csvs)
csv_list <- lapply(my_csvs, read_csv)

# Vector of years and time
year_vector <- c(2010:2020)
time_vector <- c(1:10)

# Add year and create long data
for (i in 1:length(year_vector)) {
  # For each csv file
  csv_list[[i]] <- csv_list[[i]] %>%
    # Create a year column
    mutate(year = rep(year_vector[i], nrow(.))) %>%
    # Create a time column
    mutate(t = rep(time_vector[i], nrow(.)))
}

# Combine data frames
nmhss <- bind_rows(csv_list, .id = "column_label") %>%
  select(-column_label)

# Import the CSV files - Equality Tables
my_csvs1 <- list.files("data/equality_tables/transformed/")
my_csvs1 <- paste0("data/equality_tables/transformed/", my_csvs1)
csv_list1 <- lapply(my_csvs1, read_csv)

# Combine data frames
eq_tables <- bind_rows(csv_list1, .id = "column_label") %>%
  select(-column_label)

# Import correction to SGM-tailored programming
confirmed_sgm <- read_csv("data/cleaned/confirmed_sgm_tailored_programming.csv") %>%
  select(region, perc_confirmed)

# Merge the two imported data frames
nmhss_eq_a <- left_join(nmhss, eq_tables) %>%
  rename(state_policy = overall_policy_total) %>%
  select(-c(state)) %>%
  group_by(year) %>%
  mutate(case_state = 1:50) %>%
  ungroup() %>%
  mutate(temp_id = 1:nrow(.)) %>%
  # Add the correction to SGM-tailored programming
  left_join(confirmed_sgm) %>%
  # Calculate the estimate value of actual SGM-tailored programming
  mutate(
    lgbtq_actual = lgbtq_total * perc_confirmed
  ) %>%
  select(region, case_state, year, everything())
nmhss_eq_a

# MISSING DATA ------------------------------------------------------------

# For Amelia, see: https://www.opr.princeton.edu/workshops/Downloads/2018Jan_AmeliaPratt.pdf
# Only conducted once, so do not execute code again

# Need to replace with expectation maximization
# a_out <- amelia(as.data.frame(nmhss_eq_a), m = 5, idvars = c("temp_id", "region"), 
#                ts = "year", cs = "case_state",
#                emburn = c(50, 100)
# )

# Write to file
# write.amelia(obj = a_out, file.stem = "data/amelia_nmhss/amelia_nmhss_", format = "csv")

# Import imputed data set
nmhss_eq <- read_csv("data/amelia_nmhss/amelia_nmhss_5.csv") %>%
  select(-X1)
nmhss_eq

# Any missing data generally?
gather(nmhss_eq, key = "variables", value = "values", -temp_id) %>%
  filter(is.na(values))

# PROOF OF CONCEPT TEST ---------------------------------------------------

# Get data for specific year
nmhss_eq_example <- nmhss_eq %>%
  filter(year == 2020)

# Simple cross-sectional model
summary(lm(lgbtq_total ~ state_policy, data = nmhss_eq_example))

# From a cross-sectional view, some years are associated with LGBTQ+ programming,
# but some are not. Some years, the prediction is significant.

# ASSUMPTION CHECKING -----------------------------------------------------

# Export to try in StataBE 17
nmhss_eq %>%
  write_csv("data/cleaned/nmhss_eq_panel_data.csv")

# Assumptions tested in StataBE 17:
# - Stationarity (unit root tests)
# - Serial autocorrelation

# Prepare data set
nmhss_eq_lags <- nmhss_eq %>%
  # Quadratic trend
  mutate(
    t2 = t^2
  ) %>%
  # Create lagged variables for two predictors
  mutate(
    l_lgbtq_total = Lag(lgbtq_total, shift = 1),
    l_state_policy = Lag(state_policy, shift = 1),
    l_govt_fund = Lag(govt_fund, shift = 1),
    l2_lgbtq_total = Lag(lgbtq_total, shift = 2),
    l2_state_policy = Lag(state_policy, shift = 2),
    l2_govt_fund = Lag(govt_fund, shift = 2),
    l3_lgbtq_total = Lag(lgbtq_total, shift = 3),
    l3_state_policy = Lag(state_policy, shift = 3),
    l3_govt_fund = Lag(govt_fund, shift = 3),
    l4_lgbtq_total = Lag(lgbtq_total, shift = 4),
    l4_state_policy = Lag(state_policy, shift = 4),
    l4_govt_fund = Lag(govt_fund, shift = 4),
    l5_lgbtq_total = Lag(lgbtq_total, shift = 5),
    l5_state_policy = Lag(state_policy, shift = 5),
    l5_govt_fund = Lag(govt_fund, shift = 5),
    l6_lgbtq_total = Lag(lgbtq_total, shift = 6),
    l6_state_policy = Lag(state_policy, shift = 6),
    l6_govt_fund = Lag(govt_fund, shift = 6),
    # Lags for the actual SGM-tailored programming
    l_lgbtq_actual = Lag(lgbtq_actual, shift = 1),
    l2_lgbtq_actual = Lag(lgbtq_actual, shift = 2),
    l3_lgbtq_actual = Lag(lgbtq_actual, shift = 3),
    l4_lgbtq_actual = Lag(lgbtq_actual, shift = 4),
    l5_lgbtq_actual = Lag(lgbtq_actual, shift = 5),
    l6_lgbtq_actual = Lag(lgbtq_actual, shift = 6)
  ) %>%
  # Get first differences
  mutate(
    fd_lgbtq_total = lgbtq_total - l_lgbtq_total,
    fd_state_policy = state_policy - l_state_policy,
    fd_govt_fund = govt_fund - l_govt_fund,
    # Lag = 1
    fd_l_lgbtq_total = l_lgbtq_total - l2_lgbtq_total,
    fd_l_state_policy = l_state_policy - l2_state_policy,
    fd_l_govt_fund = l_govt_fund - l2_govt_fund,
    # Lag = 2
    fd_l2_lgbtq_total = l2_lgbtq_total - l3_lgbtq_total,
    fd_l2_state_policy = l2_state_policy - l3_state_policy,
    fd_l2_govt_fund = l2_govt_fund - l3_govt_fund,
    # Lag = 3
    fd_l3_lgbtq_total = l3_lgbtq_total - l4_lgbtq_total,
    fd_l3_state_policy = l3_state_policy - l4_state_policy,
    fd_l3_govt_fund = l3_govt_fund - l4_govt_fund,
    # Lag = 4
    fd_l4_lgbtq_total = l4_lgbtq_total - l5_lgbtq_total,
    fd_l4_state_policy = l4_state_policy - l5_state_policy,
    fd_l4_govt_fund = l4_govt_fund - l5_govt_fund,
  )
nmhss_eq_lags

# Export to try in StataBE 17
nmhss_eq_lags %>%
  write_csv("data/cleaned/nmhss_eq_lags.csv")

# Remove all missing values and export to State BE 17
missing_filter <- nmhss_eq_lags %>%
  select(lgbtq_actual, state_policy, l_state_policy, l2_state_policy, govt_fund,
         l_govt_fund, l2_govt_fund, year) %>%
  complete.cases()

nmhss_eq_lags[missing_filter, ] %>%
  write_csv("data/cleaned/nmhss_eq_lags_complete.csv")

# Evidence for autoregressive dependent variable and need for lagged 
# dependent variable in the model

# Prepare data sets for correlation
corr_ds_1 <- nmhss_eq %>%
  filter(year != "2010") %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(temp_id, year, lgbtq_total)

corr_ds_2 <- nmhss_eq %>%
  filter(year != "2020") %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(temp_id, year, lgbtq_total)

# Calculate correlation of dependent variable
left_join(corr_ds_2, corr_ds_1, by = "temp_id") %>%
  group_by(year.x) %>%
  summarize(
    corr = cor(lgbtq_total.x, lgbtq_total.y, use = "pairwise.complete.obs")
  ) %>%
  ungroup() %>%
  summarize(
    avg = mean(corr)
  )

# Balanced panels - check missingness
nmhss_eq_lags %>%
  select(year, state_policy, govt_fund) %>%
  complete.cases() %>%
  sum()

# Balanced panels - visualize
balanced_df <- nmhss_eq_lags %>%
  select(year, state_policy, govt_fund) %>%
  mutate(
    govt_fund = if_else(!is.na(govt_fund), 1, govt_fund),
    state_policy = if_else(!is.na(state_policy), 1, state_policy)
  ) 

ggplot(balanced_df, aes(x = govt_fund)) +
  geom_bar() +
  facet_wrap(~ year) +
  scale_x_continuous() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

# DESCRIPTIVE STATISTICS BY YEAR ------------------------------------------

# SGM facilities
nmhss_eq_lags %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(lgbtq_total), 2),
    sd = round(sd(lgbtq_total), 2),
    med = round(median(lgbtq_total), 2),
    min = round(min(lgbtq_total), 2),
    max = round(max(lgbtq_total), 2),
    skew = round(skew(lgbtq_total), 2),
    kurtosis = round(kurtosi(lgbtq_total), 2)
  ) %>%
  write_csv("data/results/tables/lgbtq_total_descriptives.csv")

# State policy
nmhss_eq_lags %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(state_policy), 2),
    sd = round(sd(state_policy), 2),
    med = round(median(state_policy), 2),
    min = round(min(state_policy), 2),
    max = round(max(state_policy), 2),
    skew = round(skew(state_policy), 2),
    kurtosis = round(kurtosi(state_policy), 2)
  ) %>%
  write_csv("data/results/tables/state_policy_descriptives.csv")

# Government funding
nmhss_eq_lags %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(govt_fund), 2),
    sd = round(sd(govt_fund), 2),
    med = round(median(govt_fund), 2),
    min = round(min(govt_fund), 2),
    max = round(max(govt_fund), 2),
    skew = round(skew(govt_fund), 2),
    kurtosis = round(kurtosi(govt_fund), 2)
  ) %>%
  write_csv("data/results/tables/govt_fund_descriptives.csv")

# ORTHOGONAL REPARAMETERIZATION -------------------------------------------

# Year variable controls for trending
stigma_opm <- opm(lgbtq_total ~  state_policy + l_state_policy + govt_fund + l_govt_fund + t,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_total"-nmhss_eq_lags$"l_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_total"-nmhss_eq_lags$"l2_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_total"-nmhss_eq_lags$"l3_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_total"-nmhss_eq_lags$"l4_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Arellano and Bond (1991) m2 statistic is significant, therefore we reject the 
# null hypothesis of white noise; there is serial correlation in the errors

# Year variable controls for trending
stigma_opm <- opm(lgbtq_total ~ state_policy + l_state_policy + l2_state_policy + 
                    govt_fund + l_govt_fund + l2_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_total"-nmhss_eq_lags$"l_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_total"-nmhss_eq_lags$"l2_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_total"-nmhss_eq_lags$"l3_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_total"-nmhss_eq_lags$"l4_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l4_state_policy"-nmhss_eq_lags$"l5_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l4_govt_fund"-nmhss_eq_lags$"l5_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Arellano and Bond (1991) m2 statistic is significant, therefore we reject the 
# null hypothesis of white noise; there is serial correlation in the errors

# Year variable controls for trending
stigma_opm <- opm(lgbtq_total ~ state_policy + l_state_policy + l2_state_policy + l3_state_policy + 
                    govt_fund + l_govt_fund + l2_govt_fund + l3_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_total"-nmhss_eq_lags$"l_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_total"-nmhss_eq_lags$"l2_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund") - median(stigma_opm$samples$beta[,7])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,8])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_total"-nmhss_eq_lags$"l3_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_total"-nmhss_eq_lags$"l4_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l4_state_policy"-nmhss_eq_lags$"l5_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l5_state_policy"-nmhss_eq_lags$"l6_state_policy") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund") - median(stigma_opm$samples$beta[,7])*(nmhss_eq_lags$"l4_govt_fund"-nmhss_eq_lags$"l5_govt_fund") - median(stigma_opm$samples$beta[,8])*(nmhss_eq_lags$"l5_govt_fund"-nmhss_eq_lags$"l6_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# At this point, it is clear that adding more lags of the independent variables is not
# improving the serial correlation; the errors remain serially correlated. Next,
# I try to take the first difference of the original ALD (1, 1) model.

# Year variable controls for trending
stigma_opm <- opm(fd_lgbtq_total ~  fd_state_policy + fd_l_state_policy + 
                    fd_govt_fund + fd_l_govt_fund + t,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"fd_lgbtq_total"-nmhss_eq_lags$"fd_l_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"fd_l_lgbtq_total"-nmhss_eq_lags$"fd_l2_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"fd_state_policy"-nmhss_eq_lags$"fd_l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"fd_l_state_policy"-nmhss_eq_lags$"fd_l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"fd_govt_fund"-nmhss_eq_lags$"fd_l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"fd_l_govt_fund"-nmhss_eq_lags$"fd_l2_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"fd_l2_lgbtq_total"-nmhss_eq_lags$"fd_l3_lgbtq_total") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"fd_l3_lgbtq_total"-nmhss_eq_lags$"fd_l4_lgbtq_total") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"fd_l2_state_policy"-nmhss_eq_lags$"fd_l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"fd_l3_state_policy"-nmhss_eq_lags$"fd_l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"fd_l2_govt_fund"-nmhss_eq_lags$"fd_l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"fd_l3_govt_fund"-nmhss_eq_lags$"fd_l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Taking the first difference does not reduce serial autocorrelation---instead, it
# makes the issue worse.

# Try the omitted variable approach next, but first, see whether inclusion
# of the corrected SGM-tailored programming counts affect the results

# ORTHOGONAL REPARAMETERIZATION: SGM CORRECTED ----------------------------

# Year variable controls for trending
stigma_opm <- opm(lgbtq_actual ~  state_policy + l_state_policy + govt_fund + l_govt_fund + t,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_actual"-nmhss_eq_lags$"l_lgbtq_actual") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_actual"-nmhss_eq_lags$"l2_lgbtq_actual") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_actual"-nmhss_eq_lags$"l3_lgbtq_actual") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_actual"-nmhss_eq_lags$"l4_lgbtq_actual") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Since the m2 statistic is significant, there is evidence of serial correlation.
# Therefore, attempt to apply lags.

# Year variable controls for trending
stigma_opm <- opm(lgbtq_actual ~ state_policy + l_state_policy + l2_state_policy + 
                    govt_fund + l_govt_fund + l2_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_actual"-nmhss_eq_lags$"l_lgbtq_actual") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_actual"-nmhss_eq_lags$"l2_lgbtq_actual") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_actual"-nmhss_eq_lags$"l3_lgbtq_actual") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_actual"-nmhss_eq_lags$"l4_lgbtq_actual") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l4_state_policy"-nmhss_eq_lags$"l5_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l4_govt_fund"-nmhss_eq_lags$"l5_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# M2 statistic finally hit the threshold. Check 3 lags to see if improvement.

# Year variable controls for trending
stigma_opm <- opm(lgbtq_actual ~ state_policy + l_state_policy + l2_state_policy + l3_state_policy + 
                    govt_fund + l_govt_fund + l2_govt_fund + l3_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_actual"-nmhss_eq_lags$"l_lgbtq_actual") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_actual"-nmhss_eq_lags$"l2_lgbtq_actual") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund") - median(stigma_opm$samples$beta[,7])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,8])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_actual"-nmhss_eq_lags$"l3_lgbtq_actual") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_actual"-nmhss_eq_lags$"l4_lgbtq_actual") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l4_state_policy"-nmhss_eq_lags$"l5_state_policy") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l5_state_policy"-nmhss_eq_lags$"l6_state_policy") - median(stigma_opm$samples$beta[,5])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,6])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund") - median(stigma_opm$samples$beta[,7])*(nmhss_eq_lags$"l4_govt_fund"-nmhss_eq_lags$"l5_govt_fund") - median(stigma_opm$samples$beta[,8])*(nmhss_eq_lags$"l5_govt_fund"-nmhss_eq_lags$"l6_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# No significant improvement, so retain the simpler model (i.e., two lags)

# FINAL MODEL - ORTHOGONAL REPARAMETERIZATION -----------------------------

# Year variable controls for trending
stigma_opm <- opm(lgbtq_actual ~ state_policy + l_state_policy + l2_state_policy + 
                    govt_fund + l_govt_fund + l2_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Calculate long-run effects
quantile(stigma_opm$samples$beta[, 1]/(1 - stigma_opm$samples$rho), probs = c(0.025, 0.5, 0.975))
quantile(stigma_opm$samples$beta[, 2]/(1 - stigma_opm$samples$rho), probs = c(0.025, 0.5, 0.975))
quantile(stigma_opm$samples$beta[, 3]/(1 - stigma_opm$samples$rho), probs = c(0.025, 0.5, 0.975))
quantile(stigma_opm$samples$beta[, 4]/(1 - stigma_opm$samples$rho), probs = c(0.025, 0.5, 0.975))
quantile(stigma_opm$samples$beta[, 5]/(1 - stigma_opm$samples$rho), probs = c(0.025, 0.5, 0.975))
quantile(stigma_opm$samples$beta[, 6]/(1 - stigma_opm$samples$rho), probs = c(0.025, 0.5, 0.975))

# Model with report SGM totals
stigma_opm_1 <- opm(lgbtq_total ~ state_policy + l_state_policy + l2_state_policy + 
                    govt_fund + l_govt_fund + l2_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm_1)

# Calculate long-run effects
quantile(stigma_opm_1$samples$beta[, 1]/(1 - stigma_opm_1$samples$rho), probs = c(0.025, 0.5, 0.975))

# OLS fixed-effects model

# Set the data into panel data
nmhss_plm <- pdata.frame(nmhss_eq_lags, index = c("region", "year"))
head(nmhss_plm)

# Select the variables of interest
nmhss_plm <- nmhss_plm %>%
  select(lgbtq_actual, l_lgbtq_actual, state_policy, l_state_policy, l2_state_policy,
         govt_fund, l_govt_fund, l2_govt_fund, year) %>%
  as_tibble()
head(nmhss_plm)

# Scale all variables to yield standardized coefficients
nmhss_plm_1 <- lapply(nmhss_plm[, -9], scale) %>%
  as_tibble() %>%
  # Add year column back
  mutate(year = nmhss_plm$year)

# Change names
names(nmhss_plm_1) <- names(nmhss_plm)
head(nmhss_plm_1)

# Set the data into panel data
nmhss_plm_1 <- pdata.frame(nmhss_eq_lags, index = c("region", "year"))

# Estimate fixed-effects model
reg_fe <- plm(lgbtq_actual ~ l_lgbtq_actual + state_policy + l_state_policy + l2_state_policy + 
                govt_fund + l_govt_fund + l2_govt_fund + year,
                  data = nmhss_plm_1, 
                  model = "within")

summary(reg_fe)

# Long-run effects
reg_fe$coefficients[2] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[3] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[4] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[5] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[6] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[7] / (1 - reg_fe$coefficients[1])

# VISUALIZATION -----------------------------------------------------------

# Correlation visualization
nmhss_eq %>%
  group_by(year) %>%
  summarize(
    cor_lgbtq_state = cor(lgbtq_total, state_policy, use = "pairwise.complete.obs")
  ) %>%
  ggplot(aes(x = year, y = cor_lgbtq_state)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  scale_x_continuous(limits = c(2010, 2020), breaks = seq(2010, 2020, by = 1))

