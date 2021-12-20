# Load dependencies
library(Hmisc)
library(tidyverse)
library(plm)
library(OrthoPanels)
library(Amelia)
library(psych)
library(viridis)

# IMPORT DATA -------------------------------------------------------------

# Import data - N-SSATS
my_csvs <- list.files("data/cleaned/nssats/")
my_csvs <- paste0("data/cleaned/nssats/", my_csvs)
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
nssats <- bind_rows(csv_list, .id = "column_label") %>%
  select(-column_label)

# Import the CSV files - Equality Tables
my_csvs1 <- list.files("data/equality_tables/transformed/")
my_csvs1 <- paste0("data/equality_tables/transformed/", my_csvs1)
csv_list1 <- lapply(my_csvs1, read_csv)

# Combine data frames
eq_tables <- bind_rows(csv_list1, .id = "column_label") %>%
  select(-column_label)

# Merge the two imported data frames
nssats_eq_a <- left_join(nssats, eq_tables) %>%
  rename(state_policy = overall_policy_total) %>%
  select(-c(state)) %>%
  group_by(year) %>%
  mutate(case_state = 1:50) %>%
  ungroup() %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(region, case_state, year, govt_fund, lgbtq_perc, lgbtq_perc_actual, 
         lgbtq_total, n, everything())
nssats_eq_a

# Check logical bounds
nssats_eq_a %>%
  filter(region == "new hampshire") %>%
  select(region, case_state, year, govt_fund, lgbtq_perc, lgbtq_perc_actual, lgbtq_total, n)

# MISSING DATA ------------------------------------------------------------

# For Amelia, see: https://www.opr.princeton.edu/workshops/Downloads/2018Jan_AmeliaPratt.pdf
# Only conducted once, so do not execute code again

# Need to replace with expectation maximization
# a_out <- amelia(as.data.frame(nssats_eq_a), m = 5, idvars = c("temp_id", "region"), 
#             ts = "year", cs = "case_state",
#             emburn = c(50, 100),
#             bounds = rbind(c(4, 0, 100), c(5, 0, 100), c(6, 0, 100), c(7, 0, Inf), c(8, 0, Inf))
# )

# Write to file
# write.amelia(obj = a_out, file.stem = "data/amelia_nssats/amelia_nssats_", format = "csv")

# Import imputed data set
nssats_eq <- read_csv("data/amelia_nssats/amelia_nssats_5.csv") %>%
  select(-X1)
nssats_eq

# Any missing data generally?
gather(nssats_eq, key = "variables", value = "values", -temp_id) %>%
  filter(is.na(values))

# Check logical bounds
nssats_eq %>%
  filter(region == "new hampshire") %>%
  select(region, case_state, year, govt_fund, lgbtq_perc, lgbtq_perc_actual, n)

nssats_eq %>%
  select(region, year, govt_total, govt_fund) %>%
  filter(year == 2014)

# ASSUMPTION CHECKING -----------------------------------------------------

# Export to try in StataBE 17
nssats_eq %>%
  write_csv("data/cleaned/nssats_eq_panel_data.csv")

# Assumptions tested in StataBE 17:
# - Stationarity (unit root tests)
# - Serial autocorrelation

# Prepare data set
nssats_eq_lags <- nssats_eq %>%
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
    # Lags for the reported percentage SGM-tailored programming
    l_lgbtq_perc = Lag(lgbtq_perc, shift = 1),
    l2_lgbtq_perc = Lag(lgbtq_perc, shift = 2),
    l3_lgbtq_perc = Lag(lgbtq_perc, shift = 3),
    l4_lgbtq_perc = Lag(lgbtq_perc, shift = 4),
    l5_lgbtq_perc = Lag(lgbtq_perc, shift = 5),
    l6_lgbtq_perc = Lag(lgbtq_perc, shift = 6),
    # Lags for the actual SGM-tailored programming
    l_lgbtq_actual = Lag(lgbtq_actual, shift = 1),
    l2_lgbtq_actual = Lag(lgbtq_actual, shift = 2),
    l3_lgbtq_actual = Lag(lgbtq_actual, shift = 3),
    l4_lgbtq_actual = Lag(lgbtq_actual, shift = 4),
    l5_lgbtq_actual = Lag(lgbtq_actual, shift = 5),
    l6_lgbtq_actual = Lag(lgbtq_actual, shift = 6),
    # Lags for the actual percentage of SGM-tailored programming
    l_lgbtq_perc_actual = Lag(lgbtq_perc_actual, shift = 1),
    l2_lgbtq_perc_actual = Lag(lgbtq_perc_actual, shift = 2),
    l3_lgbtq_perc_actual = Lag(lgbtq_perc_actual, shift = 3),
    l4_lgbtq_perc_actual = Lag(lgbtq_perc_actual, shift = 4),
    l5_lgbtq_perc_actual = Lag(lgbtq_perc_actual, shift = 5),
    l6_lgbtq_perc_actual = Lag(lgbtq_perc_actual, shift = 6)
  ) 
nssats_eq_lags

# Export to try in StataBE 17
nssats_eq_lags %>%
  write_csv("data/cleaned/nssats_eq_lags.csv")

# Remove all missing values and export to State BE 17
missing_filter <- nssats_eq_lags %>%
  select(lgbtq_actual, state_policy, l_state_policy, l2_state_policy, govt_fund,
         l_govt_fund, l2_govt_fund, year) %>%
  complete.cases()

nssats_eq_lags[missing_filter, ] %>%
  write_csv("data/cleaned/nssats_eq_lags_complete.csv")

# Evidence for autoregressive dependent variable and need for lagged 
# dependent variable in the model

# Prepare data sets for correlation
corr_ds_1 <- nssats_eq %>%
  filter(year != "2010") %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(temp_id, year, lgbtq_total)

corr_ds_2 <- nssats_eq %>%
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
nssats_eq_lags %>%
  select(year, state_policy, govt_fund) %>%
  complete.cases() %>%
  sum()

# DESCRIPTIVE STATISTICS BY YEAR ------------------------------------------

# For Table 1
nssats_eq_lags %>%
  group_by(year) %>%
  summarize(
    lgbtq_perc_m = round(mean(lgbtq_perc), 2),
    lgbtq_perc_sd = round(sd(lgbtq_perc), 2),
    lgbtq_perc_actual_m = round(mean(lgbtq_perc_actual), 2),
    lgbtq_perc_actual_sd = round(sd(lgbtq_perc_actual), 2),
    state_policy_m = round(mean(state_policy), 2),
    state_policy_sd = round(sd(state_policy), 2),
    govt_fund_m = round(mean(govt_fund), 2),
    govt_fund_sd = round(sd(govt_fund), 2)
  ) %>%
  write_csv("data/results/tables/table_1_nssats_descriptives.csv")

# SGM facilities
nssats_eq_lags %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(lgbtq_perc), 2),
    sd = round(sd(lgbtq_perc), 2),
    med = round(median(lgbtq_perc), 2),
    min = round(min(lgbtq_perc), 2),
    max = round(max(lgbtq_perc), 2),
    skew = round(skew(lgbtq_perc), 2),
    kurtosis = round(kurtosi(lgbtq_perc), 2)
  ) %>%
  write_csv("data/results/tables/lgbtq_perc_descriptives.csv")

# SGM facilities - corrected
nssats_eq_lags %>%
  group_by(year) %>%
  summarize(
    mean = round(mean(lgbtq_perc_actual), 2),
    sd = round(sd(lgbtq_perc_actual), 2),
    med = round(median(lgbtq_perc_actual), 2),
    min = round(min(lgbtq_perc_actual), 2),
    max = round(max(lgbtq_perc_actual), 2),
    skew = round(skew(lgbtq_perc_actual), 2),
    kurtosis = round(kurtosi(lgbtq_perc_actual), 2)
  ) %>%
  write_csv("data/results/tables/lgbtq_perc_actual_descriptives.csv")

# State policy
nssats_eq_lags %>%
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
nssats_eq_lags %>%
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
stigma_opm <- opm(lgbtq_perc ~  state_policy + l_state_policy + govt_fund + l_govt_fund + t,
                  # Call the data set
                  data = nssats_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nssats_eq_lags$"lgbtq_perc"-nssats_eq_lags$"l_lgbtq_perc") - median(stigma_opm$samples$rho)*(nssats_eq_lags$"l_lgbtq_perc"-nssats_eq_lags$"l2_lgbtq_perc") - median(stigma_opm$samples$beta[,1])*(nssats_eq_lags$"state_policy"-nssats_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nssats_eq_lags$"l_state_policy"-nssats_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nssats_eq_lags$"govt_fund"-nssats_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nssats_eq_lags$"l_govt_fund"-nssats_eq_lags$"l2_govt_fund")

opms_e_2 <- (nssats_eq_lags$"l2_lgbtq_perc"-nssats_eq_lags$"l3_lgbtq_perc") - median(stigma_opm$samples$rho)*(nssats_eq_lags$"l3_lgbtq_perc"-nssats_eq_lags$"l4_lgbtq_perc") - median(stigma_opm$samples$beta[,1])*(nssats_eq_lags$"l2_state_policy"-nssats_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nssats_eq_lags$"l3_state_policy"-nssats_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nssats_eq_lags$"l2_govt_fund"-nssats_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nssats_eq_lags$"l3_govt_fund"-nssats_eq_lags$"l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Since the Arellano and Bond (1991) m2 statistic is NOT significant, there is 
# NO evidence of serial correlation. 

# ORTHOGONAL REPARAMETERIZATION: SGM CORRECTED ----------------------------

# Year variable controls for trending
stigma_opm <- opm(lgbtq_perc_actual ~  state_policy + l_state_policy + govt_fund + l_govt_fund + t,
                  # Call the data set
                  data = nssats_eq_lags,
                  # Specify panel data
                  index = c("region", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nssats_eq_lags$"lgbtq_perc_actual"-nssats_eq_lags$"l_lgbtq_perc_actual") - median(stigma_opm$samples$rho)*(nssats_eq_lags$"l_lgbtq_perc_actual"-nssats_eq_lags$"l2_lgbtq_perc_actual") - median(stigma_opm$samples$beta[,1])*(nssats_eq_lags$"state_policy"-nssats_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nssats_eq_lags$"l_state_policy"-nssats_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nssats_eq_lags$"govt_fund"-nssats_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nssats_eq_lags$"l_govt_fund"-nssats_eq_lags$"l2_govt_fund")

opms_e_2 <- (nssats_eq_lags$"l2_lgbtq_perc_actual"-nssats_eq_lags$"l3_lgbtq_perc_actual") - median(stigma_opm$samples$rho)*(nssats_eq_lags$"l3_lgbtq_perc_actual"-nssats_eq_lags$"l4_lgbtq_perc_actual") - median(stigma_opm$samples$beta[,1])*(nssats_eq_lags$"l2_state_policy"-nssats_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nssats_eq_lags$"l3_state_policy"-nssats_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nssats_eq_lags$"l2_govt_fund"-nssats_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nssats_eq_lags$"l3_govt_fund"-nssats_eq_lags$"l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Since the m2 statistic is NOT significant, there is NO evidence of serial correlation.

# FINAL MODEL - ORTHOGONAL REPARAMETERIZATION -----------------------------

# Year variable controls for trending
stigma_opm <- opm(lgbtq_perc_actual ~  state_policy + l_state_policy + govt_fund + l_govt_fund + t,
                  # Call the data set
                  data = nssats_eq_lags,
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

# Year variable controls for trending
stigma_opm_1 <- opm(lgbtq_perc ~  state_policy + l_state_policy + govt_fund + l_govt_fund + t,
                  # Call the data set
                  data = nssats_eq_lags,
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
quantile(stigma_opm_1$samples$beta[, 2]/(1 - stigma_opm_1$samples$rho), probs = c(0.025, 0.5, 0.975))
quantile(stigma_opm_1$samples$beta[, 3]/(1 - stigma_opm_1$samples$rho), probs = c(0.025, 0.5, 0.975))

# OLS fixed-effects model

# Set the data into panel data
nssats_plm <- pdata.frame(nssats_eq_lags, index = c("region", "year"))
head(nssats_plm)

# Select the variables of interest
nssats_plm <- nssats_plm %>%
  select(lgbtq_perc_actual, l_lgbtq_perc_actual, state_policy, l_state_policy, 
         govt_fund, l_govt_fund, year, region) %>%
  as_tibble()
nssats_plm

# Scale all variables to yield standardized coefficients
nssats_plm_1 <- lapply(nssats_plm[, -c(7, 8)], base::scale) %>%
  as_tibble() %>%
  # Add year and region columns back
  mutate(year = nssats_plm$year, region = nssats_plm$region)
nssats_plm_1

# Change names
names(nssats_plm_1) <- names(nssats_plm)
nssats_plm_1

# Set the data into panel data
nssats_plm_2 <- pdata.frame(nssats_plm_1, index = c("region", "year"))

# Estimate fixed-effects model
reg_fe <- plm(lgbtq_perc_actual ~ l_lgbtq_perc_actual + state_policy + l_state_policy + 
                govt_fund + l_govt_fund + year,
                  data = nssats_plm_2, 
                  model = "within")

summary(reg_fe)

# Long-run effects
reg_fe$coefficients[2] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[3] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[4] / (1 - reg_fe$coefficients[1])
reg_fe$coefficients[5] / (1 - reg_fe$coefficients[1])

# VISUALIZATION -----------------------------------------------------------

# Get geospatial data - https://rpubs.com/lokigao/maps
us_map <- map_data("state")

# Show fonts available
windowsFonts()

# SGM-tailored programming by state
nssats_map <- nssats_eq_lags %>% 
  filter(year %in% c(2010, 2020)) %>%
  select(region, year, lgbtq_perc_actual, lgbtq_perc) %>% 
  right_join(us_map, by = "region") %>%
  filter(!is.na(year))
nssats_map

# Map of LGBT programming at substance use facilities 
nssats_choropleth <- ggplot(data = nssats_map, aes(x = long, y = lat, group = group, 
                                                   fill = lgbtq_perc_actual)) +
  geom_polygon(color = "white") +
  theme_void() +
  # Set the coordinate orientation
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis(
    name = "% Facilities with SGM-Tailored Programming",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 17,
      title.hjust = 0.5
    ),
    option = "D"
  ) +
  theme(
    legend.position = "bottom",
    # Add Times New Roman font
    text = element_text(family = "serif"),
    strip.text.x = element_text(size = 16)
  ) +
  facet_wrap(~ year)
nssats_choropleth

# Map of LGBT programming at substance use facilities 
nssats_choropleth_reported <- ggplot(data = nssats_map, aes(x = long, y = lat, group = group, 
                                                   fill = lgbtq_perc)) +
  geom_polygon(color = "white") +
  theme_void() +
  # Set the coordinate orientation
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis(
    name = "% Facilities with SGM-Tailored Programming",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 17,
      title.hjust = 0.5
    ),
    option = "D"
  ) +
  theme(
    legend.position = "bottom",
    # Add Times New Roman font
    text = element_text(family = "serif"),
    strip.text.x = element_text(size = 16)
  ) +
  facet_wrap(~ year)
nssats_choropleth_reported

# Save the plots
# ggsave(filename = "data/results/nssats_choropleth.png", plot = nssats_choropleth,
#        width = 7.22, height = 3.5)

# ggsave(filename = "data/results/nssats_choropleth_reported.png", plot = nssats_choropleth_reported,
#        width = 7.22, height = 3.5)
