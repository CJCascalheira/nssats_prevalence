# Load dependencies
library(Hmisc)
library(tidyverse)
library(plm)
library(OrthoPanels)
library(psych)
library(viridis)

# IMPORT DATA -------------------------------------------------------------

# Import data - N-MHSS
my_csvs <- list.files("data/cleaned/nmhss/")
my_csvs <- paste0("data/cleaned/nmhss/", my_csvs)
csv_list <- lapply(my_csvs, read_csv)

# Vector of years and time
year_vector <- c(2014:2019)
time_vector <- c(1:6)

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
  select(-column_label) %>%
  filter(year %in% c(2014, 2015, 2016, 2017, 2018, 2019)) %>%
  select(-state) %>%
  rename(state = region)

# Merge the two imported data frames
nmhss_eq <- left_join(nmhss, eq_tables) %>%
  rename(state_policy = overall_policy_total) %>%
  group_by(year) %>%
  mutate(case_state = 1:50) %>%
  ungroup() %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(state, case_state, year, everything())
nmhss_eq

# Any missing data?
sum(complete.cases(nmhss_eq)) - nrow(nmhss_eq)

# GET AVERAGE -------------------------------------------------------------

# Average number of programs per year
nmhss %>%
  group_by(year) %>%
  summarize(
    sum = sum(n, na.rm = TRUE)) %>%
  ungroup %>%
  summarize(avg = mean(sum),
            sd = sd(sum))

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
  # Create lagged variables
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
    # LGBTQ percentage
    l_lgbtq_perc = Lag(lgbtq_perc, shift = 1),
    l2_lgbtq_perc = Lag(lgbtq_perc, shift = 2),
    l3_lgbtq_perc = Lag(lgbtq_perc, shift = 3),
    l4_lgbtq_perc = Lag(lgbtq_perc, shift = 4),
    l5_lgbtq_perc = Lag(lgbtq_perc, shift = 5),
    l6_lgbtq_perc = Lag(lgbtq_perc, shift = 6),
  ) %>%
  # Get first differences
  mutate(
    fd_govt_fund = govt_fund - l_govt_fund,
    # Lag = 1
    fd_l_govt_fund = l_govt_fund - l2_govt_fund,
    # Lag = 2
    fd_l2_govt_fund = l2_govt_fund - l3_govt_fund,
    # Lag = 3
    fd_l3_govt_fund = l3_govt_fund - l4_govt_fund,
    # Lag = 4
    fd_l4_govt_fund = l4_govt_fund - l5_govt_fund
  )
nmhss_eq_lags

# Export to try in StataBE 17
nmhss_eq_lags %>%
  write_csv("data/cleaned/nmhss_eq_lags.csv")

# Remove all missing values and export to State BE 17
missing_filter <- nmhss_eq_lags %>%
  select(lgbtq_perc, state_policy, l_state_policy, l2_state_policy, govt_fund,
         l_govt_fund, l2_govt_fund, year) %>%
  complete.cases()

nmhss_eq_lags[missing_filter, ] %>%
  write_csv("data/cleaned/nmhss_eq_lags_complete.csv")

# Evidence for autoregressive dependent variable and need for lagged 
# dependent variable in the model

# Prepare data sets for correlation
corr_ds_1 <- nmhss_eq %>%
  filter(year != "2014") %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(temp_id, year, lgbtq_perc)

corr_ds_2 <- nmhss_eq %>%
  filter(year != "2019") %>%
  mutate(temp_id = 1:nrow(.)) %>%
  select(temp_id, year, lgbtq_perc)

# Calculate correlation of dependent variable
left_join(corr_ds_2, corr_ds_1, by = "temp_id") %>%
  group_by(year.x) %>%
  summarize(
    corr = cor(lgbtq_perc.x, lgbtq_perc.y, use = "pairwise.complete.obs")
  ) %>%
  ungroup() %>%
  summarize(
    avg = mean(corr)
  )

# Because no data are missing, the data set is balanced.

# DESCRIPTIVE STATISTICS BY YEAR ------------------------------------------

# For Table 1
nmhss_eq_lags %>%
  group_by(year) %>%
  summarize(
    lgbtq_perc_m = round(mean(lgbtq_perc), 2),
    lgbtq_perc_sd = round(sd(lgbtq_perc), 2),
    state_policy_m = round(mean(state_policy), 2),
    state_policy_sd = round(sd(state_policy), 2),
    govt_fund_m = round(mean(govt_fund), 2),
    govt_fund_sd = round(sd(govt_fund), 2),
    fd_govt_fund_m = round(mean(fd_govt_fund), 2),
    fd_govt_fund_sd = round(sd(fd_govt_fund), 2)
  ) %>%
  write_csv("data/results/tables/table_1_nmhss_descriptives.csv")

# SGM facilities
nmhss_eq_lags %>%
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
  write_csv("data/results/tables/nmhss_lgbtq_perc_descriptives.csv")

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
  write_csv("data/results/tables/nmhss_govt_fund_descriptives.csv")

# ORTHOGONAL REPARAMETERIZATION -------------------------------------------

# ORIGINAL MODEL - ONE LAG

# Year variable controls for trending
stigma_opm <- opm(lgbtq_perc ~  state_policy + l_state_policy + govt_fund + l_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("state", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_perc"-nmhss_eq_lags$"l_lgbtq_perc") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_perc"-nmhss_eq_lags$"l2_lgbtq_perc") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"govt_fund"-nmhss_eq_lags$"l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l_govt_fund"-nmhss_eq_lags$"l2_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_perc"-nmhss_eq_lags$"l3_lgbtq_perc") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_perc"-nmhss_eq_lags$"l4_lgbtq_perc") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"l2_govt_fund"-nmhss_eq_lags$"l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"l3_govt_fund"-nmhss_eq_lags$"l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# Arellano and Bond (1991) m2 statistic is NOT significant, therefore we cannot
# reject the null; there is no evidence of serial autocorrelation.

# ORIGINAL MODEL - FIRST DIFFERENCE OF GOVT_FUND TO REMOVE UNIT ROOT

# Year variable controls for trending
stigma_opm <- opm(lgbtq_perc ~  state_policy + l_state_policy + fd_govt_fund + fd_l_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("state", "year"), 
                  # Derive posteriors and sample from them; more = more precise, but longer to run
                  n.samp = 5000, 
                  # Add time dummies to control for global effects to all cases across time?
                  add.time.indicators = FALSE)

# Summarize the model
summary(stigma_opm)

# Arellano and Bond (1991) m2 statistic
opms_e <- (nmhss_eq_lags$"lgbtq_perc"-nmhss_eq_lags$"l_lgbtq_perc") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l_lgbtq_perc"-nmhss_eq_lags$"l2_lgbtq_perc") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"state_policy"-nmhss_eq_lags$"l_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l_state_policy"-nmhss_eq_lags$"l2_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"fd_govt_fund"-nmhss_eq_lags$"fd_l_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"fd_l_govt_fund"-nmhss_eq_lags$"fd_l2_govt_fund")

opms_e_2 <- (nmhss_eq_lags$"l2_lgbtq_perc"-nmhss_eq_lags$"l3_lgbtq_perc") - median(stigma_opm$samples$rho)*(nmhss_eq_lags$"l3_lgbtq_perc"-nmhss_eq_lags$"l4_lgbtq_perc") - median(stigma_opm$samples$beta[,1])*(nmhss_eq_lags$"l2_state_policy"-nmhss_eq_lags$"l3_state_policy") - median(stigma_opm$samples$beta[,2])*(nmhss_eq_lags$"l3_state_policy"-nmhss_eq_lags$"l4_state_policy") - median(stigma_opm$samples$beta[,3])*(nmhss_eq_lags$"fd_l2_govt_fund"-nmhss_eq_lags$"fd_l3_govt_fund") - median(stigma_opm$samples$beta[,4])*(nmhss_eq_lags$"fd_l3_govt_fund"-nmhss_eq_lags$"fd_l4_govt_fund")

length(opms_e*opms_e_2)

opms_cov <- sum(opms_e*opms_e_2, na.rm=TRUE)/(length(opms_e*opms_e_2)-sum(is.na(opms_e*opms_e_2)))

opms_var <- sum(opms_e*opms_e, na.rm=TRUE)/(length(opms_e*opms_e)-sum(is.na(opms_e*opms_e)))

ABOND_m2_test <- opms_cov/(opms_var^(1/2))

ABOND_m2_test

2 * pnorm(-abs(ABOND_m2_test))

# FINAL MODEL - ORTHOGONAL REPARAMETERIZATION -----------------------------

# Year variable controls for trending
stigma_opm <- opm(lgbtq_perc ~  state_policy + l_state_policy + fd_govt_fund + fd_l_govt_fund + year,
                  # Call the data set
                  data = nmhss_eq_lags,
                  # Specify panel data
                  index = c("state", "year"), 
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

# OLS fixed-effects model

# Select the variables of interest
nmhss_plm <- nmhss_eq_lags %>%
  select(lgbtq_perc, l_lgbtq_perc, state_policy, l_state_policy, fd_govt_fund, 
         fd_l_govt_fund, state, year) %>%
  as_tibble()
head(nmhss_plm)

# Scale all variables to yield standardized coefficients
nmhss_plm_1 <- lapply(nmhss_plm[, -c(7, 8)], scale) %>%
  as_tibble() %>%
  # Add year and state columns back
  mutate(year = nmhss_plm$year, state = nmhss_plm$state)

# Set the data into panel data
nmhss_plm_2 <- pdata.frame(nmhss_plm_1, index = c("state", "year"))

# Estimate fixed-effects model
reg_fe <- plm(lgbtq_perc ~ l_lgbtq_perc + state_policy + l_state_policy + 
                fd_govt_fund + fd_l_govt_fund + year,
                  data = nmhss_plm_2, 
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

# Get geospatial data - https://rpubs.com/lokigao/maps
us_map <- map_data("state")

# Show fonts available
windowsFonts()

# SGM-tailored programming by state
nmhss_map <- nmhss_eq_lags %>% 
  rename(region = state) %>%
  filter(year %in% c(2014, 2019)) %>%
  select(region, year, lgbtq_perc) %>% 
  right_join(us_map, by = "region") %>%
  filter(!is.na(year))
nmhss_map

# Map of LGBT programming at substance use facilities 
nmhss_choropleth <- ggplot(data = nmhss_map, aes(x = long, y = lat, group = group, 
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
    option = "C"
  ) +
  theme(
    legend.position = "bottom",
    # Add Times New Roman font
    text = element_text(family = "serif"),
    strip.text.x = element_text(size = 16)
  ) +
  facet_wrap(~ year)
nmhss_choropleth

# Save the plot
# ggsave(filename = "data/results/nmhss_choropleth.png", plot = nmhss_choropleth, 
#        width = 7.22, height = 3.5)
