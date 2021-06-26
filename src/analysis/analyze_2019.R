# Dependencies
library(tidyverse)
library(viridis)
library(table1)
library(scales)

# Import
ssats_2019 <- read_csv("data/cleaned/ssats_2019.csv")
mhss_2019 <- read_csv("data/cleaned/mhss_2019.csv")
equality_table <- read_csv("data/equality_tables/equality_table_clean_2019.csv")

# PREVALENCE BY STATE -----------------------------------------------------

# LGBT-specific programming by state
lgbt_by_state <- ssats_2019 %>%
  group_by(state) %>%
  count(lgbt) %>%
  mutate(
    # Total facilities by state
    sum = sum(n),
    # Percent of LGBT programming
    percent = n / sum
  ) %>%
  filter(state != "ZZ")
lgbt_by_state

# Select only the positive cases
lgbt_by_state_1 <- lgbt_by_state %>%
  filter(lgbt == 1) %>%
  ungroup() %>%
  # Select columns for merging
  select(state, percent)
head(lgbt_by_state_1)

# Get geospatial data - https://rpubs.com/lokigao/maps
us_map <- map_data("state")

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# Merge data
us_map_lgbt <- us_map %>%
  left_join(state_names) %>%
  left_join(lgbt_by_state_1)
head(us_map_lgbt)

# Map of LGBT programming at substance use facilities 
ssats_2019_choropleth <- ggplot(data = us_map_lgbt, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(color = "white") +
  theme_void() +
  # Set the coordinate orientation
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis(
    name = "Percent of Facilities with LGBTQ+ Programming",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 17,
      title.hjust = 0.5
      ),
    option = "D"
  ) +
  theme(legend.position = "bottom")
ssats_2019_choropleth

# Save the plot
# ggsave(filename = "data/results/ssats_2019_choropleth.png", plot = ssats_2019_choropleth)

#######

# Prepare the merged dataframe
lgbt_map_mhss <- mhss_2019 %>%
  filter(!is.na(lgbt)) %>%
  group_by(state) %>%
  count(lgbt) %>%
  mutate(
    # Total facilities by state
    sum = sum(n),
    # Percent of LGBT programming
    percent = n / sum
  ) %>%
  select(lgbt, percent, state) %>%
  ungroup() %>%
  # Select only the positive cases
  filter(lgbt == 1) %>%
  left_join(state_names) %>%
  left_join(us_map, by = "region") %>%
  filter(state != "ZZ")

# Map of LGBT programming at mental health clinics
mhss_2019_choropleth <- ggplot(data = lgbt_map_mhss, aes(x = long, y = lat, group = group, fill = percent)) +
  geom_polygon(color = "grey") +
  theme_void() +
  # Set the coordinate orientation
  coord_map("bonne", lat0 = 50) +
  scale_fill_viridis(
    name = "Percent of Facilities with LGBTQ+ Programming",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 17,
      title.hjust = 0.5
    ),
    option = "D"
  ) +
  theme(legend.position = "bottom")
mhss_2019_choropleth

# Save the plot
# ggsave(filename = "data/results/mhss_2019_choropleth.png", plot = mhss_2019_choropleth)

# TABLES: FEATURES OF FACILITIES ------------------------------------------

# Remove missing values
ssats_2019_a <- ssats_2019 %>%
  filter(!is.na(lgbt)) %>%
  mutate(
    lgbt = factor(lgbt, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Select basic categories
ssats_2019_basic <- ssats_2019_a %>%
  select(lgbt, owner, accredited, sliding_fee, residential, outpatient)

# Table1 code for complete table with column spanner
labels_ssats_basic <- list(
  variables = list(
    owner = "Ownership",
    accredited = "Licensed facility",
    sliding_fee = "Offers sliding fee",
    residential = "Residential treatment",
    outpatient = "Outpatient treatment"
  ),
  groups = list("LGBTQ-Specific Services Offered", "")
)

# Set of the table
strata_basic <- c(split(ssats_2019_basic, ssats_2019_basic$lgbt), list(Total=ssats_2019_basic))

# Descriptive characteristics, full, with spanner
table1(strata_basic, labels = labels_ssats_basic, groupspan = c(2, 1))

#######

# Select SUPPORT categories
ssats_2019_support <- ssats_2019_a %>%
  select(lgbt, starts_with("supp"))

# Table1 code for complete table with column spanner
labels_ssats_support <- list(
  variables = list(
    supp_women = "Programming for women",
    supp_men = "Programming for men",
    supp_cooccur = "Treats co-occuring disorders",
    supp_hiv = "HIV-specific support",
    supp_sexabuse = "Programming for sex abuse",
    supp_ipv = "Programming for IPV",
    supp_trauma = "Programming for trauma"
  ),
  groups = list("LGBTQ-Specific Services Offered", "")
)

# Set of the table
strata_support <- c(split(ssats_2019_support, ssats_2019_support$lgbt), list(Total=ssats_2019_support))

# Descriptive characteristics, full, with spanner
table1(strata_support, labels = labels_ssats_support, groupspan = c(2, 1))

# CORRELATES OF LGBT PROGRAMMING ------------------------------------------

# Number of facilities missing values for LGBT
ssats_2019 %>%
  filter(is.na(lgbt)) %>%
  nrow()

# Ownership type
ssats_2019 %>%
  # Remove missing values
  filter(!is.na(lgbt)) %>%
  group_by(owner) %>%
  count(lgbt) %>%
  mutate(
    # Total ownership type
    sum = sum(n),
    # Percent of LGBT programming
    percent = n / sum
  )

# SIMPLE CORRELATION: COMPARE PERCENTAGES ---------------------------------

# Association between LGBTQ-specific programming at substance use vs. mental health facilities
ssats_mhss_by_state <- mhss_2019 %>%
  filter(!is.na(lgbt)) %>%
  group_by(state) %>%
  count(lgbt) %>%
  mutate(
    # Total facilities by state
    sum = sum(n),
    # Percent of LGBT programming
    percent = n / sum
  ) %>%
  select(lgbt, percent, state) %>%
  ungroup() %>%
  # Select only the positive cases
  filter(lgbt == 1) %>%
  rename(percent_mhss = percent) %>%
  left_join(lgbt_by_state_1) %>%
  rename(percent_ssats = percent) %>%
  filter(state != "ZZ")
head(ssats_mhss_by_state)

# Plot the relationship between SSATS and MHSS
ssats_mhss_by_state_plot <- ssats_mhss_by_state %>%
  ggplot(aes(x = percent_ssats, y = percent_mhss)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  annotate(x = 0.4, y = 0.1, 
           label = paste("r = ", round(cor(ssats_mhss_by_state$percent_mhss, ssats_mhss_by_state$percent_ssats), 2)), 
           geom = "text", size = 5, color = "blue") +
  theme_bw() +
  labs(
    x = "% Substance Use Facilities with LGBTQ+ Programming",
    y = "% Mental Health Facilities with LGBTQ+ Programming",
    title = "State-Level Availability of LGBTQ-Specific Behavioral Health Services"
  )
ssats_mhss_by_state_plot 

# Save the plot
# ggsave(filename = "data/results/ssats_mhss_by_state_plot.png", plot = ssats_mhss_by_state_plot)

# SIMPLE CORRELATION: STRUCTURAL STIGMA -----------------------------------

# Prepare equality table
equality_table_long <- equality_table %>%
  filter(!is.na(state)) %>%
  select(state, overall_policy_total) %>%
  distinct(state, .keep_all = TRUE) %>%
  left_join(ssats_mhss_by_state) %>%
  select(-lgbt) %>%
  gather(key = "facility", value = "percent", -overall_policy_total, -state)
equality_table_long 

# Calculate correlations
equality_table_cor <- equality_table_long %>%
  group_by(facility) %>%
  summarize(
    cor = cor(overall_policy_total, percent)
  )
equality_table_cor

# Prepare annotated text
dat_text <- data.frame(
  label = c(paste("r =", round(equality_table_cor$cor[1], 2)), paste("r =", round(equality_table_cor$cor[2], 2))),
  facility   = c("Mental Health Facilities", "Substance Use Treatment Facilities")
)
dat_text 

# Create a plot
stigma_facilities_plot <- equality_table_long %>%
  # Rename facets
  mutate(
    facility = recode(facility, "percent_mhss" = "Mental Health Facilities",
                      "percent_ssats" = "Substance Use Treatment Facilities")
  ) %>%
  ggplot(aes(x = overall_policy_total, y = percent)) +
  geom_point() +
  facet_wrap(~ facility) +
  geom_smooth(method = "lm", color = "blue") +
  theme_bw() +
  geom_text(
    data = dat_text,
    mapping = aes(x = 30, y = 0.1, label = label),
    color = "blue",
    size = 5
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  labs(
    x = "Index of LGBTQ+ Policies from the Movement Advancement Project",
    y = "% of Behavioral Health Facilities \nwith LGBTQ+ Services",
    title = "Association Between State-Level Structural Stigma and \nLGBTQ-Specific Programming"
  ) +
  theme(
    strip.text = element_text(size = 12)
  )
stigma_facilities_plot

# Save plot
# ggsave(filename = "data/results/stigma_facilities_plot.png", plot = stigma_facilities_plot)
