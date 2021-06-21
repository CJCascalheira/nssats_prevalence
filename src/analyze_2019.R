# Dependencies
library(tidyverse)
library(viridis)
library(table1)

# Import
ssats_2019 <- read_csv("data/cleaned/ssats_2019.csv")

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
  )
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

# Map of LGBT programming
plot_lgbt_2019 <- ggplot(data = us_map_lgbt, aes(x = long, y = lat, group = group, fill = percent)) +
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
plot_lgbt_2019

# Save the plot
ggsave(filename = "data/results/ssats_2019_choropleth.png", plot = plot_lgbt_2019)

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
