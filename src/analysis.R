# Dependencies
library(tidyverse)
library(geojsonio)
library(broom)

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
  select(state, percent) %>%
  mutate(
    percent = round(percent * 100, 2)
  )
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
    name = "Proportion with LGBTQ+ Programming",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 14,
      title.hjust = 0.5
      ),
    option = "D"
  ) +
  theme(legend.position = "bottom")
plot_lgbt_2019

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
