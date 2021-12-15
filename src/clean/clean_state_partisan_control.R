# Dependencies
library(tidyverse)

# Data frame of state names and abbreviations
state_names <- data.frame(region = tolower(state.name))
state_names$state <- state.abb[match(state_names$region, tolower(state.name))]
head(state_names)

# Export state names
write_csv(state_names, "data/state_partisan_control/state_names.csv")
