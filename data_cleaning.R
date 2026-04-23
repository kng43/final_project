## Data Cleaning and Variable Creation

library(tidyverse)
library(sf)
library(tidycensus)
library(dotenv)

load_dot_env()

credential <- Sys.getenv("census_api_key")

variable_names <- load_variables(2024, "acs5")

# median household income - B19013_001
# population with a bachelor’s degree - B15003_022
# population below the poverty line - B17020_002

nyc <- get_acs(
  geography = "tract",
  variables = c("B19013_001E", "B15003_022E", "B17020_002E"),
  year = 2022,
  output = "wide",
  state = "NY",
  county = c("Kings", "Queens", "Richmond", "Bronx", "New York"),
  key = credential,
  geometry = TRUE)

