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

cook <- get_acs(
  geography = "county",
  variables = c("B19013_001E", "B15003_022E", "B17020_002E"),
  year = 2019,
  output = "wide",
  state = "IL",
  county = "Cook",
  key = credential)

