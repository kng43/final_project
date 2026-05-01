## Data Cleaning and Variable Creation

library(tidyverse)
library(sf)
library(tidycensus)
library(dotenv)
library(lubridate)

load_dot_env()

credential <- Sys.getenv("census_api_key")

variable_names <- load_variables(2024, "acs5")

### PROJECT VARIABLE LIST ###
# ---- Economic ---- #
# B19013_001  - Median household income
# B17020_002  - Population below poverty line (count)
# B17020_001  - Poverty universe total (denominator)
# B23025_005  - Unemployed civilians (count)
# B23025_003  - Civilian labor force total (denominator)

# ---- Education ---- #
# B15003_022  - Population with bachelor's degree (count)
# B15003_001  - Population 25+ total (denominator)

# ---- Housing ---- #
# B25064_001  - Median gross rent
# B25077_001  - Median home value
# B25003_003  - Renter-occupied units (count)
# B25003_001  - Total occupied units (denominator)
# B25002_003  - Vacant housing units (count)
# B25001_001  - Total housing units (denominator)

# ---- Race/Ethnicity ---- #
# B01003_001  - Total population (denominator)
# B02001_002  - White alone (count)
# B02001_003  - Black or African American alone (count)
# B03003_003  - Hispanic or Latino (count)
# B05002_013  - Foreign-born population (count)

# ---- Mobility ---- #
# B08013_001  - Aggregate travel time to work 
# B08012_001  - Number of workers 16+ who commute (denominator for mean commute calculation).

vars <- c("B19013_001", "B17020_002", "B17020_001", "B23025_005", 
          "B23025_003", "B15003_022", "B15003_001","B25064_001", 
          "B25077_001", "B25003_003", "B25003_001", "B25002_003", 
          "B25001_001", "B01003_001", "B02001_002", "B02001_003", 
          "B03003_003", "B05002_013", "B08013_001", "B08012_001")

### VARIABLE EXTRACTION ###
nyc_2022 <- get_acs(
  geography = "tract",
  variables = vars,
  year = 2022,
  output = "wide",
  state = "NY",
  county = c("Kings", "Queens", "Richmond", "Bronx", "New York"),
  key = credential,
  geometry = TRUE)

nyc_2024 <- get_acs(
  geography = "tract",
  variables = vars,
  year      = 2024,
  output    = "wide",
  state     = "NY",
  county    = c("Kings", "Queens", "Richmond", "Bronx", "New York"),
  key       = credential,
  geometry  = TRUE)

# arrests data
nyc_arrests <- read_csv("data/NYPD_Arrests_Data.csv") |>
  janitor::clean_names() |>
  filter(!is.na(lon_lat)) |>
  select(-lon_lat)

nyc_arrests <- st_as_sf(nyc_arrests,
                        coords = c("longitude", "latitude"),
                        remove = TRUE,
                        crs = 4326)

# schools data
nyc_schools <- st_read("data/schools/SchoolPoints_APS_2024_08_28.shp") |>
  st_transform(crs = 4326)

# centers of population data
nyc_centers <- read_csv(
  "https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR36.txt") |>
  janitor::clean_names() |>
  filter(countyfp %in% c("005", "047", "061", "081", "085"))

nyc_centers <- st_as_sf(nyc_centers, 
                          coords = c("longitude", "latitude"),
                          remove = TRUE,
                          crs = 4326)  

# third places data
third_place <- read.csv("data/third_place_index_1.0.csv")

nyc_tracts <- nyc_2022$GEOID

third_place_nyc <- third_place |>
  filter(geoid %in% nyc_tracts) |>
  select(GEOID = geoid, park_count, food_beverage_count, traditional_retail_count) |>
  mutate(GEOID = as.character(GEOID))

### MERGE AND CLEAN ###
# drop the margin of error columns from both before merging #
nyc_2022_clean <- nyc_2022 |>
  select(GEOID, NAME, ends_with("E"), geometry)

nyc_2024_clean <- nyc_2024 |>
  select(GEOID, NAME, ends_with("E"), geometry)

### bind dataframe by rows

nyc_combined <- bind_rows(`2022` = nyc_2022_clean, 
                          `2024` = nyc_2024_clean, 
                          .id = "year")

### CREATE NEW VARIABLES TO COMPUTE SHARES AND RATE OF CHANGE ### 
# 2024 variables (shares and levels) #

nyc_acs <- nyc_combined |>
  #renaming for ease
  rename(median_income = B19013_001E,
         median_rent = B25064_001E,
         median_home_value = B25077_001E) |>
  #converting to percentages/rates
  mutate(pct_poverty = (B17020_002E / B17020_001E) * 100,
         unemployment_rate = (B23025_005E / B23025_003E) * 100,
         pct_bachelor = (B15003_022E / B15003_001E) * 100,
         pct_renter = (B25003_003E / B25003_001E) * 100,
         vacancy_rate = (B25002_003E / B25001_001E) * 100,
         pct_white = (B02001_002E / B01003_001E) * 100,
         pct_black = (B02001_003E / B01003_001E) * 100,
         pct_hispanic = (B03003_003E / B01003_001E) * 100,
         pct_foreign_born = (B05002_013E / B01003_001E) * 100,
         mean_commute = B08013_001E / B08012_001E) |>
  # creating change variables
  group_by(GEOID) |>
  mutate(change_income = 
           100*(median_income - lag(median_income, order_by = year)) / lag(median_income, order_by = year),
         change_rent = 
           100*(median_rent - lag(median_rent, order_by = year)) / lag(median_rent, order_by = year),
         change_poverty = pct_poverty - lag(pct_poverty, order_by = year),
         change_unemployment = unemployment_rate - lag(unemployment_rate, order_by = year)
         ) |>
  ungroup()

# Creating Crime data/variable #

# add a buffer of 800 meters to each population center
nyc_centers_buffered <- st_buffer(
  nyc_centers, 
  dist = units::set_units(800, "m")
)

# spatial join the unbuffered shapes to the buffer shapes: arrests
centers_crime_joined <- st_join(
  nyc_centers_buffered,
  nyc_arrests, 
  join = st_intersects
)

# count the arrests
nyc_crime <- centers_crime_joined |>
  st_drop_geometry() |>
  mutate(GEOID = str_glue("{statefp}{countyfp}{tractce}")) |>
  group_by(GEOID, countyfp) |>
  summarize(arrests = n()) |>
  ungroup()

# creating Schools Variable #

# spatial join the unbuffered shapes to the buffer shapes: schools
centers_schools_joined <- st_join(
  nyc_centers_buffered,
  nyc_schools, 
  join = st_intersects
)

# count the schools
nyc_educ <- centers_schools_joined |>
  st_drop_geometry() |>
  mutate(GEOID = str_glue("{statefp}{countyfp}{tractce}")) |>
  group_by(GEOID) |>
  summarize(schools = n()) |>
  ungroup()

# merging crime data, schools data, and filtering to 2024 #

nyc_final <- left_join(nyc_acs, nyc_crime, by = join_by(GEOID))

nyc_final <- left_join(nyc_final, nyc_educ, by = join_by(GEOID))

nyc_final <- left_join(nyc_final, third_place_nyc, by = join_by(GEOID))

nyc_final <- nyc_final |>
  filter(year == 2024) |>
  #cleaning new arrests and schools variables |>
  mutate(arrests = if_else(is.na(arrests), 0, arrests),
         schools = if_else(is.na(schools), 0, arrests)) |>
  select(
    GEOID, NAME, countyfp, geometry,
    # modeling variables
    pct_poverty, unemployment_rate, median_income,
    pct_bachelor, median_rent, median_home_value,
    pct_renter, vacancy_rate, pct_white, pct_black,
    pct_hispanic, pct_foreign_born, mean_commute, arrests, 
    schools, park_count, food_beverage_count, traditional_retail_count,
    # change variables
    change_income, change_rent, change_poverty, change_unemployment)

### Create Shapefile to preserve Geometry ###
st_write(nyc_final, "data/nyc_final.shp", delete_dsn = TRUE)