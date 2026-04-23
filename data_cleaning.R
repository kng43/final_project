## Data Cleaning and Variable Creation

library(tidyverse)
library(sf)
library(tidycensus)
library(dotenv)

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
# B08012_001  - Number of workers who commute (workers 16+ who did not work from home. Use as a denominator for mean commute time if we want).

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
  geometry  = FALSE)



### MERGE AND CLEAN ###
# ---- drop the margin of error columns from both before merging ----#
nyc_2022_clean <- nyc_2022 |>
  select(GEOID, NAME, ends_with("E"), geometry)

nyc_2024_clean <- nyc_2024 |>
  select(GEOID, ends_with("E"))

# ---- join on GEOID (the unique census tract identifier) ---- #
nyc_combined <- nyc_2022_clean |>
  left_join(nyc_2024_clean, by = "GEOID", suffix = c("_2022", "_2024"))



### CREATE NEW VARIABLES TO COMPUTE SHARES AND RATE OF CHANGE ### 
# ---- 2024 variables (shares and levels) ----#
nyc_model <- nyc_combined |>
  mutate(
    # ---- Economic ---- #
    pct_poverty      = B17020_002E_2024 / B17020_001E_2024,
    unemployment_rate = B23025_005E_2024 / B23025_003E_2024,
    median_income    = B19013_001E_2024,
    
    # ---- Education ---- #
    pct_bachelor     = B15003_022E_2024 / B15003_001E_2024,
    
    # ---- Housing ---- #
    median_rent      = B25064_001E_2024,
    median_home_value = B25077_001E_2024,
    pct_renter       = B25003_003E_2024 / B25003_001E_2024,
    vacancy_rate     = B25002_003E_2024 / B25001_001E_2024,
    
    # ---- Race/Ethnicity (2024 shares only) ---- #
    pct_white        = B02001_002E_2024 / B01003_001E_2024,
    pct_black        = B02001_003E_2024 / B01003_001E_2024,
    pct_hispanic     = B03003_003E_2024 / B01003_001E_2024,
    pct_foreign_born = B05002_013E_2024 / B01003_001E_2024,
    
    # --- Mobility ---
    mean_commute     = B08013_001E_2024 / B08012_001E_2024)

# ---- Change variables ---- #
nyc_model <- nyc_model |>
  mutate(
    change_income    = (B19013_001E_2024 - B19013_001E_2022) / B19013_001E_2022,
    change_rent      = (B25064_001E_2024 - B25064_001E_2022) / B25064_001E_2022,
    change_poverty   = (B17020_002E_2024 / B17020_001E_2024) - 
      (B17020_002E_2022 / B17020_001E_2022),
    change_unemployment = (B23025_005E_2024 / B23025_003E_2024) - 
      (B23025_005E_2022 / B23025_003E_2022))

# ---- Final Modelling Dataset to Work With ---- #
nyc_final <- nyc_model |>
  select(
    GEOID, NAME, geometry,
    # modelling variables
    pct_poverty, unemployment_rate, median_income,
    pct_bachelor, median_rent, median_home_value,
    pct_renter, vacancy_rate, pct_white, pct_black,
    pct_hispanic, pct_foreign_born, mean_commute,
    # supplementary change variables
    change_income, change_rent, change_poverty, change_unemployment)



### CREATE RDS FILES (BECAUSE WE HAVE GEOMETRY COLUMNS) 
# save full merged data as a backup
saveRDS(nyc_combined, "data/nyc_combined_raw.rds")

# save final modelling dataset
nyc_final <- nyc_final |> #drop tracts with missing values on the modelling variables (not change variables) because PCA and K means behave unexpectedly if NAs are present.
  drop_na(pct_poverty, unemployment_rate, median_income,
          pct_bachelor, median_rent, median_home_value,
          pct_renter, vacancy_rate, pct_white, pct_black,
          pct_hispanic, pct_foreign_born, mean_commute)
saveRDS(nyc_final, "data/nyc_final.rds")

