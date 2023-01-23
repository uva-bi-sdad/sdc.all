## THIS IS THE PART OF THE FILE WE WORKED ON OVER ZOOM!
# Get census sex population characteristics variables
# Age by sex S0101_C01_001 total population
# Age by sex S0101_C03_001 percent male

library(tidycensus)
library(tidyverse)

# To see all variable names
variables <- load_variables(2012, "acs5/subject", cache = TRUE)

# Codes are different for each year ========================
# S0101_C03_001 = Female!!Estimate!!Total population in 2010
# S0101_C03_001 = Estimate!!Male!!Total population in 2017

# Whatever geos/years you need to make a loop
states <- c("VA")
geographies <- c("county", "tract")
years <- c(2010:2021) # years where S0101_C03_001 is male total
  

# This is a test to make sure the variables we want come in the geos/years we need
get_acs(geography = "county", variable = "S0101_C01_001", state = "VA", year = 2017,
        geometry = FALSE, survey = "acs5",cache_table = TRUE)
get_acs(geography = "county", variable = "S0101_C02_001", state = "VA", year = 2010,
        geometry = FALSE, survey = "acs5",cache_table = TRUE) %>% select(-variable)

# Initalize the tables for the loop

total <- NULL
male <- NULL
sexes <- NULL

# Age by sex S0101_C01_001 total population
# Age by sex S0101_C02_001 percent male (for years 2010-2016)
# Age by sex S0101_C03_001 percent male (for years 2017-2021)

for(state in states){
  for(year in years){
    for(geography in geographies){
      total <- get_acs(geography = geography, variable = "S0101_C01_001", state = state, year = year,
                       geometry = FALSE,
                       survey = "acs5",cache_table = TRUE) %>% select(estimate)
      
      # If loop needed, ACS changed variable ID's for male population in 2017
      if(year < 2017){
        male <- get_acs(geography = geography, variable = "S0101_C02_001", state = state, year = year,
                        geometry = FALSE,
                        survey = "acs5",cache_table = TRUE) %>% select(-variable)
        }   
      else {
        male <- get_acs(geography = geography, variable = "S0101_C03_001", state = state, year = year,
                        geometry = FALSE,
                        survey = "acs5",cache_table = TRUE) %>% select(-variable)
      }
      
      male <- male %>% mutate(value = estimate/total$estimate * 100, moe = moe/total$estimate * 100) %>%
        mutate(measure = "perc_male", year = year, region_type = as.character(geography),
               measure_type = "float", measure_units = "count") %>% select(-c(estimate)) # measure_units don't know what unit to use
      
      
      sexes <- rbind(sexes, male)
    }
  }
} # there are 142 NaN's


# Save tables to data/distribution folders, fill out measure_info for measures
# Once you finish a piece of code you can move it to the code/distribution folder, you can keep scrap/archive code in the code/working folder (same with the data folders, for scrap/archive tables)


write_csv(sexes, xzfile("./data/distribution/va_cttr_2010_2021_perc_male.csv.xz", compression = 9))
