# Get census transportation population characteristics variables
# Mean travel time to work S0801_C01_046E
# % Carpooled to work S0801_C01_004E
# % No vehicle available S2504_C02_027E

# Getting all data for VA, MD, and DC for 2016-2020 counties and tracts (ACS5)
# Subsetting to NCR

library(tidycensus)
library(tidyverse)
states <- c("VA", "MD", "DC")
geographies <- c("county", "tract")
years <- c(2016:2020)
commute <- NULL
commutes <- NULL
for(state in states){
  for(year in years){
      for(geography in geographies){
        commute <- get_acs(geography = geography, variable = "S0801_C01_046E", state = state, year = year,
                         geometry = FALSE,
                         survey = "acs5",cache_table = TRUE) %>% select(-variable, -moe) %>%
          mutate(measure = "commute_time", year = year, region_type = as.character(geography),
                 measure_type = "float", measure_units = "minutes") %>%
          rename(geoid = GEOID, region_name = NAME, value = estimate)
        commutes <- rbind(commutes, commute)
      }
    }
  }

carpool <- NULL
carpools <- NULL
for(state in states){
  for(year in years){
    for(geography in geographies){
      carpool <- get_acs(geography = geography, variable = "S0801_C01_004E", state = state, year = year,
                         geometry = FALSE,
                         survey = "acs5",cache_table = TRUE) %>% select(-variable, -moe) %>%
        mutate(measure = "perc_carpool", year = year, region_type = as.character(geography),
               measure_type = "float", measure_units = "percent") %>%
        rename(geoid = GEOID, region_name = NAME, value = estimate)
      carpools <- rbind(carpools, carpool)
    }
  }
}

vehicle <- NULL
vehicles <- NULL
for(state in states){
  for(year in years){
    for(geography in geographies){
      vehicle <- get_acs(geography = geography, variable = "S2504_C02_027E", state = state, year = year,
                         geometry = FALSE,
                         survey = "acs5",cache_table = TRUE) %>% select(-variable, -moe) %>%
        mutate(measure = "perc_no_vehicle", year = year, region_type = as.character(geography),
               measure_type = "float", measure_units = "percent") %>%
        rename(geoid = GEOID, region_name = NAME, value = estimate)
      vehicles <- rbind(vehicles, vehicle)
    }
  }
}

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_commutes <- commutes %>% dplyr::filter(str_detect(geoid, ncr_counties))
ncr_carpools <- carpools %>% dplyr::filter(str_detect(geoid, ncr_counties))
ncr_vehicles <- vehicles %>% dplyr::filter(str_detect(geoid, ncr_counties))

write_csv(ncr_commutes, xzfile("./Population Characteristics/data/distribution/ncr_cttr_acs_2016_2020_commutes.csv.xz", compression = 9))
write_csv(ncr_carpools, xzfile("./Population Characteristics/data/distribution/ncr_cttr_acs_2016_2020_carpools.csv.xz", compression = 9))
write_csv(ncr_vehicles, xzfile("./Population Characteristics/data/distribution/ncr_cttr_acs_2016_2020_vehicles.csv.xz", compression = 9))
