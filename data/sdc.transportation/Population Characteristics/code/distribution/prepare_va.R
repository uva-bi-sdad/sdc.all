library(tidycensus)
library(tidyverse)

# To see all variable names
variables <- load_variables(2015, dataset = "acs5/profile", cache = TRUE)

# Whatever geos/years you need to make a loop
states <- c("VA")
geographies <- c("county", "tract")
years <- c(2010:2021) # years where S0101_C03_001 is male total
  

# <!--------------------------------------------------------------------------------->
# <!-------------------- Percent of households with no motor vehicles --------------->


# DP04_0057P --> Percent!!VEHICLES AVAILABLE!!Occupied housing units!!No vehicles available (2014 -)
# DP04_0058P --> Percent!!VEHICLES AVAILABLE!!Occupied housing units!!No vehicles available (2015 +)

carVars <- c("DP04_0057P", "DP04_0058P")
total_car <- NULL

for(state in states){
  for(geography in geographies){
    for(year in years){
      if(year < 2015){ # The variable ID changes form 2014 to 2015
        carData <- get_acs(geography = geography, variable = carVars[1], state = state,
                           year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE,
                           show_call = TRUE) %>% mutate(value = estimate, year = year, region_type = geography, measure_type = "float", measure_units = "count") %>% select(-c(estimate, variable))
        total_car <- rbind(total_car, carData)
      }
      else if (year >= 2015){
        carData <- get_acs(geography = geography, variable = carVars[2], state = state,
                           year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE,
                           show_call = TRUE) %>% mutate(value = estimate, year = year, region_type = geography, measure_type = "float", measure_units = "count") %>% select(-c(estimate, variable))
        total_car <- rbind(total_car, carData)
      }
      else{
        next
      }
    }
  }
}

write_csv(total_car, xzfile("./Population Characteristics/data/distribution/va_cttr_2010_2021_perc_no_car_households.csv.xz", compression = 9))



