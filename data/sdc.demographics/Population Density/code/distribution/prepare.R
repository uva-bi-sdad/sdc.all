
library(tigris)
library(tidycensus)
library(dplyr)

# initializing the years for which we need data
years <- c(2015,2016,2017, 2018, 2019, 2020, 2021)

# store the results in the empty list
results_list <- list()

# we can just loop over years to get the particular year data and perform teh calculation
for (i in 1:length(years)) {
  year <- years[i]
  
  # get census tracts for VA in the given year, we will get the ALAND here which is in square meters
  tracts_va <- tracts(state = "VA", cb = FALSE, year = year)
  
  # get population data for VA tracts in the given year, estimate contains the population
  va_acs <- get_acs(geography = "tract", 
                    variables = "B01003_001",
                    year = year,
                    survey = "acs5",
                    state = "VA")
  
  # merge population data with VA tracts
  merged_data <- left_join(va_acs, tracts_va, by = "GEOID")
  merged_data <- left_join(merged_data, select(tracts_va, GEOID, ALAND), by = "GEOID")
  # rename columns with .x (just for accessing and joining them for calculation)
  merged_data <- rename(merged_data,
                        NAME = NAME.x,
                        ALAND = ALAND.x
  )
  # calculating population density 
  merged_data_index <- merged_data %>%
    group_by(GEOID) %>%
    summarise(
      weighted_population_total = sum(estimate * ALAND),
      total_land_area = sum(ALAND),
      sqme_sqmi = sum(ALAND) / 2589988.1103 # convert ALAND from square meters to square miles 
    ) %>%
    mutate(
      population_density = weighted_population_total / (total_land_area * sqme_sqmi)
    )
  
  # creating a new data frame according to  our format
  newdf <- merged_data %>% 
    select(GEOID, NAME) %>% 
    left_join(merged_data_index %>% 
                select(GEOID, population_density), 
              by = "GEOID") %>%
    mutate(year = year,
           measure = "Population_weighted_density_index",
           measure_type = "index",
           region_type = "tract",
           value = population_density,
           region_name = NAME,
           geoid = GEOID) %>%
    select(geoid, measure,measure_type, region_name, region_type, value, year)
  
  # adding the new data frame to the results list
  results_list[[i]] <- newdf
}

# combining the results into a single data frame
final_df <- do.call(rbind, results_list)



#write.csv(final_df, "~/PWD/va_cttr_2015_2021_population_density_index.csv", row.names = FALSE)
