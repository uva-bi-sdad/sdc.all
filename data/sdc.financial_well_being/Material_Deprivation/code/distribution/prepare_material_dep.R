library(XML)
library(stringr)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(readxl)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

#Population data - VA state extracted from Census ACS - Tract and County
data <- data.frame()
years <- 2015:2023
geo <- c("tract", "county")
for (i in years) {
  for (g in geo) {
    data_census <- get_acs(geography = g,
                           year = i,
                           survey = "acs5",
                           state = 'VA',
                           variables = c( adult_pop = 'B23025_002E',
                                          unemployed='B23025_005E',
                                          occupancy_all='B25014_001E',
                                          occupant_owner_1_to_1half='B25014_005E',
                                          occupant_owner_greater_1half='B25014_006E',
                                          occupant_owner_greater_2='B25014_007E',
                                          occupant_renter_1_to_1half='B25014_011E',
                                          occupant_renter_greater_1half='B25014_012E',
                                          occupant_renter_greater_2='B25014_013E',
                                          all_units = 'S2502_C01_001E',
                                          #rent_units='S2502_C05_001E',
                                          rent_units= ifelse(i > 2016, 'S2502_C05_001E', 'S2502_C03_001E'),
                                          rent_units_moe= ifelse(i > 2016, 'S2502_C05_001M', 'S2502_C03_001M'),
                                          households_total='B25044_001E',
                                          households_owner_no_vehicles='B25044_003E',
                                          households_renter_no_vehicles='B25044_010E'),
                           geometry = FALSE,
                           output = 'wide' )
    data_census$year = i
    data_census$region_type = g
    data <- rbind(data, data_census)
    data <- data[substr(data$GEOID, 1, 2) == "51",]

  }}

data <- data[, c("GEOID","year","adult_pop", "unemployed","occupancy_all", "occupant_owner_1_to_1half",
                 "occupant_owner_greater_1half","occupant_owner_greater_2","occupant_renter_1_to_1half",
                 "occupant_renter_greater_1half","occupant_renter_greater_2", "all_units", "rent_units",
                 "households_total", "households_owner_no_vehicles","households_renter_no_vehicles", "region_type")]


#Aggregating the results to Health Districts
county <- data_census

health_district <- read_csv("~/git/sdc.geographies_dev/VA/State Geographies/Health Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv")
health_district <- merge(x = health_district, y = county, by.x = "ct_geoid", by.y = "GEOID")

health_district <- health_district %>%
  group_by(hd_geoid, year) %>%
  summarise(
    adult_pop = sum(adult_pop, na.rm = TRUE),
    unemployed = sum(unemployed, na.rm = TRUE),
    occupancy_all = sum(occupancy_all, na.rm = TRUE),
    occupant_owner_1_to_1half = sum(occupant_owner_1_to_1half, na.rm = TRUE),
    occupant_owner_greater_1half = sum(occupant_owner_greater_1half, na.rm = TRUE),
    occupant_owner_greater_2 = sum(occupant_owner_greater_2, na.rm = TRUE),
    occupant_renter_1_to_1half = sum(occupant_renter_1_to_1half, na.rm = TRUE),
    occupant_renter_greater_1half = sum(occupant_renter_greater_1half, na.rm = TRUE),
    occupant_renter_greater_2 = sum(occupant_renter_greater_2, na.rm = TRUE),
    all_units = sum(all_units, na.rm = TRUE),
    rent_units = sum(rent_units, na.rm = TRUE),
    households_total = sum(households_total, na.rm = TRUE),
    households_owner_no_vehicles = sum(households_owner_no_vehicles, na.rm = TRUE),
    households_renter_no_vehicles = sum(households_renter_no_vehicles, na.rm = TRUE),
  )

health_district <- rename(health_district, GEOID = hd_geoid)
health_district$region_type <- 'health district'

health_district <- health_district[, c("GEOID","year","adult_pop", "unemployed","occupancy_all", "occupant_owner_1_to_1half","occupant_owner_greater_1half","occupant_owner_greater_2","occupant_renter_1_to_1half", "occupant_renter_greater_1half","occupant_renter_greater_2", "all_units", "rent_units", "households_total", "households_owner_no_vehicles","households_renter_no_vehicles", "region_type")]

data <- rbind(data, health_district)


# Producing % unempolyment, non-car ownership, non-home ownership, and overcrowding
# unemployment rate
data$unemployed <- data$unemployed/data$adult_pop
data$unemployed  <- log(data$unemployed + 1)
data$unemployed <- as.numeric(data$unemployed)

#% households without a car
data$noncar_ownership  <- (data$households_renter_no_vehicles+data$households_owner_no_vehicles) / data$households_total
data$noncar_ownership <- as.numeric(data$noncar_ownership)

#% households renting
data$nonhome_ownership  <- data$rent_units/data$all_units
data$nonhome_ownership <- as.numeric(data$nonhome_ownership)

# overcrowded households
data$household_overcrowding <- (
  data$occupant_owner_1_to_1half +
    data$occupant_owner_greater_1half +
    data$occupant_owner_greater_2 +
    data$occupant_renter_1_to_1half +
    data$occupant_renter_greater_1half +
    data$occupant_renter_greater_2
) / (data$occupancy_all)

data$household_overcrowding  <- log(1 + (data$household_overcrowding))
data$household_overcrowding <- as.numeric(data$household_overcrowding)

townsend_data <- subset(data, select = c(GEOID, region_type, unemployed, noncar_ownership, nonhome_ownership, household_overcrowding, year))

#Scaling the variables for each year and region type
min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

townsend_data <- townsend_data %>%
  group_by(region_type, year) %>%
  mutate(across(c(unemployed, noncar_ownership, nonhome_ownership, household_overcrowding),
                ~min_max_scale(.), .names = "z_{.col}")) %>%
  ungroup()

townsend_data <- townsend_data %>%
  group_by(region_type, year) %>%
  mutate(across(c(unemployed, noncar_ownership,nonhome_ownership,household_overcrowding ), ~scale(.), .names="z_{.col}")) %>% ungroup()

#Producing the Townsend Dep Index
townsend_data$Townsend_Index_z_sum <- townsend_data$z_unemployed + townsend_data$z_noncar_ownership + townsend_data$z_nonhome_ownership + townsend_data$z_household_overcrowding
townsend_data <- townsend_data %>%   group_by(region_type, year) %>%
  mutate(Townsend_Index_z_sum_z = scale(Townsend_Index_z_sum)) %>% ungroup()

townsend_data$Townsend_Index_z_sum_z <- (townsend_data$Townsend_Index_z_sum_z)

townsend_data$measure = 'material_deprivation_indicator'
townsend_data <- rename(townsend_data, value = Townsend_Index_z_sum_z, geoid = GEOID)

townsend_data <- townsend_data %>%
  group_by(year, region_type) %>%
  mutate(value = rescale(value)) %>%
  ungroup %>%
  mutate(value = as.character(value))

townsend_bi <- subset(townsend_data, select = c(geoid, year, measure, value)) %>%
  mutate(moe="")

#Write CSV
readr::write_csv(townsend_bi, xzfile(paste0("Material_Deprivation/data/distribution/va_hdcttr_vdh_", min(years), "_", max(years), "_material_deprivation_index.csv.xz"), compression = 9))

# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## convert
rcsv <- read.csv(xzfile(paste0("Material_Deprivation/data/distribution/va_hdcttr_vdh_", min(years), "_", max(years), "_material_deprivation_index.csv.xz"), open = "r"))
stnd <- standardize_all(rcsv)

# save standardized file
write.csv(stnd, file = xzfile(paste0("Material_Deprivation/data/distribution/va_hdcttr_vdh_", min(years), "_", max(years), "_material_deprivation_index_std.csv.xz")), row.names = FALSE)

