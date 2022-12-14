# Age distribution at the geographics level of ACS.

library(readr)
library(dplyr)


# upload data from different geographies and methods -------------------------------
uploadpath = "population/VA/arlington/overall_arlington/"
acs_data <- read_csv(xzfile(paste0(uploadpath, "new_geography_synthetic/housing_units_distribution_method/parcels/data/working/acs_data.csv.xz")))

# shape acs variable to the same format than others data
acs_data <- acs_data %>% mutate(region_type='block group', year=2019)
colnames(acs_data) <- c('geoid','region_name','measure','value','region_type','year')
acs_data <- acs_data %>% select(geoid,region_type,year,region_name,measure,value)
acs_data <- acs_data %>% mutate(synthetic_method=NA)

# method 1: (hud=housing units distribution) ----------------------------------------
#ca_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/zip_code/data/distribution/va059_zc_sdad_2019_demographics.csv.xz")))


# filter the age -----------------------------------------------------------------
race = c("total_pop","pop_white","pop_black","pop_AAPI","pop_native","pop_hispanic_or_latino")
acs_data <- acs_data %>% filter(measure %in% race)
race_data <- acs_data

# method 1: housing using distribution  ---------------------------------------------------
#ca_hud_data <- zc_hud_data %>% filter(measure %in% age)
#ca_hud_data <- ca_hud_data %>% mutate(synthetic_method="housing units distribution")

# method 2: two stages IPF  ---------------------------------------------------


# combine all the data ---------------------
#age_data <- rbind(acs_data,ca_hud_data)


# compress and save -----------------------------------------------------------------
savepath = "population/VA/arlington/race/data/distribution/"
write_csv(race_data, xzfile(paste0(savepath,"va013_sdad_2019_race.csv.xz"), compression = 9))


