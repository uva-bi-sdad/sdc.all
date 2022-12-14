# Age distribution at the geographics level of ACS.

library(readr)
library(dplyr)


# upload data from different geographies and methods -------------------------------
uploadpath = "population/DC/overall_DC/"
acs_data <- read_csv(xzfile(paste0(uploadpath, "new_geography_synthetic/housing_units_distribution_method/ssl/data/working/acs_data.csv.xz")))

# shape acs variable to the same format than others data
acs_data <- acs_data %>% mutate(region_type='block group', year=2019)
colnames(acs_data) <- c('geoid','region_name','measure','value','region_type','year')
acs_data <- acs_data %>% select(geoid,region_type,year,region_name,measure,value)
acs_data <- acs_data %>% mutate(synthetic_method=NA)


# filter the age -----------------------------------------------------------------
race = c("total_pop","pop_white","pop_black","pop_AAPI","pop_native","pop_hispanic_or_latino")
acs_data <- acs_data %>% filter(measure %in% race)
race_data <- acs_data


# compress and save -----------------------------------------------------------------
savepath = "population/DC/race/data/distribution/"
write_csv(race_data, xzfile(paste0(savepath,"dc001_sdad_2019_race.csv.xz"), compression = 9))


