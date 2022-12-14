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
age = c("total_pop","pop_under_20","pop_20_64","pop_65_plus")
acs_data <- acs_data %>% filter(measure %in% age)
age_data <- acs_data


# compress and save -----------------------------------------------------------------
savepath = "population/DC/age/data/distribution/"
write_csv(age_data, xzfile(paste0(savepath,"dc001_sdad_2019_age.csv.xz"), compression = 9))


