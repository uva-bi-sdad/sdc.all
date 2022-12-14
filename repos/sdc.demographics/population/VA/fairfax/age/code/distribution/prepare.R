# Age distribution at the geographics level of ACS.

library(readr)
library(dplyr)


# upload data from different geographies and methods -------------------------------
uploadpath = "population/VA/fairfax/overall_fairfax/"
acs_data <- read_csv(xzfile(paste0(uploadpath, "new_geography_synthetic/housing_units_distribution_method/parcels/data/working/acs_data.csv.xz")))

# shape acs variable to the same format than others data
acs_data <- acs_data %>% mutate(region_type='block group', year=2019)
colnames(acs_data) <- c('geoid','region_name','measure','value','region_type','year')
acs_data <- acs_data %>% select(geoid,region_type,year,region_name,measure,value)
acs_data <- acs_data %>% mutate(synthetic_method=NA)

# method 1: (hud=housing units distribution) ----------------------------------------
zc_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/zip_code/data/distribution/va059_zc_sdad_2019_demographics.csv.xz")))
pd_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/planning_districts/data/distribution/va059_pd_sdad_2019_demographics.csv.xz")))
sd_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/supervisor_districts/data/distribution/va059_sd_sdad_2019_demographics.csv.xz")))
hsr_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/human_service_regions/data/distribution/va059_hsr_sdad_2019_demographics.csv.xz")))


# filter the age -----------------------------------------------------------------
age = c("total_pop","pop_under_20","pop_20_64","pop_65_plus")
acs_data <- acs_data %>% filter(measure %in% age)

# method 1: housing using distribution  ---------------------------------------------------
zc_hud_data <- zc_hud_data %>% filter(measure %in% age)
pd_hud_data <- pd_hud_data %>% filter(measure %in% age)
sd_hud_data <- sd_hud_data %>% filter(measure %in% age)
hsr_hud_data <- hsr_hud_data %>% filter(measure %in% age)
hud_data <- rbind(zc_hud_data,pd_hud_data,sd_hud_data,hsr_hud_data)
hud_data <- hud_data %>% mutate(synthetic_method="housing units distribution")

# method 2: two stages IPF  ---------------------------------------------------


# combine all the data ---------------------
age_data <- rbind(acs_data,hud_data)


# compress and save -----------------------------------------------------------------
savepath = "population/VA/fairfax/age/data/distribution/"
write_csv(age_data, xzfile(paste0(savepath,"va059_sdad_2019_age.csv.xz"), compression = 9))


