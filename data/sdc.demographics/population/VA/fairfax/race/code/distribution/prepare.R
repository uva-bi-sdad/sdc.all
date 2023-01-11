# Age distribution at the geographics level of ACS.

library(readr)
library(dplyr)


# upload data from different geographies and methods -------------------------------
uploadpath = "population/VA/fairfax/overall_fairfax/"
acs_data <- read_csv(xzfile(paste0(uploadpath, "new_geography_synthetic/housing_units_distribution_method/parcels/data/working/acs_data.csv.xz")))

# shape acs variable to the same format than others data
acs_data <- acs_data %>% mutate(region_type='block group', year=2019, measure_type='count', MOE='') %>% select(geoid=bg_geoid,region_type,region_name=name,year,measure=variable,measure_type,MOE,value=estimate) %>%
  mutate(measure=recode(measure, "wht_alone"="pop_white",  "afr_amer_alone"="pop_black", "asian_alone"="pop_AAPI","hispanic"="pop_hispanic_or_latino"))


# method 1: (hud=housing units distribution) ----------------------------------------
zc_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/zip_code/data/distribution/va059_zc_sdad_2019_demographics.csv.xz")))
pd_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/planning_districts/data/distribution/va059_pd_sdad_2019_demographics.csv.xz")))
sd_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/supervisor_districts/data/distribution/va059_sd_sdad_2019_demographics.csv.xz")))
hsr_hud_data <- read_csv(xzfile(paste0(uploadpath,"new_geography_synthetic/housing_units_distribution_method/human_service_regions/data/distribution/va059_hsr_sdad_2019_demographics.csv.xz")))


# filter the age -----------------------------------------------------------------
race = c("total_pop","pop_white","pop_black","pop_AAPI","pop_hispanic_or_latino")
acs_data <- acs_data %>% filter(measure %in% race)

# method 1: housing using distribution  ---------------------------------------------------
zc_hud_data <- zc_hud_data %>% filter(measure %in% race)
pd_hud_data <- pd_hud_data %>% filter(measure %in% race)
sd_hud_data <- sd_hud_data %>% filter(measure %in% race)
hsr_hud_data <- hsr_hud_data %>% filter(measure %in% race)
hud_data <- rbind(zc_hud_data,pd_hud_data,sd_hud_data,hsr_hud_data)

# method 2: two stages IPF  ---------------------------------------------------


# combine all the data ---------------------
race_data <- rbind(acs_data,hud_data)


# compress and save -----------------------------------------------------------------
savepath = "population/VA/fairfax/race/data/distribution/"
write_csv(gender_data, xzfile(paste0(savepath,"va059_bgzcpdsdhsr_sdad_2019_race.csv.xz"), compression = 9))


