# Race distribution for new geographies aera (only for sub-area in virginia) using the living units distribution across parcels (method 1)
# New geography : - Zip code
#                 - Human services
#                 - Civic association
#                 - Supervisor districts


# librairies ------------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(httr)
library(sp)
library(data.table)
library(stringr)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(tidyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(rjson)


# 1. Case of fairfax county:  --------------------------------------------------------------------
years <- 2013:2019

#upload data from fairfax (demography and geometry). filter on race
fairfax_pc_dmg <- NULL
for (year in years) {
  temp0 <- read_csv(xzfile(paste0("Synthetic_population/Housing_units_distribution/Fairfax/data/working/va059_pc_sdad_",year,"_demographics.csv.xz")))
  temp <- temp0 %>% select(geoid,year,measure,value) %>% filter(measure %in% c('pop_wht_alone','pop_afr_amer_alone','pop_native_alone','pop_AAPI','pop_other','pop_two_or_more','pop_hispanic_or_latino','total_race','pop_eth_tot'))
  fairfax_pc_dmg <- rbind(fairfax_pc_dmg,temp)
}

fairfax_pc_geo <- sf::st_read(unzip("Synthetic_population/Housing_units_distribution/Fairfax/data/working/fairfax_parcel_geometry.geojson.zip", "Synthetic_population/Housing_units_distribution/Fairfax/data/working/fairfax_parcel_geometry.geojson"))
file.remove("Synthetic_population/Housing_units_distribution/Fairfax/data/working/fairfax_parcel_geometry.geojson")
fairfax_pc_geo <- fairfax_pc_geo %>% select(parid=geoid, geometry)

# upload new geographies and mapping with parcels (comments: just add a new geography below and the intersects with parcels)
sf::sf_use_s2(FALSE)
hsr_geo <- sf::st_read("https://github.com/uva-bi-sdad/sdc.geographies/blob/7723c7ad25b92a7ae7edb88c17b3c561b521a82b/VA/Local%20Geographies/Fairfax%20County/Human%20Services%20Regions/2022/data/distribution/va059_geo_ffxct_gis_2022_human_services_regions.geojson?raw=T")
pd_geo <- sf::st_read("https://github.com/uva-bi-sdad/sdc.geographies/blob/7723c7ad25b92a7ae7edb88c17b3c561b521a82b/VA/Local%20Geographies/Fairfax%20County/Planning%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_planning_districts.geojson?raw=T")
sd_geo <- sf::st_read("https://github.com/uva-bi-sdad/sdc.geographies/blob/7723c7ad25b92a7ae7edb88c17b3c561b521a82b/VA/Local%20Geographies/Fairfax%20County/Supervisor%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_supervisor_districts.geojson?raw=T")
zc_geo <- sf::st_read("https://github.com/uva-bi-sdad/sdc.geographies/blob/7723c7ad25b92a7ae7edb88c17b3c561b521a82b/VA/Local%20Geographies/Fairfax%20County/Zip%20Codes/2022/data/distribution/va059_geo_ffxct_gis_2022_zip_codes.geojson?raw=T")

# coments: for some cases the number of rows can be lower than the number of parcels meaning that the new geography doesn't cover all the parcels
hsr_pc_map <- st_join(hsr_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(-year)
pd_pc_map <- st_join(pd_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(-year)
sd_pc_map <- st_join(sd_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(-year)
zc_pc_map <- st_join(zc_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(-year)

# estimate the demographics for the new geography
hsr_dmg <- merge(hsr_pc_map, fairfax_pc_dmg, by.x='parid', by.y='geoid', all.y=T) %>%
  group_by(geoid,region_name,region_type,year,measure) %>%
  summarise(value=sum(value, na.rm=T))

pd_dmg <- merge(pd_pc_map, fairfax_pc_dmg, by.x='parid', by.y='geoid', all.y=T) %>%
  group_by(geoid,region_name,region_type,year,measure) %>%
  summarise(value=sum(value, na.rm=T))

sd_dmg <- merge(sd_pc_map, fairfax_pc_dmg, by.x='parid', by.y='geoid', all.y=T) %>%
  group_by(geoid,region_name,region_type,year,measure) %>%
  summarise(value=sum(value, na.rm=T))

zc_dmg <- merge(zc_pc_map, fairfax_pc_dmg, by.x='parid', by.y='geoid', all.y=T) %>%
  mutate(geoid=as.character(geoid)) %>%
  group_by(geoid,region_name,region_type,year,measure) %>%
  summarise(value=sum(value, na.rm=T))

fairfax_newgeo_dmg <- rbind(hsr_dmg,pd_dmg,sd_dmg,zc_dmg) %>%
  pivot_wider(names_from='measure', values_from='value') %>%
  filter(!is.na(geoid)) %>%
  mutate(perc_wht_alone = 100*pop_wht_alone/total_race,
         perc_afr_amer_alone = 100*pop_afr_amer_alone/total_race,
         perc_native_alone = 100*pop_native_alone/total_race,
         perc_AAPI = 100*pop_AAPI/total_race,
         perc_two_or_more = 100*pop_two_or_more/total_race,
         perc_other = 100*pop_other/total_race,
         perc_hispanic_or_latino = 100*pop_hispanic_or_latino/pop_eth_tot) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type=case_when(
    grepl('perc',measure)==T ~ "percentage",
    grepl('pop',measure)==T ~ "count",
    grepl('race',measure)==T ~ "count"),
    MOE='')


# save the data ----------------------------------------------------------------------------------
savepath = "Race/data/distribution/"
readr::write_csv(fairfax_newgeo_dmg, xzfile(paste0(savepath,"va059_hsrpdsdzc_sdad_",min(years),max(years),"_race_demographics.csv.xz"), compression = 9))




# 2. Case of arlington county --------------------------------------------------------------------
arl_pc_dmg <- NULL
years <- 2013:2020

# upload data from arlington (demography and geometry). filter on gender
for (year in years) {
  temp0 <- read_csv(xzfile(paste0("Synthetic_population/Housing_units_distribution/Arlington/data/working/va013_pc_sdad_",year,"_demographics.csv.xz")))
  temp <- temp0 %>% select(geoid,year,measure,value) %>% filter(measure %in% c('pop_wht_alone','pop_afr_amer_alone','pop_native_alone','pop_AAPI','pop_other','pop_two_or_more','pop_hispanic_or_latino','total_race','pop_eth_tot'))
  arl_pc_dmg <- rbind(arl_pc_dmg,temp)
}

arl_pc_geo <- sf::st_read(unzip("Synthetic_population/Housing_units_distribution/Arlington/data/working/arl_parcel_geometry.geojson.zip", "Synthetic_population/Housing_units_distribution/Arlington/data/working/arl_parcel_geometry.geojson"))
file.remove("Synthetic_population/Housing_units_distribution/Arlington/data/working/arl_parcel_geometry.geojson")
arl_pc_geo <- arl_pc_geo %>% select(parid=geoid, geometry)

# upload new geographies and mapping with parcels (comments: just add a new geography below and the intersects with parcels)
sf::sf_use_s2(FALSE)
civic_geo <- sf::st_read("https://github.com/uva-bi-sdad/sdc.geographies/blob/7723c7ad25b92a7ae7edb88c17b3c561b521a82b/VA/Local%20Geographies/Arlington%20County/Civic%20Associations/2021/data/distribution/va013_geo_arl_2021_civic_associations.geojson?raw=T")

# comments: for some cases the number of rows can be lower than the number of parcels meaning that the new geography doesn't cover all the parcels
civic_pc_map <- st_join(civic_geo, arl_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(-year)

# estimate the demographics for the new geography. all the 
civic_dmg <- merge(civic_pc_map, arl_pc_dmg, by.x='parid', by.y='geoid', all.y=T) %>%
  group_by(geoid,region_name,region_type,year,measure) %>%
  summarise(value=sum(value, na.rm=T))

arl_newgeo_dmg <- civic_dmg %>%
  pivot_wider(names_from='measure', values_from='value') %>%
  filter(!is.na(geoid)) %>%
  mutate(perc_wht_alone = 100*pop_wht_alone/total_race,
         perc_afr_amer_alone = 100*pop_afr_amer_alone/total_race,
         perc_native_alone = 100*pop_native_alone/total_race,
         perc_AAPI = 100*pop_AAPI/total_race,
         perc_two_or_more = 100*pop_two_or_more/total_race,
         perc_other = 100*pop_other/total_race,
         perc_hispanic_or_latino = 100*pop_hispanic_or_latino/pop_eth_tot) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type=case_when(
    grepl('perc',measure)==T ~ "percentage",
    grepl('pop',measure)==T ~ "count",
    grepl('race',measure)==T ~ "count"),
    MOE='')


# save the data ----------------------------------------------------------------------------------
savepath = "Race/data/distribution/"
readr::write_csv(arl_newgeo_dmg, xzfile(paste0(savepath,"va013_civic_sdad_",min(years),max(years),"_race_demographics.csv.xz"), compression = 9))




# 2. Case of DC area -----------------------------------------------------------------------------




