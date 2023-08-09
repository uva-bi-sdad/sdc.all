# this file estimate demographics to new geographies by directly expand the census geographies (without refining): use the package redistribute
# lets go from block group to zipcode 

# libraries -------------------------------------------------------------
library(dplyr)
library(sf)
# library(httr)
library(sp)
library(data.table)
library(stringr)
#library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(tidyr)
library(readr)
# library(tidyverse)
library(tidycensus)
library(tigris)
library(rjson)
library(redistribute)


# load the data -------------------------------------------------------------------
# get the age demographics acs data for virginia
uploadpath = "Race/data/working/"
files = list.files(uploadpath)
filename = files[str_detect(files,"va_cttrbg_acs")]
acs <- readRDS(paste0(uploadpath,filename))

# prepare the data for modeling -------------------------------------------
# select the census block group for aggregation, select only population count as measure
acs_bg <- acs %>%
  filter(region_type=='tract') %>%
#  filter(!str_detect(measure, "perc")) %>%
  select(geoid,year,measure,value) %>%
#  mutate(measure=str_replace(measure,'race_','')) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(pop_eth_tot= 100*pop_hispanic_or_latino/perc_hispanic_or_latino,
         census_year=if_else(year<2020,2010,2020), geoid=as.numeric(geoid))

# get census 2010 and 2020
# bg_geo_2010 <- sf::read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Block%20Group/2010/data/distribution/va_geo_census_cb_2010_census_block_groups.geojson')
# bg_geo_2020 <- sf::read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Block%20Group/2020/data/distribution/va_geo_census_cb_2020_census_block_groups.geojson')
# bg_geo <- rbind(bg_geo_2010,bg_geo_2020) %>%
#   select(geoid,year,geometry) %>%
#   mutate(census_year=as.numeric(year))

bg_geo_2010 <- sf::read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Tract/2010/data/distribution/va_geo_census_cb_2010_census_tracts.geojson')
bg_geo_2020 <- sf::read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Tract/2020/data/distribution/va_geo_census_cb_2020_census_tracts.geojson')
bg_geo <- rbind(bg_geo_2010,bg_geo_2020) %>% select(geoid,year,geometry) %>% 
  mutate(census_year=as.numeric(year),
         geoid=as.numeric(geoid)) %>%
  select(-year)

temp_acs <- merge(acs_bg,bg_geo, by=c('geoid','census_year')) 
temp_acs_sf <- st_as_sf(temp_acs)

# load new geographies geometries
hsr_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Human%20Services%20Regions/2022/data/distribution/va059_geo_ffxct_gis_2022_human_services_regions.geojson")
pd_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Planning%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_planning_districts.geojson")
sd_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Supervisor%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_supervisor_districts.geojson")
zc_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Zip%20Codes/2022/data/distribution/va059_geo_ffxct_gis_2022_zip_codes.geojson")
civic_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Arlington%20County/Civic%20Associations/2021/data/distribution/va013_geo_arl_2021_civic_associations.geojson")
hd_geo <- sf::st_read('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_geo_vhd_2020_health_districts.geojson')
pd_geo <- sf::st_make_valid(pd_geo)

# get label and geometry separetly
temp_hsr <- hsr_geo %>% select(geoid_hsr=geoid,geometry)
temp_pd <- pd_geo %>% select(geoid_pd=geoid,geometry)
temp_sd <- sd_geo %>% select(geoid_sd=geoid,geometry)
temp_zc <- zc_geo %>% select(geoid_zc=geoid,geometry)
temp_civic <- civic_geo %>% select(geoid_ca=geoid,geometry)
temp_hd <- hd_geo %>% select(geoid_hd=geoid,geometry)

# redistribute the population ---------------------------------------------------
yearlist <- unique(temp_acs_sf$year)
model <- NULL
for (vyear in yearlist){
  subtemp_acs_sf <- temp_acs_sf %>% filter(year==vyear) %>% select(-c('census_year','year'))
  predict_hsr <- redistribute(subtemp_acs_sf, temp_hsr, source_id = "geoid", target_id = 'geoid_hsr')
  predict_pd <- redistribute(subtemp_acs_sf, temp_pd, source_id = "geoid", target_id = 'geoid_pd')
  predict_sd <- redistribute(subtemp_acs_sf, temp_sd, source_id = "geoid", target_id = 'geoid_sd')
  predict_zc <- redistribute(subtemp_acs_sf, temp_zc, source_id = "geoid", target_id = 'geoid_zc')
  predict_civic <- redistribute(subtemp_acs_sf, temp_civic, source_id = "geoid", target_id = 'geoid_ca')
  predict_hd <- redistribute(subtemp_acs_sf, temp_hd, source_id = "geoid", target_id = 'geoid_hd')
  
  # add the year as variable
  predict_hsr <- predict_hsr %>% mutate(year=vyear) %>% st_drop_geometry()
  predict_pd <- predict_pd %>% mutate(year=vyear) %>% st_drop_geometry()
  predict_sd <- predict_sd %>% mutate(year=vyear) %>% st_drop_geometry()
  predict_zc <- predict_zc %>% mutate(year=vyear) %>% st_drop_geometry()
  predict_civic <- predict_civic %>% mutate(year=vyear) %>% st_drop_geometry()
  predict_hd <- predict_hd %>% mutate(year=vyear) %>% st_drop_geometry()
  
  # combine aggregation over year
  predict <- rbind(predict_hsr,
                   predict_pd,
                   predict_sd,
                   predict_zc,
                   predict_civic,
                   predict_hd)
  model <- rbind(model,predict)
}

# estimate the percentage  --------------------------------------------------
model_direct <- model %>%
  mutate(perc_wht_alone = 100*pop_wht_alone/total_race,
         perc_afr_amer_alone = 100*pop_afr_amer_alone/total_race,
         perc_native_alone = 100*pop_native_alone/total_race,
         perc_AAPI = 100*pop_AAPI/total_race,
         perc_two_or_more = 100*pop_two_or_more/total_race,
         perc_other = 100*pop_other/total_race,
         perc_hispanic_or_latino = 100*pop_hispanic_or_latino/pop_eth_tot) %>%
  pivot_longer(!c('id','year'), names_to = "measure", values_to = "value") %>%
  filter(!(measure=='pop_eth_tot')) %>%
  mutate(moe='') %>%
  select(geoid=id,year,measure,value,moe)
  

# combine the data
temp_acs_dmg <- acs %>% 
  select(geoid,year,measure,value,moe) 
temp_direct_dmg <- model_direct 
baseline_data <- rbind(temp_acs_dmg,temp_direct_dmg)

# baseline_data <- read_csv('Race/data/working/model/va_hsrsdpdzccttrbg_sdad_2013_2021_race_demographics1.csv.xz')
# unique(baseline_data$measure)
# yearlist <- unique(baseline_data$year)

baseline_data <- baseline_data %>% 
  mutate(measure=case_when(
    measure=="total_race" ~ "race_total_count_direct",
    measure=="pop_wht_alone" ~ "race_wht_alone_count_direct",
    measure=="pop_afr_amer_alone" ~ "race_afr_amer_alone_count_direct",
    measure=="pop_native_alone" ~ "race_native_alone_count_direct",
    measure=="pop_AAPI" ~ "race_AAPI_count_direct",
    measure=="pop_other" ~ "race_other_count_direct", 
    measure=="pop_two_or_more" ~ "race_two_or_more_count_direct",
    measure=="pop_hispanic_or_latino" ~ "race_hispanic_or_latino_count_direct",
    measure=="perc_wht_alone" ~ "race_wht_alone_percent_direct",
    measure=="perc_afr_amer_alone" ~ "race_afr_amer_alone_percent_direct",
    measure=="perc_native_alone" ~ "race_native_alone_percent_direct",
    measure=="perc_AAPI" ~ "race_AAPI_percent_direct",
    measure=="perc_two_or_more" ~ "race_two_or_more_percent_direct",
    measure=="perc_other" ~ "race_other_percent_direct",
    measure=="perc_hispanic_or_latino" ~ "race_hispanic_or_latino_percent_direct"))%>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))




# save the living units distribution ----------------------------------------------------------------------------
savepath = "Race/data/working/model/"
readr::write_csv(baseline_data, xzfile(paste0(savepath,"va_hsrsdpdzccttrbg_sdad_",min(yearlist),'_',max(yearlist),"_race_demographics_direct.csv.xz"), compression = 9))




# files = list.files(savepath)
# filename = files[str_detect(files,"va059_hsrsdpdzc_sdad")]
# 
# if (length(filename)==0){
#   # create the file
#   readr::write_csv(model_direct, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_race_demographics.csv.xz"), compression = 9))
#   
# }else{
#   # there is only one file perform check to whether replace the file or update the content
#   file_maxyear = as.numeric(substr(filename,27,30))
#   if(max(yearlist)!=file_maxyear){
#     # update filename
#     file.remove(filename)
#     readr::write_csv(model_direct, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_race_demographics.csv.xz"), compression = 9))
#     
#   }else{
#     #read the file and check if the measure content the model name
#     lastfile <- read.csv(paste0(savepath,filename))
#     lastfile <- rbind(lastfile, model_direct)
#     readr::write_csv(lastfile, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_race_demographics.csv.xz"), compression = 9))
#   }
# }
