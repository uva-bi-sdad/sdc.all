# This file use the refine demographics data at the parcel level to estimate demographics in new geographies
# New geography : - Zip code
#                 - Human services
#                 - Civic association
#                 - Supervisor districts


# libraries ---------------------------------------------------------------------------------------
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



# 1. Case study of Fairfax county  --------------------------------------------------------------------------------------------------------------
path = "Synthetic_population/Housing_units_distribution/Fairfax/data/working/parcels_demographics/"

#upload data from fairfax (demographies and geometry). filter on age
fairfax_pc_dmg <- NULL
for (file in list.files(path)) {
  temp0 <- read_csv(xzfile(paste0(path,file)))
  temp0 <- temp0 %>% filter(str_detect(measure,'pop_male|pop_female|total_pop'))
  fairfax_pc_dmg <- rbind(fairfax_pc_dmg,temp0)
}

# reshape the data
fairfax_pc_dmg_lg <- fairfax_pc_dmg %>%
  mutate(measure=str_remove_all(measure, paste('_refined_by_housing_units', collapse = "|"))) %>%
  select(parid=geoid,year,measure,value) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = measure, values_from=value) 




# get the parcel geometry. --------------------------------------------------------------------------
yearlist <- sort(unique(fairfax_pc_dmg_lg$year))
hsr_dmg <- NULL
pd_dmg <- NULL
sd_dmg <- NULL
zc_dmg <- NULL
hd_dmg <- NULL

for (vyear in yearlist){
  # subset the acs data
  subset_dmg <- fairfax_pc_dmg_lg %>% filter(year==vyear) %>% select(-year)
  
  # we use the parcel distribution of 2022 for all acs demographics from 2013-2022.
  if (vyear<=2022){
    parcel_geo <- readRDS("Synthetic_population/Housing_units_distribution/Fairfax/data/working/parcels_infos/2022/fairfax_parcel_geometry.rds")
    fairfax_pc_geo <- parcel_geo %>% select(parid=geoid, geometry)
  }else{
    parcel_geo <- st_read(paste0("Synthetic_population/Housing_units_distribution/Fairfax/data/working/parcels_infos/",vyear,"/fairfax_parcel_geometry.geojson"))
    fairfax_pc_geo <- parcel_geo %>% select(parid=geoid, geometry)
  }
  
  fairfax_pc_geo <- st_as_sf(fairfax_pc_geo)
  st_crs(fairfax_pc_geo) <- 4326
  #fairfax_pc_geo <- st_transform(fairfax_pc_geo,4326) 
  
  # add geometry to the acs
  #subset <- merge(subset_dmg, fairfax_pc_geo, by='parid')
  
  # upload new geographies geometry
  sf::sf_use_s2(FALSE)
  hsr_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Human%20Services%20Regions/2022/data/distribution/va059_geo_ffxct_gis_2022_human_services_regions.geojson")
  pd_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Planning%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_planning_districts.geojson")
  sd_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Supervisor%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_supervisor_districts.geojson")
  zc_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Fairfax%20County/Zip%20Codes/2022/data/distribution/va059_geo_ffxct_gis_2022_zip_codes.geojson")
  hd_geo <- sf::st_read('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_geo_vhd_2020_health_districts.geojson')
  
  # build the map between the parcel and new geoid
  hsr_pc_map <- st_join(hsr_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(parid,geoid)
  pd_pc_map <- st_join(pd_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(parid,geoid)
  sd_pc_map <- st_join(sd_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(parid,geoid)
  zc_pc_map <- st_join(zc_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(parid,geoid)
  hd_pc_map <- st_join(hd_geo, fairfax_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(parid,geoid)
  
  # aggregate demographics using redistribute ---------------------------------------------------
  temp_hsr_dmg <- merge(subset_dmg, hsr_pc_map, by='parid') %>% select(-parid) %>% group_by(geoid) %>% summarise(across(everything(), sum)) %>% mutate(year=vyear)
  temp_pd_dmg <- merge(subset_dmg, pd_pc_map, by='parid') %>% select(-parid) %>% group_by(geoid) %>% summarise(across(everything(), sum)) %>% mutate(year=vyear)
  temp_sd_dmg <- merge(subset_dmg, sd_pc_map, by='parid') %>% select(-parid) %>% group_by(geoid) %>% summarise(across(everything(), sum)) %>% mutate(year=vyear)
  temp_zc_dmg <- merge(subset_dmg, zc_pc_map, by='parid') %>% select(-parid) %>% group_by(geoid) %>% summarise(across(everything(), sum)) %>% mutate(year=vyear)
  temp_hd_dmg <- merge(subset_dmg, hd_pc_map, by='parid') %>% select(-parid) %>% group_by(geoid) %>% summarise(across(everything(), sum)) %>% mutate(year=vyear)
  
  # combine data over year
  hsr_dmg <- rbind(hsr_dmg,temp_hsr_dmg)
  pd_dmg <- rbind(pd_dmg,temp_pd_dmg)
  sd_dmg <- rbind(sd_dmg,temp_sd_dmg)
  zc_dmg <- rbind(zc_dmg,temp_zc_dmg)
  hd_dmg <- rbind(hd_dmg,temp_hd_dmg)
}


#fairfax_pc_geo <- sf::st_read(unzip("Synthetic_population/Housing_units_distribution/Fairfax/data/working/fairfax_parcel_geometry.geojson.zip", "Synthetic_population/Housing_units_distribution/Fairfax/data/working/fairfax_parcel_geometry.geojson"))
#file.remove("Synthetic_population/Housing_units_distribution/Fairfax/data/working/fairfax_parcel_geometry.geojson")
#fairfax_pc_sf <- st_as_sf(merge(fairfax_pc_dmg_lg,fairfax_pc_geo, by='geoid'))

# combine data from different geographies into one
model_parcels <- rbind(hsr_dmg,pd_dmg,sd_dmg,zc_dmg) %>%
  filter(!is.na(geoid)) %>%
  mutate(perc_male = 100*pop_male/total_pop,
         perc_female = 100*pop_female/total_pop) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(
    #measure=paste0('gender_',measure,'_parcels'),
    moe='')


# get the acs data ----------------------------------------------
uploadpath = "Gender/data/working/"
files = list.files(uploadpath)
filename = files[str_detect(files,"va_cttrbg_acs")]
temp_acs_dmg <- readRDS(paste0(uploadpath,filename)) %>% select(geoid,year,measure,value,moe) 

temp_parcels_dmg <- model_parcels 
fx_newgeo_dmg <- rbind(temp_acs_dmg,temp_parcels_dmg) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))







# 2. Case of arlington county --------------------------------------------------------------------
path = "Synthetic_population/Housing_units_distribution/Arlington/data/working/parcels_demographics/"

#upload data for arlington (demographies and geometry). filter on age
arl_pc_dmg <- NULL
for (file in list.files(path)) {
  temp0 <- read_csv(xzfile(paste0(path,file)))
  temp0 <- temp0 %>% filter(str_detect(measure,'pop_male|pop_female|total_pop'))
  arl_pc_dmg <- rbind(arl_pc_dmg,temp0)
}


arl_pc_geo <- sf::st_read("Synthetic_population/Housing_units_distribution/Arlington/data/working/va_arl_parcel_geometry.geojson")
arl_pc_geo <- arl_pc_geo %>% select(parid=geoid, geometry)
arl_pc_dmg <- arl_pc_dmg %>% select(geoid,year,measure,value) %>% filter(measure %in% c('pop_male','pop_female','total_pop'))

# upload new geographies and mapping with parcels (comments: just add a new geography below and the intersects with parcels)
sf::sf_use_s2(FALSE)
civic_geo <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Arlington%20County/Civic%20Associations/2021/data/distribution/va013_geo_arl_2021_civic_associations.geojson")

# comments: for some cases the number of rows can be lower than the number of parcels meaning that the new geography doesn't cover all the parcels
civic_pc_map <- st_join(civic_geo, arl_pc_geo, join = st_intersects) %>% st_drop_geometry() %>% select(-year)

# estimate the demographics for the new geography. all the
civic_dmg <- merge(civic_pc_map, arl_pc_dmg, by.x='parid', by.y='geoid', all.y=T) %>%
  group_by(geoid,region_name,region_type,year,measure) %>%
  summarise(value=sum(value, na.rm=T))

arl_newgeo_dmg <- civic_dmg %>%
  pivot_wider(names_from='measure', values_from='value') %>%
  filter(!is.na(geoid)) %>%
  mutate(perc_male = 100*pop_male/total_pop,
         perc_female = 100*pop_female/total_pop) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type=case_when(
           grepl('pop',measure)==T ~ "count",
           grepl('perc',measure)==T ~ "percentage"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,moe)


# combine the data ------------------------------------
baseline_data <- rbind(fx_newgeo_dmg,arl_newgeo_dmg)
baseline_data <- baseline_data %>% 
  mutate(measure=case_when(
    measure=="total_pop" ~ "gender_total_count_parcels",
    measure=="pop_male" ~ "gender_male_count_parcels",
    measure=="pop_female" ~ "gender_female_count_parcels",
    measure=="perc_male" ~ "gender_male_percent_parcels",
    measure=="perc_female" ~ "gender_female_percent_parcels"))


# save the data 
savepath = "Gender/data/working/model/"
readr::write_csv(baseline_data, xzfile(paste0(savepath,"va_civichsrsdpdzccttrbg_sdad_",min(yearlist),'_',max(yearlist),"_gender_demographics_parcels.csv.xz"), compression = 9))


# files = list.files(savepath)
# filename = files[str_detect(files,"va059_hsrsdpdzc_sdad")]
# 
# if (length(filename)==0){
#   # create the file
#   readr::write_csv(model_parcels, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_gender_demographics.csv.xz"), compression = 9))
#   
# }else{
#   # there is only one file perform check to whether replace the file or update the content
#   file_maxyear = as.numeric(substr(filename,27,30))
#   if(max(yearlist)!=file_maxyear){
#     # update filename
#     file.remove(filename)
#     readr::write_csv(model_parcels, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_gender_demographics.csv.xz"), compression = 9))
#     
#   }else{
#     #read the file, identify the model in those file 
#     lastfile <- read.csv(paste0(savepath,filename))
#     lastfile$model <- sapply(strsplit(lastfile$measure, split= "_", fixed = TRUE), tail, 1L)
#     modellist <- unique(lastfile$model)
#     if ('parcels' %in% modellist){
#       # get the set of the other models listed in the data. keep other models and update the current model data
#       set <- lastfile %>% filter(model!='parcels') %>% select(geoid,year,measure,value,moe)
#       lastfile <- rbind(set, model_parcels)
#       readr::write_csv(lastfile, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_gender_demographics.csv.xz"), compression = 9))
#     }else{
#       # just add the current model data
#       set <- lastfile %>% select(geoid,year,measure,value,moe)
#       lastfile <- rbind(set, model_parcels)
#       readr::write_csv(lastfile, xzfile(paste0(savepath,"va059_hsrsdpdzc_sdad_",min(yearlist),'_',max(yearlist),"_gender_demographics.csv.xz"), compression = 9))
#     }
#   }
# }






# 
# 
# 
# 
# 
# # 2. Case of DC area -----------------------------------------------------------------------------
#  
# 
