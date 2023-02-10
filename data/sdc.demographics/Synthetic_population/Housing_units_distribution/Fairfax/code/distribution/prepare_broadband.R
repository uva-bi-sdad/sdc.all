# Broadband estimation at the smallest geographic area (parcels)


# libraries -------------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(httr)
library(rjson)
library(tidyr)
library(readr)
library(tidycensus)
library(reprex)


# load the broadband data at the block group -----------------------------------------------------
# download the file and read it
temp <- tempfile()
url = 'https://github.com/uva-bi-sdad/sdc.broadband/raw/main/Wired/Accessibility/Average%20Download%20Speed/data/distribution/ncr_hdcttrbg_2019_2021q3_speed_measurements.csv.xz'
download.file(url,temp)
downloadspeed <- read_csv(temp) %>% select(geoid,region_name,region_type,year,measure,value) %>% mutate(measure='avg_down_using_devices')
unlink(temp)

temp <- tempfile()
url = 'https://github.com/uva-bi-sdad/sdc.broadband/raw/main/Wired/Accessibility/Average%20Upload%20Speed/data/distribution/ncr_hdcttrbg_2019_2021q3_speed_measurements.csv.xz'
download.file(url,temp)
uploadspeed <- read_csv(temp) %>% select(geoid,region_name,region_type,year,measure,value)
unlink(temp)

temp <- tempfile()
url = 'https://github.com/uva-bi-sdad/sdc.broadband/raw/main/Wired/Accessibility/Number%20of%20devices/data/distribution/ncr_hdcttrbg_2019_2021q3_speed_measurements.csv.xz'
download.file(url,temp)
devices <- read_csv(temp) %>% select(geoid,region_name,region_type,year,measure,value)
unlink(temp)

broadband <- rbind(downloadspeed,uploadspeed,devices) %>% filter(region_type=='block group') %>% mutate(geoid=as.numeric(geoid))


# load the parcel data
uploadpath = "Synthetic_population/Housing_units_distribution/Fairfax/data/working/"
parcel_livunit <- read.csv(paste0(uploadpath,"va059_sdad_parcel_bg_livingunits.csv.xz"))

# parcel level estimation of broadband  ---------------------------------------------------------------------------------------------------------------
# Assumptions:
#   1. The upload and download speed are uniformly distributed across parcels from the same block group: meaning that parcels from the same block groups received the same upload and download speed
#         In other words, the speed doesn't depends on the number of living units in the area
#   2. The number of devices in a block groups is distributed across parcels proportional to the number of living units. The more livings units is observed in a parcel, 
#         more are the devices used

# compute the multiplier
temp <- parcel_livunit %>%
  mutate(bg_geo=as.numeric(substr(geoid, 1, 12))) %>%
  group_by(bg_geo) %>%
  mutate(bg_unitcnt=sum(liv_unit, na.rm=T),
         mult=liv_unit/bg_unitcnt) %>%
  rename(parid=geoid) %>%
  filter(!is.na(mult)) %>%
  select(parid,bg_geo,mult)

# estimate the broadband at the parcel level
fairfax_parcel_broad <- merge(broadband, temp, by.x='geoid', by.y='bg_geo', all.y=T) %>%
  filter(!is.na(measure)) %>%
  pivot_wider(names_from='measure', values_from='value') %>%
  group_by(parid) %>%
  mutate(avg_down_using_devices=mean(avg_down_using_devices),
         avg_up_using_devices=mean(avg_up_using_devices),
         devices=mult*devices,
         region_name=paste0('Parcel ',parid),
         region_type='parcel') %>%
  pivot_longer(!c('geoid','parid','region_name','region_type','year'), names_to='measure', values_to='value') %>% 
  filter(!(measure=='mult')) %>%
  select(geoid=parid,region_name,region_type, year,measure,value) 


# save the data ------------------------------------------------------------------------------------------------------------
savepath = "Synthetic_population/Housing_units_distribution/Fairfax/data/working/"
readr::write_csv(fairfax_parcel_broad, xzfile(paste0(savepath,"va059_pc_sdad_2021_broadband.csv.xz"), compression = 9))




