# Broadband estimation at the smallest geographic area (parcels)


# libraries ----------------------------------------------------
library(dplyr)
library(sf)
library(httr)
library(rjson)
library(tidyr)
library(readr)
library(tidycensus)

# load the broadband data at the block group -------------------
downloadspeed <- read_csv('https://github.com/uva-bi-sdad/sdc.broadband/blob/ceeee700b1ae2ef64f8993b8fbb27af3ff21aba7/Wired/Accessibility/Average%20Download%20Speed/data/distribution/ncr_hdcttrbg_2019_2021q3_speed_measurements.csv.xz?raw=T') 
uploadspeed <- read_csv('https://github.com/uva-bi-sdad/sdc.broadband/blob/ceeee700b1ae2ef64f8993b8fbb27af3ff21aba7/Wired/Accessibility/Average%20Download%20Speed/data/distribution/ncr_hdcttrbg_2019_2021q3_speed_measurements.csv.xz?raw=T') 
devices <- read_csv('https://github.com/uva-bi-sdad/sdc.broadband/blob/ceeee700b1ae2ef64f8993b8fbb27af3ff21aba7/Wired/Accessibility/Average%20Download%20Speed/data/distribution/ncr_hdcttrbg_2019_2021q3_speed_measurements.csv.xz?raw=T') 
broadband <- rbind(downloadspeed,uploadspeed,devices)

# load the parcel data
uploadpath = "Synthetic_population/Housing_units_distribution/Fairfax/data/working/"
parcel_livunit <- read.csv(paste0(uploadpath,"va059_sdad_parcel_bg_livingunits.csv.xz"))


# Assumptions:
#   1. The upload and download speed are uniformly distributed across parcels: meaning that parcels from the same block groups received the same upload and download speed
#         In other words, the speed doesn't depends on the number of living units in the area
#   2. The number of devices in a block groups is distributed across parcels proportional to the number of living units. The more livings units is observed in a parcel, 
#         more are the devices used

# compute the multiplier
temp <- parcel_livunit %>%
  mutate(bg_geo=as.numeric(substr(geoid, 1, 12))) %>%
  group_by(bg_geo) %>%
  mutate(bg_unitcnt=sum(liv_unit),
         mult=liv_unit/bg_unitcnt) %>%
  rename(parid=geoid) %>%
  select(parid,bg_geo,mult)

# estimate the broadband at the parcel level
fairfax_parcel_bbnd <- merge(broadband, temp, by.x='geoid', by.y='bg_geo', all.y=T) %>%
  mutate(value=mult*value,
         region_name=paste0('parcel ',parid),
         region_type='parcel') %>%
  select(geoid=parid,region_name,region_type,measure,value)

# compress and save the data 
path = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"
readr::write_csv(acs_data, xzfile(paste0(path,"acs_data.csv.xz"), compression = 9))
