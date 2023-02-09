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




# load data -------------------------------------------------------------------------
# load parcel living units information
uploadpath = "Synthetic_population/Housing_units_distribution/DC/data/working/"
ssl_livunit <- read.csv(paste0(uploadpath,"dc011_sdad_ssl_bg_livingunits.csv.xz"))

# load ACS data (demographics infos on age, gender and race). filter on block group and fairfax county
dc_acs <- read.csv(paste0(uploadpath,"dc011_bg_acs_20092020_demographics.csv.xz"))
dc_acs2010 <- dc_acs %>% filter(year==2019)



# compute the demographic multiplier -------------------------------------------------
temp <- ssl_livunit %>%
  mutate(bg_geo=as.numeric(substr(geoid, 1, 12))) %>%
  group_by(bg_geo) %>%
  mutate(bg_unitcnt=sum(liv_unit, na.rm=T),
         mult=liv_unit/bg_unitcnt) %>%
  rename(parid=geoid) %>%
  select(parid,bg_geo,mult)



# compute the demographics at the parcels --------------------------------------------
# comment remove block groups without ssl
dc_ssl_dmg <- merge(dc_acs2010, temp, by.x='geoid', by.y='bg_geo', all.x=T)  %>%
  filter(!is.na(parid)) %>%   
  mutate(value=mult*value,
         region_name=paste0('Suffixe Square Lot ',parid),
         region_type='SSL') %>%
  select(geoid=parid,region_name,region_type,measure,value) 

# save the data (demographics at the ssl level)
savepath = "Synthetic_population/Housing_units_distribution/DC/data/working/"
readr::write_csv(dc_ssl_dmg, xzfile(paste0(savepath,"dc011_ssl_sdad_2019_demographics.csv.xz"), compression = 9))





