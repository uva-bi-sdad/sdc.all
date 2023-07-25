# Compute the contribution of each block group to an industry activity


# library -----------------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(reshape2)
library(crosstable)
library(tidyr)
library(scales)
library(plm)


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# Identify incumbent and new businesses. Count the number of new employees from new business, incumbent.
location_quotient <- mi_fairfax_features %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  mutate(emp_bg_ind_year=sum(employment, na.rm=T),
         value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2),
         measure=paste0(naics_name,'_Location_quotient'),
         region_type = 'block group',
         measure_type = 'index',
         moe='',
         census_year=if_else(year<2020,2010,2020)) %>%
  select(geoid,region_type,year,measure,value,measure_type,moe,census_year)


# add geometry 
fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2010)
fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2020)
fairfax_bg <- rbind(fairfax_bg2010,fairfax_bg2020)

# merge the data
temp <- merge(location_quotient, fairfax_bg, by.x=c('geoid','census_year'), by.y=c('geoid','census_year')) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,moe)



# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(location_quotient, xzfile(paste0(savepath,"va059_bg_mi_",min(location_quotient$year),'_',max(location_quotient$year),"_location_quotient_by_industry.csv.xz"), compression = 9))






####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  select(duns,year,geoid,region_name,region_type,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,region_name,region_type) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,region_name,region_type,naics_name) %>%
  mutate(emp_bg_ind_year=sum(employment, na.rm=T),
         value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2),
         measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='') %>% ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,moe)


# save the data
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),'_',max(temp$year),"_location_quotient_by_industry.csv.xz"), compression = 9))






####  upload data for Richmond city, Henrico county and Chesterfield county ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_subva_features %>%
  select(duns,year,geoid,region_name,region_type,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,region_name,region_type) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,region_name,region_type,naics_name) %>%
  mutate(emp_bg_ind_year=sum(employment, na.rm=T),
         value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2),
         measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='') %>% ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,moe)


# save the data
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"rva_bg_mi_",min(temp$year),'_',max(temp$year),"_location_quotient_by_industry.csv.xz"), compression = 9))
