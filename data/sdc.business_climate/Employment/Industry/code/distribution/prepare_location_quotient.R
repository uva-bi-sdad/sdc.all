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
temp_bg <- mi_fairfax_features %>%
  mutate(geoid=as.character(geoid)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))


temp_tr <- mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))


temp_ct <- mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))

temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))

# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),'_',max(temp$year),"_location_quotient_by_industry.csv.xz"), compression = 9))






####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_ncr_features %>%
  mutate(geoid=as.character(geoid)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))


temp_tr <- mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))


temp_ct <- mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))

temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))

# save the data
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),'_',max(temp$year),"_location_quotient_by_industry.csv.xz"), compression = 9))






####  upload data for Richmond city, Henrico county and Chesterfield county ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_subva_features %>%
  mutate(geoid=as.character(geoid)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))


temp_tr <- mi_subva_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))


temp_ct <- mi_subva_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  select(duns,year,geoid,naics_name,employment) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment, na.rm=T)) %>%
  group_by(year,naics_name) %>%
  mutate(emp_ind_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid) %>%
  mutate(emp_bg_year=sum(employment, na.rm=T)) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(emp_bg_year=mean(emp_bg_year),
            emp_ind_year=mean(emp_ind_year),
            emp_year=mean(emp_year),
            emp_bg_ind_year=sum(employment, na.rm=T),
            value=round((emp_bg_ind_year/emp_bg_year)/(emp_ind_year/emp_year),2)) %>%
  mutate(measure=paste0(naics_name,'_Location_quotient'),
         measure_type = 'index',
         moe='')%>%
  select(geoid,year,measure,value,measure_type,moe) %>%
  ungroup() %>%
  filter(!is.na(value))

# save the data
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"rva_bg_mi_",min(temp$year),'_',max(temp$year),"_location_quotient_by_industry.csv.xz"), compression = 9))
