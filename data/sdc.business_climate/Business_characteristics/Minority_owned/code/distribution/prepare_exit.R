# Count the total number of business at entry by block group

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


uploadpath = "Microdata/Mergent_intellect/data/working/"
savepath = "Business_characteristics/Minority_owned/data/distribution/"

# upload the data --------------------------------------------------------------------
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_fairfax_features %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,region_name,region_type,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,region_name,region_type,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year','type'), names_to='measure', values_to='value') %>%
  mutate(geoid=as.character(geoid),
         measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)


# save the data ---------------------------------------------------------------------------------------
#readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_exit_by_minority.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp_tr <-  mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
#readr::write_csv(temp1, xzfile(paste0(savepath,"va059_tr_mi_",min(temp1$year),max(temp1$year),"_exit_by_minority.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp_ct <-  mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))
readr::write_csv(temp, xzfile(paste0(savepath,"va059_cttrbg_mi_",min(temp$year),'_',max(temp$year),"_exit_by_minority.csv.xz"), compression = 9))





####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_ncr_features %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,region_name,region_type,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,region_name,region_type,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year','type'), names_to='measure', values_to='value') %>%
  mutate(geoid=as.character(geoid),
         measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save the data
savepath = "Business_characteristics/Minority_owned/data/distribution/"
#readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_exit_by_minority.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp_tr <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
#readr::write_csv(temp1, xzfile(paste0(savepath,"ncr_tr_mi_",min(temp1$year),max(temp1$year),"_exit_by_minority.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp_ct <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_cttrbg_mi_",min(temp$year),'_',max(temp$year),"_exit_by_minority.csv.xz"), compression = 9))







####  upload data for Richmond city, Henrico county and Chesterfield county ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_subva_features %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,region_name,region_type,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,region_name,region_type,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year','type'), names_to='measure', values_to='value') %>%
  mutate(geoid=as.character(geoid),
         measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)


# save the data
savepath = "Business_characteristics/Minority_owned/data/distribution/"
#readr::write_csv(temp, xzfile(paste0(savepath,"va_bg_mi_",min(temp$year),max(temp$year),"_exit_by_minority.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp_tr <-  mi_subva_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
#readr::write_csv(temp1, xzfile(paste0(savepath,"va_tr_mi_",min(temp1$year),max(temp1$year),"_exit_by_minority.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp_ct <-  mi_subva_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(geoid,year,type) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit, na.rm=T),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,year,type,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))
readr::write_csv(temp, xzfile(paste0(savepath,"rva_cttrbg_mi_",min(temp$year),'_',max(temp$year),"_exit_by_minority.csv.xz"), compression = 9))

