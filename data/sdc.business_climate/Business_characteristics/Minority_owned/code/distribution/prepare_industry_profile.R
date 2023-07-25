# Count the total number of business by block group

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


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# count the total number of business by block groups 
profile <- mi_fairfax_features %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  group_by(year,naics_name,type) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            exit_business=sum(exit),
            entry_rate=100*new_business/total_business,
            exit_rate=100*exit_business/total_business) %>%
  select(year,naics_name,type,total_business,new_business,exit_business,entry_rate,exit_rate) %>%
  pivot_longer(!c('year','naics_name','type'), names_to='measure', values_to='value') %>%
  mutate(region_type='county',
         region_name = 'Fairfax county',
         geoid=51059,
         measure=paste0(type,'_',naics_name,'_',measure),
         measure_type = case_when(
           grepl('rate',measure)==T ~ "percentage",
           grepl('business',measure)==T ~ "count"),
         moe='') %>%
  select(geoid,year,measure,value,measure_type,moe)


# save the data ---------------------------------------------------------------------------------------
savepath = "Business_characteristics/Minority_owned/data/distribution/"
readr::write_csv(profile, xzfile(paste0(savepath,"va059_ct_mi_",min(profile$year),max(profile$year),"_minority_industry_profile.csv.xz"), compression = 9))



