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
savepath = "Business_characteristics/Total/data/distribution/"

# upload the data --------------------------------------------------------------------
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_fairfax_features %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,region_name,region_type,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_entry.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp1 <-  mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp1, xzfile(paste0(savepath,"va059_tr_mi_",min(temp1$year),max(temp1$year),"_entry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp2 <-  mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp2, xzfile(paste0(savepath,"va059_ct_mi_",min(temp2$year),max(temp2$year),"_entry.csv.xz"), compression = 9))





####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,region_name,region_type,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
           grepl('entry_rate',measure)==T ~ "percentage",
           grepl('new_business',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data
savepath = "Business_characteristics/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_entry.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp1 <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp1, xzfile(paste0(savepath,"ncr_tr_mi_",min(temp1$year),max(temp1$year),"_entry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp2 <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp2, xzfile(paste0(savepath,"ncr_ct_mi_",min(temp2$year),max(temp2$year),"_entry.csv.xz"), compression = 9))



####  upload data for Richmond city, Henrico county and Chesterfield county ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_subva_features %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,region_name,region_type,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data
savepath = "Business_characteristics/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va_bg_mi_",min(temp$year),max(temp$year),"_entry.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp1 <-  mi_subva_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp1, xzfile(paste0(savepath,"va_tr_mi_",min(temp1$year),max(temp1$year),"_entry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp2 <-  mi_subva_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('entry_rate',measure)==T ~ "percentage",
    grepl('new_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp2, xzfile(paste0(savepath,"va_ct_mi_",min(temp2$year),max(temp2$year),"_entry.csv.xz"), compression = 9))
