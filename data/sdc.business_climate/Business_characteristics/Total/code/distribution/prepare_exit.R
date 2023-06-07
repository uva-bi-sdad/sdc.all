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


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_fairfax_features %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,region_name,region_type,year,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('exit_rate',measure)==T ~ "percentage",
    grepl('exit_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)

# save the data ---------------------------------------------------------------------------------------
savepath = "Business_characteristics/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_exit.csv.xz"), compression = 9))





####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,region_name,region_type,year,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
           grepl('exit_rate',measure)==T ~ "percentage",
           grepl('exit_business',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data
savepath = "Business_characteristics/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_exit.csv.xz"), compression = 9))




####  upload data for Richmond city, Henrico county and Chesterfield county ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_subva_features %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarize(total_business=length(duns),
            exit_business=sum(exit),
            exit_rate=100*exit_business/total_business) %>%
  select(geoid,region_name,region_type,year,exit_business,exit_rate) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
    grepl('exit_rate',measure)==T ~ "percentage",
    grepl('exit_business',measure)==T ~ "count"),
    MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data
savepath = "Business_characteristics/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va_bg_mi_",min(temp$year),max(temp$year),"_exit.csv.xz"), compression = 9))
