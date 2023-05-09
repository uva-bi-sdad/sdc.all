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

uploadpath = "Microdata/Mergent_intellect/data/working/"
savepath = "Business_characteristics/Industry/data/distribution/"

# upload the data --------------------------------------------------------------------
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))


# count the total number of business per block groups and year
temp <- mi_fairfax_features %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarize(measure='number_business',
            value=length(duns)) %>%
  mutate(measure=paste0(naics_name,'_',measure),
         measure_type='count',
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_number_business_by_industry.csv.xz"), compression = 9))





####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarize(measure='number_business',
            value=length(duns)) %>%
  mutate(measure=paste0(naics_name,'_',measure),
         measure_type='count',
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)

# save the data
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_number_business_by_industry.csv.xz"), compression = 9))
