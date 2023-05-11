# Count the total employment by minority-owned and block group

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
  group_by(geoid,region_name,region_type,year,minority) %>%
  summarize(type=if_else(minority==1,'minority_owned','non_minority_owned'),
            measure=paste0(type,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Minority_owned/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_total_employment_by_minority.csv.xz"), compression = 9))




####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  group_by(geoid,region_name,region_type,year,minority) %>%
  summarize(type=if_else(minority==1,'minority_owned','non_minority_owned'),
            measure=paste0(type,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)

# save the data
savepath = "Employment/Minority_owned/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_total_employment_by_minority.csv.xz"), compression = 9))



# aggregate the data at the tract level and save -----------
temp1 <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,minority) %>%
  summarize(type=if_else(minority==1,'minority_owned','non_minority_owned'),
            measure=paste0(type,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp1, xzfile(paste0(savepath,"ncr_tr_mi_",min(temp1$year),max(temp1$year),"_total_employment_by_minority.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp2 <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,minority) %>%
  summarize(type=if_else(minority==1,'minority_owned','non_minority_owned'),
            measure=paste0(type,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp2, xzfile(paste0(savepath,"ncr_ct_mi_",min(temp2$year),max(temp2$year),"_total_employment_by_minority.csv.xz"), compression = 9))
