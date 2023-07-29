# Count the total employment by idustry and block group

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
temp_bg <- mi_fairfax_features %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(geoid=as.character(geoid),
         measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Industry/data/distribution/"
#readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_total_employment_by_industry.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp_tr <-  mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
#readr::write_csv(temp1, xzfile(paste0(savepath,"va059_tr_mi_",min(temp1$year),max(temp1$year),"_total_employment_by_industry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp_ct <-  mi_fairfax_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))
readr::write_csv(temp, xzfile(paste0(savepath,"va059_cttrbg_mi_",min(temp$year),'_',max(temp$year),"_total_employment_by_industry.csv.xz"), compression = 9))





####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_ncr_features %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(geoid=as.character(geoid),
         measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save the data
savepath = "Employment/Industry/data/distribution/"
#readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_total_employment_by_industry.csv.xz"), compression = 9))



# aggregate the data at the tract level and save -----------
temp_tr <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
#readr::write_csv(temp1, xzfile(paste0(savepath,"ncr_tr_mi_",min(temp1$year),max(temp1$year),"_total_employment_by_industry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp_ct <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_cttrbg_mi_",min(temp$year),'_',max(temp$year),"_total_employment_by_industry.csv.xz"), compression = 9))





####  upload data for Richmond city, Henrico county and Chesterfield county ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp_bg <- mi_subva_features %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(geoid=as.character(geoid),
         measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save the data
savepath = "Employment/Industry/data/distribution/"
#readr::write_csv(temp, xzfile(paste0(savepath,"va_bg_mi_",min(temp$year),max(temp$year),"_total_employment_by_industry.csv.xz"), compression = 9))


# aggregate the data at the tract level and save -----------
temp_tr <-  mi_subva_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
#readr::write_csv(temp, xzfile(paste0(savepath,"va_tr_mi_",min(temp1$year),max(temp1$year),"_total_employment_by_industry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp_ct <-  mi_subva_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,naics_name) %>%
  summarize(measure=paste0(naics_name,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(measure_type='count',
         moe='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save
temp <- rbind(temp_bg, temp_tr, temp_ct) %>%
  filter(!is.na(value))
readr::write_csv(temp, xzfile(paste0(savepath,"rva_cttrbg_mi_",min(temp$year),'_',max(temp$year),"_total_employment_by_industry.csv.xz"), compression = 9))
