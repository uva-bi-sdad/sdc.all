# Measure the job creation by industry and block group

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
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))


# count the total number of business per block groups and year
temp <- mi_fairfax_features %>%
  select(duns,year,geoid,region_name,region_type,naics_name,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = -c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','region_name','region_type','year','naics_name'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(naics_name,'_',measure),
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)



# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_jobs_creation_by_industry.csv.xz"), compression = 9))





####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  select(duns,year,geoid,region_name,region_type,naics_name,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = -c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,region_name,region_type,year,naics_name) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','region_name','region_type','year','naics_name'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(naics_name,'_',measure),
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_jobs_creation_by_industry.csv.xz"), compression = 9))



# aggregate the data at the tract level and save -----------
temp1 <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  select(duns,year,geoid,naics_name,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = -c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,year,naics_name) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','year','naics_name'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(naics_name,'_',measure),
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp1, xzfile(paste0(savepath,"ncr_tr_mi_",min(temp1$year),max(temp1$year),"_jobs_creation_by_industry.csv.xz"), compression = 9))


# aggregate the data at the county level and save ------------------
temp2 <-  mi_ncr_features %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  select(duns,year,geoid,naics_name,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = -c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,year,naics_name) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','year','naics_name'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(naics_name,'_',measure),
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,MOE)

# save
readr::write_csv(temp2, xzfile(paste0(savepath,"ncr_ct_mi_",min(temp2$year),max(temp2$year),"_jobs_creation_by_industry.csv.xz"), compression = 9))
