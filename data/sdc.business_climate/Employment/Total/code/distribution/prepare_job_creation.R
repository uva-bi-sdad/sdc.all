# Count the job creation by geography

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
  select(duns,year,geoid,region_name,region_type,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = -c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(region_type='block group',
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_mi_",min(temp$year),max(temp$year),"_jobs_creation.csv.xz"), compression = 9))



####  upload data for ncr ####  ------------------------------------------------------------------------------------------------------------------

# load the data
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))

# count the total number of business per block groups and year
temp <- mi_ncr_features %>%
  select(duns,year,geoid,region_name,region_type,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = -c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,region_name,region_type,year) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','region_name','region_type','year'), names_to='measure', values_to='value') %>%
  mutate(region_type='block group',
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='') %>%
  ungroup() %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)

# save the data
savepath = "Employment/Total/data/distribution/"
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_mi_",min(temp$year),max(temp$year),"_jobs_creation.csv.xz"), compression = 9))

