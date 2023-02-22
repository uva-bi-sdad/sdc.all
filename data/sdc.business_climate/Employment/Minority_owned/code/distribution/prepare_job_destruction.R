# Measure the job destruction by industry and block group

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
job_destruction <- mi_fairfax_features %>%
  mutate(type=if_else(minority==1,'minority_owned','non_minority_owned')) %>%
  select(duns,year,geoid,type,exit,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((exit==1)|(employment_diff<0)) %>%
  group_by(geoid,year,type) %>%
  summarise(job_destruction_exit=sum(exit*employment, na.rm=T),
            job_destruction_active=-sum((1-exit)*employment_diff, na.rm=T),
            business_job_destruction=length(duns),
            total_job_destruction=job_destruction_exit+job_destruction_active,
            perc_job_destruction_exit=100*job_destruction_exit/total_job_destruction,
            perc_job_destruction_active=100*job_destruction_active/total_job_destruction) %>%
  pivot_longer(!c('geoid','year','type'), names_to='measure', values_to='value') %>%
  mutate(measure=paste0(type,'_',measure),
         region_type='block group',
         measure_type = case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('job',measure)==T ~ "count"),
         MOE='',
         census_year=if_else(year<2020,2010,2020))


# add geometry 
fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2010)
fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2020)
fairfax_bg <- rbind(fairfax_bg2010,fairfax_bg2020)

# merge the data
job_destruction <- merge(job_destruction, fairfax_bg, by.x=c('geoid','census_year'), by.y=c('geoid','census_year')) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)



# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Minority_owned/data/distribution/"
readr::write_csv(job_destruction, xzfile(paste0(savepath,"va059_bg_mi_",min(job_destruction$year),max(job_destruction$year),"_jobs_destruction_by_minority.csv.xz"), compression = 9))

