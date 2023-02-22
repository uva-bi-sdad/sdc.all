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
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# count the total number of business by block groups 
entry_business <- mi_fairfax_features %>%
  group_by(geoid,year,naics_name) %>%
  summarize(total_business=length(duns),
            new_business=sum(entry),
            entry_rate=100*new_business/total_business) %>%
  select(geoid,year,naics_name,new_business,entry_rate) %>%
  pivot_longer(!c('geoid','year','naics_name'), names_to='measure', values_to='value') %>%
  mutate(region_type='block group',
         measure=paste0(naics_name,'_',measure),
         census_year=if_else(year<2020,2010,2020),
         measure_type = case_when(
           grepl('entry_rate',measure)==T ~ "percentage",
           grepl('new_business',measure)==T ~ "count"),
         MOE='')

# add geometry 
fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2010)
fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2020)
fairfax_bg <- rbind(fairfax_bg2010,fairfax_bg2020)

# merge the data
entry_business <- merge(entry_business, fairfax_bg, by.x=c('geoid','census_year'), by.y=c('geoid','census_year')) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Business_characteristics/Industry/data/distribution/"
readr::write_csv(entry_business, xzfile(paste0(savepath,"va059_bg_mi_",min(entry_business$year),max(entry_business$year),"_entry_by_industry.csv.xz"), compression = 9))

