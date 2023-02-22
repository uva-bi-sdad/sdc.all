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
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# count the total employment by block groups 
minority_employment <- mi_fairfax_features %>%
  group_by(geoid,year,minority) %>%
  summarize(type=if_else(minority==1,'minority_owned','non_minority_owned'),
            measure=paste0(type,'_total_employment'),
            value=sum(employment, na.rm=T)) %>%
  mutate(region_type='block group',
         census_year=if_else(year<2020,2010,2020),
         measure_type='count',
         MOE='')

# add geometry 
fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2010)
fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2020)
fairfax_bg <- rbind(fairfax_bg2010,fairfax_bg2020)

# merge the data
minority_employment <- merge(minority_employment, fairfax_bg, by.x=c('geoid','census_year'), by.y=c('geoid','census_year')) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Minority_owned/data/distribution/"
readr::write_csv(minority_employment, xzfile(paste0(savepath,"va059_bg_mi_",min(minority_employment$year),max(minority_employment$year),"_total_employment_by_minority.csv.xz"), compression = 9))

