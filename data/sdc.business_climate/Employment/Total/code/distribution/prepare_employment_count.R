# Count the total employment by geographies

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
fairfax_employment <- mi_fairfax_features %>%
  group_by(geoid,year) %>%
  summarize(measure='total_employment',
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
fairfax_employment <- merge(fairfax_employment, fairfax_bg, by.x=c('geoid','census_year'), by.y=c('geoid','census_year')) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Total/data/distribution/"
readr::write_csv(fairfax_employment, xzfile(paste0(savepath,"va059_bg_mi_",min(fairfax_employment$year),max(fairfax_employment$year),"_total_employment.csv.xz"), compression = 9))

