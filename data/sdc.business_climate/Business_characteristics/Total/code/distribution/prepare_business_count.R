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


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# count the total number of business by block groups 
fairfax_business <- mi_fairfax_features %>%
  group_by(geoid,year) %>%
  summarize(measure='number_business',
            value=length(duns)) %>%
  mutate(region_type='block group',
         census_year=if_else(year<2020,2010,2020),
         measure_type='count',
         MOE='')

# add geometry 
fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2010)
fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% select(geoid=GEOID,region_name=NAMELSAD) %>% st_drop_geometry() %>% mutate(census_year=2020)
fairfax_bg <- rbind(fairfax_bg2010,fairfax_bg2020)

# merge the data
fairfax_business <- merge(fairfax_business, fairfax_bg, by.x=c('geoid','census_year'), by.y=c('geoid','census_year')) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Business_characteristics/Total/data/distribution/"
readr::write_csv(fairfax_business, xzfile(paste0(savepath,"va059_bg_mi_",min(fairfax_business$year),max(fairfax_business$year),"_number_business.csv.xz"), compression = 9))

