# get geometries for pumas in NCR ----------------------------------------------

library(sf)
library(dplyr)

pumas_2020 <- read_sf('NCR/Census Geographies/PUMAS/2020/data/original/dc_md_va_geo_2020_pumas.geojson')

ncr_puma_geoids <- read.csv('NCR/Census Geographies/PUMAS/2020/data/distribution/ncr_2020_tr_to_puma_crosswalk.csv.xz',
                            colClasses='character') %>%
  select(puma_geoid) %>% distinct(puma_geoid)

# subset for ncr puma geometries
ncr_pumas <- right_join(pumas_2020, ncr_puma_geoids, by=c('GEOID10'='puma_geoid')) %>%
  select(puma_geoid=GEOID10, geometry)

sf::st_write(ncr_pumas, "NCR/Census Geographies/PUMAS/2020/data/distribution/ncr_geo_2020_census_pumas.geojson",
             delete_dsn=TRUE)
