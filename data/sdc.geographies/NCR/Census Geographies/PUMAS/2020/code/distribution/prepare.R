# subset census tract to puma crosswalk data for the ncr

library(dplyr)
library(sf)

upload_path <- 'NCR/Census Geographies/PUMAS/2020/data/original/'
national_crosswalk <- read.csv(paste0(upload_path, 'national_2020_tr_to_puma_crosswalk.csv.xz'),
                               colClasses='character')

tr_puma_2020 <- national_crosswalk %>% 
  mutate(year=2020, tract_geoid=paste0(STATEFP, COUNTYFP, TRACTCE), 
         puma_geoid=paste0(STATEFP, PUMA5CE)) %>% 
  select(tract_geoid, puma_geoid, year)

# subset for NCR based on tract
ncr_tr_2020 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Tract/2020/data/distribution/ncr_geo_census_cb_2020_census_tracts.geojson') %>%
  select(geoid,year) %>% st_drop_geometry()
ncr_tr_puma <- merge(tr_puma_2020, ncr_tr_2020, by.x=c('tract_geoid', 'year'), 
                     by.y=c('geoid', 'year'))

path <- 'NCR/Census Geographies/PUMAS/2020/data/distribution/'
readr::write_csv(ncr_tr_puma, xzfile(paste0(path, 'ncr_2020_tr_to_puma_crosswalk.csv.xz'), 
                                     compression=9))
