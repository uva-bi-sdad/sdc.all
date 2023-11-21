# subset census tract to puma crosswalk data for the ncr

library(dplyr)
library(sf)

upload_path <- 'NCR/Census Geographies/PUMAS/2010/data/original/'
national_crosswalk <- read.csv(paste0(upload_path, 'national_2010_tr_to_puma_crosswalk.csv.xz'),
                               colClasses='character')

tr_puma_2010 <- national_crosswalk %>% 
  mutate(year=2010, tract_geoid=paste0(STATEFP, COUNTYFP, TRACTCE), 
         puma_geoid=paste0(STATEFP, PUMA5CE)) %>% 
  select(tract_geoid, puma_geoid, year)

# subset for NCR based on tract
ncr_tr_2010 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Tract/2010/data/distribution/ncr_geo_census_cb_2010_census_tracts.geojson') %>%
  select(geoid,year) %>% st_drop_geometry()
ncr_tr_puma <- merge(tr_puma_2010, ncr_tr_2010, by.x=c('tract_geoid', 'year'), 
                     by.y=c('geoid', 'year'))

path <- 'NCR/Census Geographies/PUMAS/2010/data/distribution/'
readr::write_csv(ncr_tr_puma, xzfile(paste0(path, 'ncr_2010_tr_to_puma_crosswalk.csv.xz'), 
                                     compression=9))
