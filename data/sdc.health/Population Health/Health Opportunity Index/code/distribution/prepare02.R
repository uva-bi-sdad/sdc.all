library(tidyverse)
library(geojsonsf)
library(sf)

counties <- geojson_sf("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/County/2010/data/distribution/va_geo_census_cb_2010_counties.geojson") %>% st_drop_geometry() %>% select(-year)

# Manually aggregate data 

files <- list.files("Population Health/Health Opportunity Index/data/distribution/")
files <- files[-(1)]

for (i in 1:length(files)) {
  tract <- read_csv(paste0("Population Health/Health Opportunity Index/data/distribution/", files[i])) 
  county <- tracts %>% mutate(geoid = substr(geoid, 1, 5)) %>% select(-region_name, -region_type) %>% 
    group_by(geoid) %>% mutate(value = median(value)) %>% distinct() %>% left_join(counties, by = "geoid")
  both <- tract %>% rbind(county)
  write_csv(both, paste0("Population Health/Health Opportunity Index/data/distribution/va_cttr_", 
                         substr(files[i], 7, nchar(files[i]))))
  tract <- NULL
  county <- NULL
  both <- NULL
}
