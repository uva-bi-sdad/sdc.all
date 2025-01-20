# Get data on geography mobility across VA
# geography: tracts, counties, block group (extended to health districts)

# Libraries -------------------------------------------------------------------------------------
library(dplyr)
library(sf)
# library(httr)
#library(rjson)
library(tidyr)
library(readr)
library(tidycensus)
#library(geojsonio)
census_api_key(Sys.getenv('census_api_key'))


# Upload the data --------------------------------------------------------------------------------

# list of variables
named_acs_var_list <- c(
  total_pop = "B07204_001",
  pop_moving = "B07204_003")

# list of geography level
geographies <- c('tract','county','block group')

# list of years
years <- 2015:2023

# Download ACS data for VA
acs_data_va_wd <- NULL
for (geo in geographies){
  for (year in years){
    temp <- data.table::setDT(
      tidycensus::get_acs(
        state = "VA",
        survey = "acs5",
        year = year,
        geography = geo,
        output = "wide",
        variables = named_acs_var_list
      )
    )
    
    temp$year <- year
    temp$region_type <- geo
    acs_data_va_wd <- rbind(acs_data_va_wd, temp)
  }
}

# bring the data at the health district
hd_shp <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_geo_vhd_2020_health_districts.geojson")
crosswalk <- read_csv('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv') %>%
  select(ct_geoid,hd_geoid)

counties <- unique(crosswalk$ct_geoid)
acs_va_counties_wd <- acs_data_va_wd %>% filter(GEOID %in% counties) %>% mutate(geoid=as.numeric(GEOID))

# each county belong to only one hd. Therefore we can perform the sum over hd_geoid 
# run this for checking test <- crosswalk %>% group_by(ct_geoid) %>% mutate(count=length(hd_geoid))
acs_va_hd <- merge(acs_va_counties_wd, crosswalk, by.x='geoid', by.y='ct_geoid') %>%
  group_by(hd_geoid,year) %>%
  mutate(pop_movingE=sum(pop_movingE),
         total_popE=sum(total_popE),
         value=100*pop_movingE/total_popE,
         measure='perc_moving') %>%
  dplyr::select(geoid=hd_geoid,year,measure,value) %>%
  mutate(moe='') %>%
  filter(!is.na(value)) %>%
  mutate(geoid=format(geoid, scientific = FALSE, justify='none'))
  

# transform and store the data
acs_data_va <- acs_data_va_wd %>%
  mutate(value=100*pop_movingE/total_popE,
         measure='perc_moving') %>%
  dplyr::select(geoid=GEOID,year,measure,value) %>%
  mutate(moe='') %>%
  filter(!is.na(value)) %>%
  mutate(geoid=format(geoid, scientific = FALSE, justify='none'))

# combine data
acs_data <- rbind(acs_data_va,acs_va_hd)

# save the data
#savepath = "Geography_mobility/data/distribution/"
#readr::write_csv(acs_data, xzfile(paste0(savepath,"va_cttrbg_acs_",min(years),"_",max(years),"_moving_demographics.csv.xz"), compression = 9))

# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## convert
stnd <- standardize_all(acs_data)
write.csv(stnd, xzfile("Geographic Mobility (HOI)/data/distribution/va_hdcttrbg_2015_2023_moving_demographics.csv.xz"), row.names = F)
