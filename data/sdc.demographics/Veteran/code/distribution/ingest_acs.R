# Get the Veteran status distribution from ACS for different geographies level
# Geographic level:   - Tracts
#                     - Counties
#                     - Block groups

# Libraries -------------------------------------------------------------------------------------
library(dplyr)
library(sf)
# library(httr)
library(rjson)
library(tidyr)
library(readr)
library(tidycensus)
census_api_key(Sys.getenv('census_api_key'))


# Upload the data --------------------------------------------------------------------------------

# list of variables
named_acs_var_list <- c(
  vet_denom = "B21001_001", 
  num_vet = "B21001_002")

# list of geography level
geographies <- c('county','tract','block group')

# list of states
states <- c('VA','MD','DC')

# periods (this table start in 2016)
years <- 2009:2021

# Download the data from ACS for VA and NCR
# 1. Virginia (all census geographies) . (comments: block groups information are available after 2012)
acs_data_va_wd <- NULL
for (geo in geographies){
  for (year in years){
    if ((geo=='block group') && (year<2013)){
      temp <- NULL
    }else{
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
}


# 2. NCR (all census geographies)
acs_data_ncr_wd <- NULL
for (state in states){
  for (geo in geographies){
    for (year in years){
      if ((geo=='block group') && (year<2013)){
        temp <- NULL
      }else{
        temp <- data.table::setDT(
          tidycensus::get_acs(
            state = state,
            survey = "acs5",
            year = year,
            geography = geo,
            output = "wide",
            variables = named_acs_var_list
          )
        )
        
        temp$year <- year
        temp$region_type <- geo
        acs_data_ncr_wd <- rbind(acs_data_ncr_wd, temp)
      }
    }
  }
}



# Compute statistics -----------------------------------------------------------------------------

# 1. Veteran distribution for VA
acs_data_va <- acs_data_va_wd %>%
  mutate(pop_veteran = num_vetE, perc_veteran = 100*num_vetE/vet_denomE)%>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,pop_veteran,perc_veteran) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(
    #measure=paste0('veteran_',measure),
         measure_type=case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('pop',measure)==T ~ "count"),
         moe='',)


#2. Veteran distribution for NCR
acs_data_ncr <- acs_data_ncr_wd %>%
  mutate(pop_veteran = num_vetE, perc_veteran = 100*num_vetE/vet_denomE)%>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,pop_veteran,perc_veteran) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(
    #measure=paste0('veteran_',measure),
         measure_type=case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('pop',measure)==T ~ "count"),
         moe='',
        census_year=if_else(year<2020,2010,2020))



# get the list of tracts, counties and block groups from NCR
temp_bg2010 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Block%20Group/2010/data/distribution/ncr_geo_census_cb_2010_census_block_groups.geojson') %>%
  select(geoid,region_type,year) %>% st_drop_geometry()
temp_bg2020 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Block%20Group/2020/data/distribution/ncr_geo_census_cb_2020_census_block_groups.geojson') %>%
  select(geoid,region_type,year) %>% st_drop_geometry()
temp_ct2010 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/County/2010/data/distribution/ncr_geo_census_cb_2010_counties.geojson') %>%
  select(geoid,region_type,year) %>% st_drop_geometry()
temp_ct2020 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/County/2020/data/distribution/ncr_geo_census_cb_2020_counties.geojson') %>%
  select(geoid,region_type,year) %>% st_drop_geometry()
temp_tr2010 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Tract/2010/data/distribution/ncr_geo_census_cb_2010_census_tracts.geojson') %>%
  select(geoid,region_type,year) %>% st_drop_geometry()
temp_tr2020 <- read_sf('https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Tract/2020/data/distribution/ncr_geo_census_cb_2020_census_tracts.geojson') %>%
  select(geoid,region_type,year) %>% st_drop_geometry()
ncr_geo <- rbind(temp_bg2010,temp_bg2020,temp_ct2010,temp_ct2020,temp_tr2010,temp_tr2020) %>%
  rename(census_year=year)

acs_data_ncr <- merge(acs_data_ncr, ncr_geo, by.x=c('geoid','region_type','census_year'), by.y=c('geoid','region_type','census_year'), all.y=T) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,moe)



# Save the data ----------------------------------------------------------------------------------
savepath = "Veteran/data/working/"
readr::write_csv(acs_data_va, xzfile(paste0(savepath,"va_cttrbg_acs_",min(years),'_',max(years),"_veteran_demographics.csv.xz"), compression = 9))

savepath<-"Veteran/data/distribution"
readr::write_csv(acs_data_ncr, xzfile(paste0(savepath,"ncr_cttrbg_acs_",min(years),'_',max(years),"_veteran_demographics.csv.xz"), compression = 9))




