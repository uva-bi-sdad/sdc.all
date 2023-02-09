# Get the race distribution from ACS for different geographies level
# Geographic level:   - Tracts
#                     - Counties
#                     - Block groups

# Libraries -------------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(httr)
library(rjson)
library(tidyr)
library(readr)
library(tidycensus)
census_api_key(Sys.getenv('census_api_key'))


# Upload the data --------------------------------------------------------------------------------

# list of variables
named_acs_var_list <- c(
  total_pop = "B01001_001",
  wht_alone = "B02001_002",
  afr_amer_alone = "B02001_003",
  amr_ind_alone = "B02001_004",
  asian_alone = "B02001_005",
  hispanic = "B03002_012")

# list of geography level
geographies <- c('tract','county','block group')

# list of states
states <- c('VA','MD','DC')

# list of years
years <- 2009:2020

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

# 1. Race distribution for VA
acs_data_va <- acs_data_va_wd %>%
  mutate(total_pop=total_popE,
         wht_alone = wht_aloneE, afr_amer_alone = afr_amer_aloneE, amr_ind_alone = amr_ind_aloneE, asian_alone = asian_aloneE, hispanic = hispanicE,
         perc_wht_alone = 100*wht_alone/total_pop,
         perc_afr_amer_alone = 100*afr_amer_alone/total_pop,
         perc_amr_ind_alone = 100*amr_ind_alone/total_pop,
         perc_asian_alone = 100*asian_alone/total_pop,
         perc_hispanic = 100*hispanic/total_pop) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total_pop,wht_alone,afr_amer_alone,amr_ind_alone,asian_alone,hispanic,perc_wht_alone,perc_afr_amer_alone,perc_amr_ind_alone,perc_asian_alone,perc_hispanic) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
    grepl('pop',measure)==T ~ "count",
    grepl('perc',measure)==T ~ "percentage"),
         MOE='')


#2. Age distribution afor NCR
acs_data_ncr <- acs_data_ncr_wd %>%
  mutate(total_pop=total_popE,
         wht_alone = wht_aloneE, afr_amer_alone = afr_amer_aloneE, amr_ind_alone = amr_ind_aloneE, asian_alone = asian_aloneE, hispanic = hispanicE,
         perc_wht_alone = 100*wht_alone/total_pop,
         perc_afr_amer_alone = 100*afr_amer_alone/total_pop,
         perc_amr_ind_alone = 100*amr_ind_alone/total_pop,
         perc_asian_alone = 100*asian_alone/total_pop,
         perc_hispanic = 100*hispanic/total_pop) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total_pop,wht_alone,afr_amer_alone,amr_ind_alone,asian_alone,hispanic,perc_wht_alone,perc_afr_amer_alone,perc_amr_ind_alone,perc_asian_alone,perc_hispanic) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
    grepl('pop',measure)==T ~ "count",
    grepl('perc',measure)==T ~ "percentage"),
    MOE='')



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
ncr_geo <- rbind(temp_bg2010,temp_bg2020,temp_ct2010,temp_ct2020,temp_tr2010,temp_tr2020)

acs_data_ncr <- merge(acs_data_ncr, ncr_geo, by.x=c('geoid','region_type','year'), by.y=c('geoid','region_type','year'), all.y=T)



# Save the data ----------------------------------------------------------------------------------
savepath = "Race/data/distribution/"
readr::write_csv(acs_data_va, xzfile(paste0(savepath,"va_trctbg_acs_20092020_race_demographics.csv.xz"), compression = 9))
readr::write_csv(acs_data_ncr, xzfile(paste0(savepath,"ncr_trctbg_acs_20092020_race_demographics.csv.xz"), compression = 9))




