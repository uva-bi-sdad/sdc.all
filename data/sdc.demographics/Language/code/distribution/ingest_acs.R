# Get the language (limited english) distribution from ACS for different geographies level
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
  total = "B16002_001",
  english_only = "B16002_002",
  spanish = "B16002_003",
  other_indo_european = "B16002_006",
  asian_pacific_insland = "B16002_009",
  other_language = "B16002_012")

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

# 1. Language distribution for VA
acs_data_va <- acs_data_va_wd %>%
  mutate(total=totalE,
         english_only = english_onlyE, spanish = spanishE, other_indo_european = other_indo_europeanE, asian_pacific_insland = asian_pacific_inslandE, other_language = other_languageE,
         perc_english_only = 100*english_only/total,
         perc_spanish = 100*spanish/total,
         perc_other_indo_european = 100*other_indo_european/total,
         perc_asian_pacific_insland = 100*asian_pacific_insland/total,
         perc_other_language = 100*other_language/total) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total,english_only,spanish,other_indo_european,asian_pacific_insland,other_language,perc_english_only,perc_spanish,perc_other_indo_european,perc_asian_pacific_insland,perc_other_language) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
    grepl('pop',measure)==T ~ "count",
    grepl('perc',measure)==T ~ "percentage"),
         MOE='')


#2. Language distribution afor NCR
acs_data_ncr <- acs_data_ncr_wd %>%
  mutate(total=totalE,
         english_only = english_onlyE, spanish = spanishE, other_indo_european = other_indo_europeanE, asian_pacific_insland = asian_pacific_inslandE, other_language = other_languageE,
         perc_english_only = 100*english_only/total,
         perc_spanish = 100*spanish/total,
         perc_other_indo_european = 100*other_indo_european/total,
         perc_asian_pacific_insland = 100*asian_pacific_insland/total,
         perc_other_language = 100*other_language/total) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total,english_only,spanish,other_indo_european,asian_pacific_insland,other_language,perc_english_only,perc_spanish,perc_other_indo_european,perc_asian_pacific_insland,perc_other_language) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
    grepl('pop',measure)==T ~ "count",
    grepl('perc',measure)==T ~ "percentage"),
    MOE='',
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
  select(geoid,region_name,region_type,year,measure,value,MOE)



# Save the data ----------------------------------------------------------------------------------
savepath = "Language/data/distribution/"
readr::write_csv(acs_data_va, xzfile(paste0(savepath,"va_trctbg_acs_20092020_language_demographics.csv.xz"), compression = 9))
readr::write_csv(acs_data_ncr, xzfile(paste0(savepath,"ncr_trctbg_acs_20092020_language_demographics.csv.xz"), compression = 9))




