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
years <- 2017:2021

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

# transform and store the data
acs_data_va <- acs_data_va_wd %>%
  mutate(value=100*pop_movingE/total_popE,
         measure='perc_moving') %>%
  dplyr::select(geoid=GEOID,year,measure,value) %>%
  mutate(moe='') %>%
  filter(!is.na(value)) %>%
  mutate(geoid=format(geoid, scientific = FALSE, justify='none'))



savepath = "Geography_mobility/data/working/"
readr::write_csv(acs_data_va, xzfile(paste0(savepath,"va_cttrbg_acs_",min(years),"_",max(years),"_moving_demographics.csv.xz"), compression = 9))


