# Get the language (limited english) distribution from ACS for different geographies level
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
  total_hh = "C16002_001",
  limited_english_spanish = "C16002_004",
  limited_english_indo_europe = "C16002_007",
  limited_english_asian_pacific = "C16002_010",
  limited_english_other_language = "C16002_013")

# list of geography level
geographies <- c('tract','county','block group')

# list of states
states <- c('VA','MD','DC')

# periods (this table start in 2016)
years <- 2016:2021

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
  mutate(total_hh=total_hhE,
         hh_limited_english = limited_english_spanishE + limited_english_indo_europeE + limited_english_asian_pacificE + limited_english_other_languageE,
         perc_hh_limited_english = 100*(hh_limited_english)/total_hhE,
         hh_limited_english_MOE = sqrt(sum(limited_english_spanishM^2, limited_english_indo_europeM^2, limited_english_asian_pacificM^2, limited_english_other_languageM^2, na.rm=T)) )%>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,hh_limited_english,perc_hh_limited_english) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('hh',measure)==T ~ "count"),
         moe='') %>%
  mutate(geoid=format(geoid, scientific = FALSE, justify='none'))


#2. Language distribution afor NCR
acs_data_ncr <- acs_data_ncr_wd %>%
  mutate(total_hh=total_hhE,
         hh_limited_english = limited_english_spanishE + limited_english_indo_europeE + limited_english_asian_pacificE + limited_english_other_languageE,
         perc_hh_limited_english = 100*(hh_limited_english)/total_hhE,
         hh_limited_english_MOE = sqrt(sum(limited_english_spanishM^2, limited_english_indo_europeM^2, limited_english_asian_pacificM^2, limited_english_other_languageM^2, na.rm=T)) )%>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,hh_limited_english,perc_hh_limited_english) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
           grepl('perc',measure)==T ~ "percentage",
           grepl('hh',measure)==T ~ "count"),
         moe='',
         census_year=if_else(year<2020,2010,2020)) %>%
  mutate(geoid=format(geoid, scientific = FALSE, justify='none'))



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
  rename(census_year=year) %>%
  mutate(geoid=format(geoid, scientific = FALSE, justify='none'))

acs_data_ncr <- merge(acs_data_ncr, ncr_geo, by.x=c('geoid','region_type','census_year'), by.y=c('geoid','region_type','census_year'), all.y=T) %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,moe)

acs_data_ncr <- acs_data_ncr %>%
  mutate(measure=case_when(
    measure=="hh_limited_english" ~ "language_hh_limited_english_count_direct",
    measure=="perc_hh_limited_english" ~ "language_hh_limited_english_percent_direct")) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))

acs_data_ncr_parcels <- acs_data_ncr %>%
  mutate(measure=str_replace(measure,'direct','parcels'))

acs_data_ncr <- rbind(acs_data_ncr,acs_data_ncr_parcels)

# Save the data ----------------------------------------------------------------------------------
savepath = "Language/data/working/"
saveRDS(acs_data_va, paste0(savepath,"va_cttrbg_acs_",min(years),'_',max(years),"_language_demographics.csv.xz"), compress = 'xz')

savepath = "Language/data/distribution/"
readr::write_csv(acs_data_ncr, xzfile(paste0(savepath,"ncr_cttrbg_acs_",min(years),'_',max(years),"_language_demographics.csv.xz"), compression = 9))




