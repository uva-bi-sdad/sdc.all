# Get the age distribution from ACS for different geographies level
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
library(geojsonio)
census_api_key(Sys.getenv('census_api_key'))


# Upload the data --------------------------------------------------------------------------------

# list of variables
named_acs_var_list <- c(
  total_pop = "B01001_001",
  male_under_5 = "B01001_003", male_5_9 = "B01001_004",
  male_10_14 = "B01001_005", male_15_17 = "B01001_006",
  male_18_19 = "B01001_007", male_20 = "B01001_008",
  male_21 = "B01001_009", male_22_24 = "B01001_010",
  male_25_29 = "B01001_011", male_30_34 = "B01001_012",
  male_35_39 = "B01001_013", male_40_44 = "B01001_014",
  male_45_49 = "B01001_015", male_50_54 = "B01001_016",
  male_55_59 = "B01001_017", male_60_61 = "B01001_018",
  male_62_64 = "B01001_019", male_65_66 = "B01001_020",
  male_67_69 = "B01001_021", male_70_74 = "B01001_022",
  male_75_79 = "B01001_023", male_80_84 = "B01001_024",
  male_over_85 = "B01001_025",
  female_under_5 = "B01001_027", female_5_9 = "B01001_028",
  female_10_14 = "B01001_029", female_15_17 = "B01001_030",
  female_18_19 = "B01001_031", female_20 = "B01001_032",
  female_21 = "B01001_033", female_22_24 = "B01001_034",
  female_25_29 = "B01001_035", female_30_34 = "B01001_036",
  female_35_39 = "B01001_037", female_40_44 = "B01001_038",
  female_45_49 = "B01001_039", female_50_54 = "B01001_040",
  female_55_59 = "B01001_041", female_60_61 = "B01001_042",
  female_62_64 = "B01001_043", female_65_66 = "B01001_044",
  female_67_69 = "B01001_045", female_70_74 = "B01001_046",
  female_75_79 = "B01001_047", female_80_84 = "B01001_048",
  female_over_85 = "B01001_049")

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

# 1. Age distribution for VA
acs_data_va <- acs_data_va_wd %>%
  mutate(total_pop=total_popE,
         pop_under_20 = male_under_5E + male_5_9E + male_10_14E + male_15_17E + male_18_19E + 
           female_under_5E + female_5_9E + female_10_14E + female_15_17E + female_18_19E,
         pop_20_64 = male_20E + male_21E + male_22_24E + male_25_29E + male_30_34E + male_35_39E +
           male_40_44E +  male_45_49E + male_50_54E + male_55_59E + male_60_61E + male_62_64E + 
           female_20E + female_21E + female_22_24E + female_25_29E + female_30_34E + female_35_39E +
           female_40_44E +  female_45_49E + female_50_54E + female_55_59E + female_60_61E + female_62_64E,
         pop_65_plus = male_65_66E + male_67_69E + male_70_74E + male_75_79E + male_80_84E + male_over_85E +
           female_65_66E + female_67_69E + female_70_74E + female_75_79E + female_80_84E + male_over_85E,
         perc_pop_under_20 = 100*pop_under_20/total_pop,
         perc_pop_20_64 = 100*pop_20_64/total_pop,
         perc_pop_65_plus = 100*pop_65_plus/total_pop) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total_pop,pop_under_20,pop_20_64,pop_65_plus,perc_pop_under_20,perc_pop_20_64,perc_pop_65_plus) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
    grepl('pop',measure)==T ~ "count",
    grepl('perc',measure)==T ~ "percentage"),
         MOE='')


#2. Age distribution afor NCR
acs_data_ncr <- acs_data_ncr_wd %>%
  mutate(total_pop=total_popE,
         pop_under_20 = male_under_5E + male_5_9E + male_10_14E + male_15_17E + male_18_19E + 
           female_under_5E + female_5_9E + female_10_14E + female_15_17E + female_18_19E,
         pop_20_64 = male_20E + male_21E + male_22_24E + male_25_29E + male_30_34E + male_35_39E +
           male_40_44E +  male_45_49E + male_50_54E + male_55_59E + male_60_61E + male_62_64E + 
           female_20E + female_21E + female_22_24E + female_25_29E + female_30_34E + female_35_39E +
           female_40_44E +  female_45_49E + female_50_54E + female_55_59E + female_60_61E + female_62_64E,
         pop_65_plus = male_65_66E + male_67_69E + male_70_74E + male_75_79E + male_80_84E + male_over_85E +
           female_65_66E + female_67_69E + female_70_74E + female_75_79E + female_80_84E + male_over_85E,
         perc_pop_under_20 = 100*pop_under_20/total_pop,
         perc_pop_20_64 = 100*pop_20_64/total_pop,
         perc_pop_65_plus = 100*pop_65_plus/total_pop) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total_pop,pop_under_20,pop_20_64,pop_65_plus,perc_pop_under_20,perc_pop_20_64,perc_pop_65_plus) %>%
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
savepath = "Age/data/distribution/"
readr::write_csv(acs_data_va, xzfile(paste0(savepath,"va_trctbg_acs_20092020_age_demographics.csv.xz"), compression = 9))
readr::write_csv(acs_data_ncr, xzfile(paste0(savepath,"ncr_trctbg_acs_20092020_age_demographics.csv.xz"), compression = 9))




