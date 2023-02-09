# Get the DC ACS data at the block group level
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
  female_over_85 = "B01001_049",
  male = "B01001_002",
  female = "B01001_026",
  wht_alone = "B02001_002",
  afr_amer_alone = "B02001_003",
  amr_ind_alone = "B02001_004",
  asian_alone = "B02001_005",
  hispanic = "B03002_012")


# list of years
years <- 2013:2020

# Download the data from ACS for DC
acs_data_dc_wd <- NULL
for (year in years){
  temp <- data.table::setDT(
    tidycensus::get_acs(
      state = "DC",
      survey = "acs5",
      year = year,
      geography = 'block group',
      output = "wide",
      variables = named_acs_var_list
    )
  )
  
  temp$year <- year
  temp$region_type <- 'block group'
  acs_data_dc_wd <- rbind(acs_data_dc_wd, temp)
}



# Compute statistics -----------------------------------------------------------------------------
acs_data_dc <- acs_data_dc_wd %>%
  mutate(total_pop=total_popE,
         pop_under_20 = male_under_5E + male_5_9E + male_10_14E + male_15_17E + male_18_19E + 
           female_under_5E + female_5_9E + female_10_14E + female_15_17E + female_18_19E,
         pop_20_64 = male_20E + male_21E + male_22_24E + male_25_29E + male_30_34E + male_35_39E +
           male_40_44E +  male_45_49E + male_50_54E + male_55_59E + male_60_61E + male_62_64E + 
           female_20E + female_21E + female_22_24E + female_25_29E + female_30_34E + female_35_39E +
           female_40_44E +  female_45_49E + female_50_54E + female_55_59E + female_60_61E + female_62_64E,
         pop_65_plus = male_65_66E + male_67_69E + male_70_74E + male_75_79E + male_80_84E + male_over_85E +
           female_65_66E + female_67_69E + female_70_74E + female_75_79E + female_80_84E + male_over_85E,
         male = maleE, 
         female = femaleE,
         wht_alone = wht_aloneE, afr_amer_alone = afr_amer_aloneE, amr_ind_alone = amr_ind_aloneE, asian_alone = asian_aloneE, hispanic = hispanicE) %>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,total_pop,pop_under_20,pop_20_64,pop_65_plus,wht_alone,afr_amer_alone,amr_ind_alone,asian_alone,hispanic,male,female) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(measure_type=case_when(
    grepl('pop',measure)==T ~ "count",
    grepl('perc',measure)==T ~ "percentage"),
    MOE='')



# Save the data ----------------------------------------------------------------------------------
savepath = "Synthetic_population/Housing_units_distribution/DC/data/working/"
readr::write_csv(acs_data_dc, xzfile(paste0(savepath,"dc011_bg_acs_20092020_demographics.csv.xz"), compression = 9))




