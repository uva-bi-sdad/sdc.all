

# librairies ----------------------------------------------------------------------
library(dplyr)
library(sf)
library(httr)
library(sp)
library(data.table)
library(stringr)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(tidyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(rjson)
census_api_key(Sys.getenv('census_api_key'))




# load data -------------------------------------------------------------------------
# load parcel living units information
uploadpath = "Synthetic_population/Housing_units_distribution/Arlington/data/working/"
parcel_livunit <- read.csv(paste0(uploadpath,"va013_sdad_parcel_bg_livingunits.csv.xz"))


# get the acs data -------------------------------------------------------------------
named_acs_var_list1 <- c(
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
  total_race = "B02001_001",
  wht_alone = "B02001_002",
  afr_amer_alone = "B02001_003",
  native_alone = "B02001_004",
  asian_alone = "B02001_005",
  pacific_islander_alone = "B02001_006",
  other_language = "B02001_007",
  two_or_more = "B02001_008",
  pop_eth_tot = "B03003_001", 
  pop_hispanic_or_latino = "B03003_003",
  vet_denom = "B21001_001", 
  num_vet = "B21001_002")


named_acs_var_list2 <- c(
  total_hh = "C16002_001",
  limited_english_spanish = "C16002_004",
  limited_english_indo_europe = "C16002_007",
  limited_english_asian_pacific = "C16002_010",
  limited_english_other_language = "C16002_013")


# upload ACS and refine for each year ----------------------------------------
temp_acs <- NULL
years <- 2013:2020

for (year in years){
  # get the acs data for the year and the fairfax county
  if (year<2016){
    named_acs_var_list <- named_acs_var_list1
  }else{
    # limited english is only reported after 2016
    named_acs_var_list <- c(named_acs_var_list1,named_acs_var_list2)
  }
  temp <- data.table::setDT(
    tidycensus::get_acs(
      state = "VA",
      county="013",
      survey = "acs5",
      year = year,
      geography = 'block group',
      output = "wide",
      variables = named_acs_var_list
    )
  )
  temp$year <- year
  temp$region_type <- 'block group'
  temp_acs <- rbind(temp_acs, temp, fill=TRUE)
}


# acs treatment
temp_acs2 <- temp_acs %>%
  mutate(total_pop=total_popE,
         pop_under_20 = male_under_5E + male_5_9E + male_10_14E + male_15_17E + male_18_19E + 
           female_under_5E + female_5_9E + female_10_14E + female_15_17E + female_18_19E,
         pop_20_64 = male_20E + male_21E + male_22_24E + male_25_29E + male_30_34E + male_35_39E +
           male_40_44E +  male_45_49E + male_50_54E + male_55_59E + male_60_61E + male_62_64E + 
           female_20E + female_21E + female_22_24E + female_25_29E + female_30_34E + female_35_39E +
           female_40_44E +  female_45_49E + female_50_54E + female_55_59E + female_60_61E + female_62_64E,
         pop_65_plus = male_65_66E + male_67_69E + male_70_74E + male_75_79E + male_80_84E + male_over_85E +
           female_65_66E + female_67_69E + female_70_74E + female_75_79E + female_80_84E + male_over_85E,
         pop_male = maleE, 
         pop_female = femaleE,
         total_hh=total_hhE,
         hh_limited_english = limited_english_spanishE + limited_english_indo_europeE + limited_english_asian_pacificE + limited_english_other_languageE,
         total_race=total_raceE,
         pop_wht_alone = wht_aloneE, 
         pop_afr_amer_alone = afr_amer_aloneE, 
         pop_native_alone = native_aloneE, 
         pop_AAPI = (asian_aloneE + pacific_islander_aloneE), 
         pop_other = other_languageE,
         pop_two_or_more = two_or_moreE,
         pop_hispanic_or_latino = pop_hispanic_or_latinoE,
         pop_eth_tot = pop_eth_totE,
         pop_veteran = num_vetE, 
         pop_vet_denom = vet_denomE)%>%
  dplyr::select(geoid=GEOID,region_name=NAME,region_type,year,
                total_pop,pop_under_20,pop_20_64,pop_65_plus,
                pop_male,pop_female,
                total_hh,hh_limited_english,
                total_race,pop_wht_alone,pop_afr_amer_alone,pop_native_alone,pop_AAPI,pop_other,pop_two_or_more,pop_hispanic_or_latino,pop_eth_tot,
                pop_veteran,pop_vet_denom) %>%
  gather(measure, value, -c(geoid, region_name, region_type, year)) %>%
  select(geoid,region_name,region_type,year,measure,value) %>%
  mutate(census_year=if_else(year<2020,2010,2020),
         measure_type='count')



# compute the demographic multiplier -------------------------------------------------
# comments: a block group can have a set of parcels with a total of 0 living units, for those case we can refine the demographics informations. 
#           Those cases are identified with a multiplier equals to NA. we exclude them
temp <- NULL
for (t in 2009:2020) {
  temp_t <- parcel_livunit %>%
    filter(year<=t) %>%
    mutate(bg_geo=as.numeric(substr(geoid, 1, 12))) %>%
    group_by(bg_geo) %>%
    mutate(bg_unitcnt=sum(liv_unit, na.rm=T),
           mult=liv_unit/bg_unitcnt,
           year_obs=t) %>%
    rename(parid=geoid) %>%
    filter(!is.na(mult)) %>%
    select(parid,bg_geo,mult,year=year_obs)
  
  # combine
  temp <- rbind(temp,temp_t)
}

# add census_year
temp <- temp %>% mutate(census_year=if_else(year<2020,2010,2020))


# compute the demographics at the parcels --------------------------------------------
# note: refine the demographics for block groups with the demographics information
#       - some block group in parcel_livunit doesn't match the acs (2010) but belong to acs (2020), remove them
arl_parcel_dmg <- merge(temp_acs2, temp, by.x=c('geoid','year','census_year'), by.y=c('bg_geo','year','census_year'), all.y=T) %>%
  mutate(value=mult*value,
         region_name=paste0('parcel ',parid),
         region_type='parcel') %>%
  filter(!is.na(measure)) %>%
  select(geoid=parid,region_name,region_type,year,measure,value)


# save the data (small piece by year)
savepath = "Synthetic_population/Housing_units_distribution/Arlington/data/working/"
for (year in years) {
  readr::write_csv(arl_parcel_dmg[arl_parcel_dmg$year==year,], xzfile(paste0(savepath,"va013_pc_sdad_",year,"_demographics.csv.xz"), compression = 9))
}


