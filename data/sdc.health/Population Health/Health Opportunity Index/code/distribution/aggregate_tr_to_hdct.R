# Given the VDH tract level data, this file aggregates the data to county and health district 
# levels using population weighted averages.

library(tidycensus)
library(dplyr)
library(readr)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get population data

yrs = c(2017, 2022)  # 2022 ACS data not yet available.  Used 2021 instead
pop_tr <- NULL 

for (j in 1:length(yrs))
{
  y = ifelse(yrs[j] == 2022, 2021, yrs[j])
  
  pop_tr_yr <- get_acs(geography = "tract", variables = "B01003_001", state = "VA", 
                       year = y, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                       output = "wide") %>%
    transmute(
      geoid = GEOID,
      year = yrs[j],
      pop = B01003_001E
    ) 
  
  pop_tr <- rbind(pop_tr, pop_tr_yr)
}

# aggregate each working/tract_data file

tr_files <- list.files("Population Health/Health Opportunity Index/data/working/tract_data/")

for (i in 1:length(tr_files)) 
{
  print(paste0("Iteration ", i, "----------------"))
  
  tract <- read_csv(paste0("Population Health/Health Opportunity Index/data/working/tract_data/", tr_files[i])) 
  tract$geoid <- as.character(tract$geoid)
  
  m = unique(tract$measure)
  
  # aggregate to county level using population weighted estimate
  
  tract$st_fips <- substr(tract$geoid, 1, 5)
  
  if (length(setdiff(tract$geoid, pop_tr$geoid)) > 0)  # empty - good
  { 
    print(paste0("Check tract ids in ", tr_files[i]))
  }
  
  tract <- merge(tract, pop_tr, by = c("geoid", "year"), all.x = TRUE)
  tract$pop_wgt_val <- tract$value * tract$pop
  
  county <- tract %>%
    group_by(st_fips, year) %>%
    summarise(
      ct_pop = sum(pop),
      ct_pop_wgt_val = sum(pop_wgt_val)
    )
  
  county$value <- county$ct_pop_wgt_val / county$ct_pop
  
  county <- county %>%
    rename(geoid = st_fips) %>%
    mutate(
      measure = m,
      moe = ""
    ) 
  
  # aggregate to health district level using population weighted estimates
  
  county <- merge(county, ct_hd_crosswalk[ , c("ct_geoid", "hd_geoid")], 
                  by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)
  
  hlth_dis <- county %>%
    group_by(hd_geoid, year) %>%
    summarise(
      hd_pop = sum(ct_pop),
      hd_pop_wgt_val = sum(ct_pop_wgt_val)
    )
  
  hlth_dis$value <- hlth_dis$hd_pop_wgt_val / hlth_dis$hd_pop
  
  hlth_dis <- hlth_dis %>%
    rename(geoid = hd_geoid) %>%
    mutate(
      measure = m,
      moe = ""
    ) 
  
  tract <- tract %>%
    select(geoid, measure, value, year, moe)
  
  county <- county %>%
    select(geoid, measure, value, year, moe)
  
  hlth_dis <- hlth_dis %>%
    select(geoid, measure, value, year, moe)
  
  hdcttr <- rbind(hlth_dis, county, tract)
  
  write_csv(hdcttr, xzfile(paste0("Population Health/Health Opportunity Index/data/distribution/va_hdcttr_", 
                         substr(tr_files[i], 7, nchar(tr_files[i]))), compression = 9) )
   
  
  tract <- NULL
  county <- NULL
  hlth_dis <- NULL
  hdcttr <- NULL
}
