#
# ingest ACS data: households receiving SNAP
#

library(dplyr)
library(tidycensus)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# geographies and years to pull from ACS

geographies <- c("county", "tract", "block group")  
years <- c(2013:2023)  

#
# DATA PULL ------------------------------------ 
#

hh_snap_yr <- NULL
hh_snap_all_yrs <- NULL

for(year in years)
{
  for(geography in geographies)
  {
    # pull educational attainment table 
    vars = c("B22010_001", "B22010_002")
    
    hh_snap_yr <- get_acs(geography = geography, variables = vars, state = c("VA", "MD", "DC"), 
                          year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                          output = "wide") %>% 
      transmute(
        geoid = GEOID,
        region_type = as.character(geography),
        region_name = NAME,
        year = year,
        hh_received_snap_cnt = B22010_002E,
        hh_received_snap_pct = 100 * hh_received_snap_cnt / B22010_001E,
        population = B22010_001E
      ) %>%
      arrange(geoid)
    
    hh_snap_all_yrs <- rbind(hh_snap_all_yrs, hh_snap_yr)
  }
}

write.csv(hh_snap_all_yrs, "Food and Nutrition Assistance/Supplemental Nutrition Assistance Program (SNAP)/data/working/vamddc_hh_receiving_snap.csv", row.names = F)
#
# ACS has stable API - not saving into data/original
#

rm(hh_snap_yr)
