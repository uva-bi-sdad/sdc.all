#
# ingest ACS data: type of internet in household 
#

library(dplyr)
library(tidycensus)
library(tidyr)
library(stringr)
library(readr)
library(naniar)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# geographies and years to pull from ACS

geographies <- c("county", "tract", "block group")  
years <- c(2017:2023)  

#
# DATA PULL ------------------------------------ 
#

hh_int_yr <- NULL
hh_int_all_yrs <- NULL

for(year in years)
{
  for(geography in geographies)
  {
    # pull internet type table 
    vars = c("B28002_001", "B28002_013", "B28002_004", "B28002_007")
    
    hh_int_yr <- get_acs(geography = geography, variables = vars, state = c("VA", "MD", "DC"), 
                          year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                          output = "wide") %>% 
      transmute(
        geoid = GEOID,
        region_type = as.character(geography),
        region_name = NAME,
        year = year,
        num_hh = B28002_001E,
        num_hh_without_internet = B28002_013E,
        num_hh_with_broadband = B28002_004E,
        num_hh_with_cable_fiber_dsl = B28002_007E,
        perc_hh_without_internet = 100 * num_hh_without_internet / B28002_001E,
        perc_hh_with_broadband = 100 * num_hh_with_broadband / B28002_001E,
        perc_hh_with_cable_fiber_dsl = 100 * num_hh_with_cable_fiber_dsl / B28002_001E,
      ) %>%
      arrange(geoid)
    
    hh_int_all_yrs <- rbind(hh_int_all_yrs, hh_int_yr)
  }
}

rm(hh_int_yr)


#
# Missingness check ------------------------------------
#

miss_var_summary(hh_int_all_yrs)  # NaN in perc_var when num_var = num_hh = 0
hh_int_all_yrs[is.na(hh_int_all_yrs$perc_hh_without_internet), "perc_hh_without_internet"] <- 0
hh_int_all_yrs[is.na(hh_int_all_yrs$perc_hh_with_broadband), "perc_hh_with_broadband"] <- 0
hh_int_all_yrs[is.na(hh_int_all_yrs$perc_hh_with_cable_fiber_dsl), "perc_hh_with_cable_fiber_dsl"] <- 0

#
# Aggregate VA county data to VA Health District data
#

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get VA county data

va_ct_data <- hh_int_all_yrs %>%
  filter(substr(geoid,1,2) == "51" & region_type == "county")

# merge with health district info

va_ct_data <- merge(va_ct_data, ct_hd_crosswalk[ , c(1,3,4)], by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

miss_var_summary(va_ct_data) # no missing data

# aggregate county data to health district data

va_hd_data <- va_ct_data %>%
  group_by(hd_geoid, hd_name, year) %>%
  summarize(no_int_cnt = sum(num_hh_without_internet),
            bb_cnt = sum(num_hh_with_broadband),
            cfdsl_cnt = sum(num_hh_with_cable_fiber_dsl), 
            hh = sum(num_hh)) %>%
  mutate(no_int_pct = 100 * (no_int_cnt/hh),
         bb_pct = 100 * (bb_cnt/hh),
         cfdsl_pct = 100 * (cfdsl_cnt/hh))

colnames(va_hd_data) <- c("geoid", "region_name", "year", "num_hh_without_internet", 
                          "num_hh_with_broadband", "num_hh_with_cable_fiber_dsl", "num_hh", 
                          "perc_hh_without_internet", "perc_hh_with_broadband", 
                          "perc_hh_with_cable_fiber_dsl")
va_hd_data$region_type <- "health district"

va_hd_data <- va_hd_data[ , c(1,11,2,3,7,4,5,6,8,9,10)]

# add health district data to hh_int_all_years

hh_int_all_yrs <- rbind(hh_int_all_yrs, va_hd_data)


#
# Wrangle data -------------------------------
#

# Format to long 

hh_int_long <- gather(hh_int_all_yrs, measure, value, num_hh:perc_hh_with_cable_fiber_dsl)
hh_int_long$moe <- ""


#
# NCR ------------------------------------------
#

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_hh_int <- hh_int_long %>% 
  dplyr::filter(str_detect(geoid, ncr_counties))

# save

write_csv(ncr_hh_int, xzfile(paste0("Wired/Adoption/Internet Type/data/distribution/ncr_cttrbg_acs_", min(years), "_", max(years), "_hh_internet_type.csv.xz"), compression = 9))


#
# VA -----------------------------------------
#

# filter to VA

va_hh_int <- hh_int_long %>% 
  dplyr::filter(str_detect(geoid, "^51"))

# save

write_csv(va_hh_int, xzfile(paste0("Wired/Adoption/Internet Type/data/distribution/va_hdcttrbg_acs_", min(years), "_", max(years), "_hh_internet_type.csv.xz"), compression = 9))

