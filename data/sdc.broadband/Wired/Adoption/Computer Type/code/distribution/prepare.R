#
# ingest ACS data: households without computer
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

hh_compdev_yr <- NULL
hh_compdev_all_yrs <- NULL

for(year in years)
{
  for(geography in geographies)
  {
    # pull computer type table 
    vars = c("B28001_001", "B28001_011")
    
    hh_compdev_yr <- get_acs(geography = geography, variables = vars, state = c("VA", "MD", "DC"), 
                          year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                          output = "wide") %>% 
      transmute(
        geoid = GEOID,
        region_type = as.character(geography),
        region_name = NAME,
        year = year,
        num_hh_without_compdev = B28001_011E,
        perc_hh_without_compdev = 100 * num_hh_without_compdev / B28001_001E,
        num_hh = B28001_001E
      ) %>%
      arrange(geoid)
    
    hh_compdev_all_yrs <- rbind(hh_compdev_all_yrs, hh_compdev_yr)
  }
}

rm(hh_compdev_yr)


#
# Missingness check ------------------------------------
#

miss_var_summary(hh_compdev_all_yrs)  # NaN in perc_without when num_without = num_hh = 0
hh_compdev_all_yrs[is.na(hh_compdev_all_yrs$perc_hh_without_compdev), "perc_hh_without_compdev"] <- 0


#
# Aggregate VA county data to VA Health District data
#

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get VA county data

va_ct_data <- hh_compdev_all_yrs %>%
  filter(substr(geoid,1,2) == "51" & region_type == "county")

# merge with health district info

va_ct_data <- merge(va_ct_data, ct_hd_crosswalk[ , c(1,3,4)], by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

miss_var_summary(va_ct_data) # no missing data

# aggregate county data to health district data

va_hd_data <- va_ct_data %>%
  group_by(hd_geoid, hd_name, year) %>%
  summarize(no_compdev_cnt = sum(num_hh_without_compdev),
            hh = sum(num_hh)) %>%
  mutate(no_compdev_pct = 100 * (no_compdev_cnt/hh))

colnames(va_hd_data) <- c("geoid", "region_name", "year", "num_hh_without_compdev", "num_hh", "perc_hh_without_compdev")
va_hd_data$region_type <- "health district"

va_hd_data <- va_hd_data[ , c(1,7,2,3,4,6,5)]

# add health district data to hh_compdev_all_years

hh_compdev_all_yrs <- rbind(hh_compdev_all_yrs, va_hd_data)


#
# Wrangle data -------------------------------
#

# Format to long 

hh_compdev_long <- gather(hh_compdev_all_yrs, measure, value, num_hh_without_compdev:num_hh)
hh_compdev_long$moe <- ""


#
# NCR ------------------------------------------
#

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_hh_compdev <- hh_compdev_long %>% 
  dplyr::filter(str_detect(geoid, ncr_counties))

# save

write_csv(ncr_hh_compdev, xzfile(paste0("Wired/Adoption/Computer Type/data/distribution/ncr_cttrbg_acs_", min(years), "_", max(years), "_hh_without_compdev.csv.xz"), compression = 9))


#
# VA -----------------------------------------
#

# filter to VA

va_hh_compdev <- hh_compdev_long %>% 
  dplyr::filter(str_detect(geoid, "^51"))

# save

write_csv(va_hh_compdev, xzfile(paste0("Wired/Adoption/Computer Type/data/distribution/va_hdcttrbg_acs_", min(years), "_", max(years), "_hh_without_compdev.csv.xz"), compression = 9))

