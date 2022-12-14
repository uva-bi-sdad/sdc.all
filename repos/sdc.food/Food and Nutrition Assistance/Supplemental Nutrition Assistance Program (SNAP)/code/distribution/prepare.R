#
# Prepare ACS data: households receiving SNAP
#

library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(naniar)

# Data frame from running ingest.R: hh_snap_all_yrs

#
# Missingness check ------------------------------------
#

miss_var_summary(hh_snap_all_yrs)  # NaN in snap_pct when snap_cnt = population = 0
hh_snap_all_yrs[is.na(hh_snap_all_yrs$hh_received_snap_pct), "hh_received_snap_pct"] <- 0


#
# Aggregate VA county data to VA Health District data
#

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/va_geo_vhd_2020_health_districts/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get VA county data

va_ct_data <- hh_snap_all_yrs %>%
  filter(substr(geoid,1,2) == "51" & region_type == "county")

# merge with health district info

va_ct_data <- merge(va_ct_data, ct_hd_crosswalk[ , c(1,3,4)], by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

miss_var_summary(va_ct_data) 
# There is missing health district info for Bedford City (51515) in 2013.
# Bedford City was an independent city but became a town in July, 2013.
# It is only present in this data set for 2013, and is then reported as part of Bedford County in 2014+.
# I am adding the health district to 2013 Bedford City as Central Virginia, 51_hd_05
va_ct_data[va_ct_data$geoid == "51515", "hd_geoid"] <- "51_hd_05"
va_ct_data[va_ct_data$geoid == "51515", "hd_name"] <- "Central Virginia"

# aggregate county data to health district data

va_hd_data <- va_ct_data %>%
  group_by(hd_geoid, hd_name, year) %>%
  summarize(snap_cnt = sum(hh_received_snap_cnt),
            pop = sum(population)) %>%
  mutate(snap_pct = 100 * (snap_cnt/pop))

colnames(va_hd_data) <- c("geoid", "region_name", "year", "hh_received_snap_cnt", "population", "hh_received_snap_pct")
va_hd_data$region_type <- "health district"

va_hd_data <- va_hd_data[ , c(1,7,2,3,4,6,5)]

# add health district data to hh_snap_all_years

hh_snap_all_yrs <- rbind(hh_snap_all_yrs, va_hd_data)
 

#
# Wrangle data -------------------------------
#

# Format to long and add measure_type column

hh_snap_long <- gather(hh_snap_all_yrs, measure, value, hh_received_snap_cnt:population)
hh_snap_long$measure_type <- "count"
hh_snap_long[grepl("pct", hh_snap_long$measure, fixed = TRUE), ]$measure_type <- "percent"

#
# NCR ------------------------------------------
#

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_hh_snap <- hh_snap_long %>% 
  dplyr::filter(str_detect(geoid, ncr_counties))

# save

write_csv(ncr_hh_snap, xzfile("data/hh_receiving_snap/distribution/ncr_cttrbg_acs_2013_2020_hh_receiving_snap.csv.xz", compression = 9))


#
# VA -----------------------------------------
#

# filter to VA

va_hh_snap <- hh_snap_long %>% 
  dplyr::filter(str_detect(geoid, "^51"))

# save

write_csv(va_hh_snap, xzfile("data/hh_receiving_snap/distribution/va_hdcttrbg_acs_2013_2020_hh_receiving_snap.csv.xz", compression = 9))

