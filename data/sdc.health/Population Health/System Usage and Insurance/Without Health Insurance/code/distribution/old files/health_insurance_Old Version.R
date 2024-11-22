# pull NCR ACS health insurance data
# correct health district geoids and add hlth_ins_pct var to VA data

library(DBI)
library(dplyr)
library(tidycensus)
library(tidyverse)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# DC, MD, VA data pull -----------------------------------------

states <- c("VA", "MD", "DC")
geographies <- c("county", "tract", "block group")
years <- c(2015:2019)

ins <- NULL
ins_wide <- NULL

for(state in states)
{
  for(year in years)
  {
    for(geography in geographies)
    {
      # pull health insurance vars
      vars = c("B27010_033", "B27010_050", "B27010_018", "B27010_034")

      ins <- get_acs(geography = geography, variables = vars, state = state, year = year,
                     geometry = FALSE, survey = "acs5", cache_table = TRUE, output = "wide") %>%
        transmute(
          geoid = GEOID,
          region_type = as.character(geography),
          region_name = NAME,
          year = year,
          no_hlth_ins_pct = 100 * (B27010_033E + B27010_050E)/(B27010_018E + B27010_034E),
          hlth_ins_pct = 100 - no_hlth_ins_pct
        ) %>%
        arrange(geoid)

      ins_wide <- rbind(ins_wide, ins)
    }
  }
}

# Format change to long and add measure_type column

ins_long <- gather(ins_wide, measure, value, no_hlth_ins_pct:hlth_ins_pct)
ins_long$measure_type <- "percent"


# filter to NCR and VA ------------------------------------------

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

ncr_ins <- ins_long %>% dplyr::filter(str_detect(geoid, ncr_counties))
va_ins <- ins_long %>% dplyr::filter(str_detect(geoid, "^51"))


# aggregate VA data to health districts ---------------------------

con <- get_db_conn()
hd_cts <- DBI::dbReadTable(con, c("dc_health_behavior_diet", "va_hd_vhd_2021_virginia_health_districts"))
hd <- DBI::dbReadTable(con, c("dc_geographies", "va_hd_vdh_2021_health_district_geo_names"))
DBI::dbDisconnect(con)

hd_cts$geometry <- NULL

va_ct_ins <- va_ins %>%
  filter(region_type == "county")

# add health district column

va_ct_ins_whd <- merge(va_ct_ins, hd_cts[ , c(1,3)], by.x = "geoid", by.y = "geoid_county", all.x = TRUE)

va_hd_ins <- va_ct_ins_whd %>%
  group_by(health_district, year, measure) %>%
  summarise(value = mean(value)) %>%
  rename(region_name = health_district)

# add new hd geoids

new_va_hd_ins <- merge(va_hd_ins, hd, by = "region_name", all.x = TRUE)
new_va_hd_ins$measure_type = "percent"
new_va_hd_ins <- new_va_hd_ins[ , c(5,6,1,2,3,4,7)]

va_cttr_ins <- va_ins %>%
  filter(region_type == "county" | region_type == "tract")
va_hdcttr_ins <- rbind(va_cttr_ins, new_va_hd_ins)


# write to database ---------------------

con <- get_db_conn()
dc_dbWriteTable(con, "dc_health_behavior_diet",
                "ncr_cttrbg_acs5_2015_2019_no_health_insurance_19_to_64", ncr_ins)
dc_dbWriteTable(con, "dc_health_behavior_diet",
                "va_hdcttr_acs5_2015_2019_no_health_insurance_19_to_64", va_hdcttr_ins)
DBI::dbDisconnect(con)

