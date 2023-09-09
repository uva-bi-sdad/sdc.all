library(readxl)
library(data.table)
library(fabricatr)

# hoi_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi.xlsx", sheet = 1))
hoi_2022 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))

# 2017 --------------------
# 
# hoi_2017_sel <-
#   hoi_2017[, .(
#     geoid = Ctfips,
#     measure = "health_opportunity_indicator",
#     value = `Profile Selector`,
#     year = "2017",
#     moe = ""
#   )]
# 
# hoi_2017_sel[value == "Very Low", value := "1"]
# hoi_2017_sel[value == "Low", value := "2"]
# hoi_2017_sel[value == "Average", value := "3"]
# hoi_2017_sel[value == "High", value := "4"]
# hoi_2017_sel[value == "Very High", value := "5"]
# hoi_2017_sel[, value := as.integer(value)]
# 
# hoi_2017_sel <- unique(hoi_2017_sel)
# 
# # bedford city tract stil in VDH data:
# # bedford city (51515050100) became Bedford County tract (51019050100)
# # updating tract id for bedford city  
# 
# hoi_2017_sel[hoi_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"



# # aggregate to county level using population weighted estimate
# 
# hoi_tr_2017 <- hoi_2017_sel
# hoi_tr_2017$st_fips <- substr(hoi_tr_2017$geoid, 1, 5)
# 
# pop_tr_2017 <- get_acs(geography = "tract", variables = "B01003_001", state = "VA", 
#                       year = 2017, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
#                       output = "wide") %>%
#   transmute(
#     geoid = GEOID,
#     pop = B01003_001E
#   ) 
# 
# #setdiff(hoi_tr_2017$geoid, pop_tr_2017$geoid)  # empty - good
# 
# hoi_tr_2017 <- merge(hoi_tr_2017, pop_tr_2017, by = "geoid", all.x = TRUE)
# hoi_tr_2017$pop_wgt_val <- hoi_tr_2017$value * hoi_tr_2017$pop
# 
# hoi_ct_2017 <- hoi_tr_2017 %>%
#   group_by(st_fips) %>%
#   summarise(
#     ct_pop = sum(pop),
#     ct_pop_wgt_val = sum(pop_wgt_val)
#     )
# 
# hoi_ct_2017$value <- hoi_ct_2017$ct_pop_wgt_val / hoi_ct_2017$ct_pop
# 
# hoi_ct_2017 <- hoi_ct_2017 %>%
#   rename(geoid = st_fips) %>%
#   mutate(
#     measure = "health_opportunity_indicator",
#     year = "2017",
#     moe = ""
#     ) 
# 
# # aggregate to health district level using population weighted estimates
# 
# ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
#                             col_types = "cccc")
# 
# hoi_ct_2017 <- merge(hoi_ct_2017, ct_hd_crosswalk[ , c("ct_geoid", "hd_geoid")], 
#                      by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)
# 
# hoi_hd_2017 <- hoi_ct_2017 %>%
#   group_by(hd_geoid) %>%
#   summarise(
#     hd_pop = sum(ct_pop),
#     hd_pop_wgt_val = sum(ct_pop_wgt_val)
#   )
# 
# hoi_hd_2017$value <- hoi_hd_2017$hd_pop_wgt_val / hoi_hd_2017$hd_pop
# 
# hoi_hd_2017 <- hoi_hd_2017 %>%
#   rename(geoid = hd_geoid) %>%
#   mutate(
#     measure = "health_opportunity_indicator",
#     year = "2017",
#     moe = ""
#   ) 
# 
# hoi_tr_2017 <- hoi_tr_2017 %>%
#   select(geoid, measure, value, year, moe)
# 
# hoi_ct_2017 <- hoi_ct_2017 %>%
#   select(geoid, measure, value, year, moe)
# 
# hoi_hd_2017 <- hoi_hd_2017 %>%
#   select(geoid, measure, value, year, moe)
# 
# 
# hoi_hdcttr_2017 <- rbind(hoi_hd_2017, hoi_ct_2017, hoi_tr_2017)


# 2022 -------------------------------------------------------

hoi_2022_sel <- 
  hoi_2022[, .(
    geoid = CT, 
    measure = "health_opportunity_indicator",
    value = `Composite in Quintiles`,
    year = "2020",
    moe = ""
  )]

hoi_2022_sel[value == "Very Low Opportunity", value := "1"]
hoi_2022_sel[value == "Low Opportunity", value := "2"]
hoi_2022_sel[value == "Moderate Opportunity", value := "3"]
hoi_2022_sel[value == "High Opportunity", value := "4"]
hoi_2022_sel[value == "Very High Opportunity", value := "5"]
hoi_2022_sel[, value := as.integer(value)]

hoi_2022_sel <- unique(hoi_2022_sel)

# hoi <- rbind(hoi_2017_sel, hoi_2022_sel)

# write to working
# readr::write_csv(hoi, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_2022_health_opportunity_profile.csv.xz", compression = 9))

# 2022 only
readr::write_csv(hoi_2022_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2020_health_opportunity_profile.csv.xz", compression = 9))



# # aggregate to county level using population weighted estimate
# 
# hoi_tr_2022 <- hoi_2022_sel
# hoi_tr_2022$st_fips <- substr(hoi_tr_2022$geoid, 1, 5)
# 
# # 2022 data is not available from ACS - used 2021 instead. 
# pop_tr_2022 <- get_acs(geography = "tract", variables = "B01003_001", state = "VA", 
#                        year = 2021, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
#                        output = "wide") %>%
#   transmute(
#     geoid = GEOID,
#     pop = B01003_001E
#   ) 
# 
# setdiff(hoi_tr_2022$geoid, pop_tr_2022$geoid)  # empty - good
# 
# hoi_tr_2022 <- merge(hoi_tr_2022, pop_tr_2022, by = "geoid", all.x = TRUE)
# hoi_tr_2022$pop_wgt_val <- hoi_tr_2022$value * hoi_tr_2022$pop
# 
# hoi_ct_2022 <- hoi_tr_2022 %>%
#   group_by(st_fips) %>%
#   summarise(
#     ct_pop = sum(pop),
#     ct_pop_wgt_val = sum(pop_wgt_val)
#   )
# 
# hoi_ct_2022$value <- hoi_ct_2022$ct_pop_wgt_val / hoi_ct_2022$ct_pop
# 
# hoi_ct_2022 <- hoi_ct_2022 %>%
#   rename(geoid = st_fips) %>%
#   mutate(
#     measure = "health_opportunity_indicator",
#     year = "2022",
#     moe = ""
#   ) 
# 
# # aggregate to health district level using population weighted estimates
# 
# ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
#                             col_types = "cccc")
# 
# hoi_ct_2022 <- merge(hoi_ct_2022, ct_hd_crosswalk[ , c("ct_geoid", "hd_geoid")], 
#                      by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)
# 
# hoi_hd_2022 <- hoi_ct_2022 %>%
#   group_by(hd_geoid) %>%
#   summarise(
#     hd_pop = sum(ct_pop),
#     hd_pop_wgt_val = sum(ct_pop_wgt_val)
#   )
# 
# hoi_hd_2022$value <- hoi_hd_2022$hd_pop_wgt_val / hoi_hd_2022$hd_pop
# 
# hoi_hd_2022 <- hoi_hd_2022 %>%
#   rename(geoid = hd_geoid) %>%
#   mutate(
#     measure = "health_opportunity_indicator",
#     year = "2022",
#     moe = ""
#   ) 
# 
# hoi_tr_2022 <- hoi_tr_2022 %>%
#   select(geoid, measure, value, year, moe)
# 
# hoi_ct_2022 <- hoi_ct_2022 %>%
#   select(geoid, measure, value, year, moe)
# 
# hoi_hd_2022 <- hoi_hd_2022 %>%
#   select(geoid, measure, value, year, moe)
# 
# hoi_hdcttr_2022 <- rbind(hoi_hd_2022, hoi_ct_2022, hoi_tr_2022)
# 
# 
# # combine
# hoi <- rbind(hoi_hdcttr_2017, hoi_hdcttr_2022)
#
#write_csv(hoi, xzfile("Population Health/Health Opportunity Index/data/distribution/va_cttr_vdh_2017_2022_health_opportunity_profile.csv.xz", compression = 9))

