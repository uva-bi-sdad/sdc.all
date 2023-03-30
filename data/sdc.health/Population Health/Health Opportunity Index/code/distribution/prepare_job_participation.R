# Replicating Job Participation Component of the VHD's HOI Economic Opportunity Profile
# https://apps.vdh.virginia.gov/omhhe/hoi/economic-opportunity-profile

# packages
library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(reshape2)

# data from HOI website
ct_jpi <- read_excel("Population Health/Health Opportunity Index/data/original/LPI_ct_map.xlsx")
jpi_tracts <- ct_jpi[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
                                  "Ctfips", "Indicator Selector")] 
jpi_tracts <- jpi_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# assign quintiles
jpi_tracts <- jpi_tracts %>% mutate(
  quintiles = case_when(
    `Indicator Selector` == "Very Low" ~ 1, 
    `Indicator Selector` == "Low" ~ 2,
    `Indicator Selector` == "Average" ~ 3,
    `Indicator Selector` == "High" ~ 4,
    `Indicator Selector` == "Very High" ~ 5
  )
)
jpi_tracts["new_id"] <- paste0(jpi_tracts$Ctfips, "_", jpi_tracts$quintiles)
# remove duplicates
jpi_tracts_nodups <- jpi_tracts %>% distinct()
# check if the census tarct ID is unique
# jpi_tracts_unq <- jpi_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique

# rename measures
out_df <- jpi_tracts_nodups %>% 
  rename( "geoid"= "Ctfips",
          "job_participation_indicator" = "quintiles")
out_df <- out_df[,c("geoid", "job_participation_indicator")]

# geographies
geos_data <- st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Tract/2010/data/distribution/va_geo_census_cb_2010_census_tracts.geojson") %>%
  select(geoid, region_name, region_type)

geos_data$geometry <- NULL

# add geographies
out_df <- left_join(out_df, geos_data, by=c("geoid"))

# long format
out_long <- melt(out_df,
                     id.vars=c("geoid", "region_type", "region_name"),
                     variable.name="measure",
                     value.name="value"
)

# add missing columns
out_long["year"] <- "2017"
out_long["measure_type"] <- "index"
out_long["moe"] <- ""

# Select final columns
out_long <- out_long[, c("geoid", "region_name", "region_type", "year", "measure", "value", "measure_type", "moe")]

# save the dataset 
write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/distribution/va_tr_vdh_2017_job_participation_index.csv.xz", compression = 9))

# # add geographies
# jpi_tracts_geo <- left_join(jpi_tracts_nodups, acs_est14[,c("GEOID", "geometry")], by=c("Ctfips"="GEOID"))
# 
# # aggregate
# jpi_tracts_agg <- jpi_tracts_geo %>% group_by(`County Name`, `STFIPS (CountyHOI)`, `Indicator Selector`) %>%
#   summarise(quintiles = median(quintiles), 
#             geometry = st_union(geometry),
#             lhd_name = first(`LHD Name`),
#             tr_geoid = first(Ctfips)
#             )
# 
# jpi_pr <- read_excel("data/original/%ofPop.xlsx")
# 
# #################
# # MAPS
# #################
# 
# agg_acs19["new_id"] <- paste0(agg_acs19$geoid_cnt, "_", agg_acs19$quintiles)
# joint_df19 <- left_join(jpi_tracts, agg_acs19, by=c("new_id"))
# 
# agg_acs15["new_id"] <- paste0(agg_acs15$geoid_cnt, "_", agg_acs15$quintiles)
# joint_df15 <- left_join(jpi_tracts, agg_acs15, by=c("new_id"))
# 
# agg_acs14["new_id"] <- paste0(agg_acs14$geoid_cnt, "_", agg_acs14$quintiles)
# joint_df14 <- left_join(jpi_tracts, agg_acs14, by=c("new_id"))
# 
# # acs 14 labor force participation
# ggplot() +
#   geom_sf(data = agg_acs14, aes(geometry=geometry,  fill=quintiles)) +
#   ggtitle("Labor Force Participation from ACS 5-year Estimates Subject Table")
# 
# ggplot() +
#   geom_sf(data = jpi_tracts_agg, aes(geometry=geometry,  fill=quintiles)) +
#   ggtitle("Labor Force Participation from HOI Economic Opportunity Profile")
