# Replicating Population Churning Indicator of the VHD's HOI Community Environmental Profile
# The amount of population turnover within a community. 
# It measure the rate at which people both move into a community and move out of a community.
# https://apps.vdh.virginia.gov/omhhe/hoi/community-environmental-profile

# packages
library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(reshape2)

# data from HOI website
orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/pop_churn.xlsx")
df_tracts <- orig_df[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
                        "Ctfips", "Indicator Selector")] 
df_tracts <- df_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# assign quintiles
df_tracts <- df_tracts %>% mutate(
  quintiles = case_when(
    `Indicator Selector` == "Very Low" ~ 1, 
    `Indicator Selector` == "Low" ~ 2,
    `Indicator Selector` == "Average" ~ 3,
    `Indicator Selector` == "High" ~ 4,
    `Indicator Selector` == "Very High" ~ 5
  )
)
df_tracts["new_id"] <- paste0(df_tracts$Ctfips, "_", df_tracts$quintiles)
# remove duplicates
df_tracts_nodups <- df_tracts %>% distinct()
# check if the census tarct ID is unique
#df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique

# rename measures
out_df <- df_tracts_nodups %>% 
  rename("geoid"= "Ctfips",
        "pop_churning_indicator" = "quintiles")
out_df <- out_df[,c("geoid", "pop_churning_indicator")]

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
write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/distribution/va_tr_vdh_2017_pop_churning_index.csv.xz", compression = 9))
