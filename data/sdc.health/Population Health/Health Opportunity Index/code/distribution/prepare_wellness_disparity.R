# Replicating Wellness Disparity Profile of the VHD's HOI
# Wellness Disparity Profile is a measure of the disparate access to services in a community. 
# This profile demonstrates the direct influence of social factors on the opportunity of all Virginians to live 
# a long and healthy life.
# https://apps.vdh.virginia.gov/omhhe/hoi/wellness-disparity-profile

# packages
library(readxl)
library(tidycensus)
library(dplyr)
library(tidyverse)
library(sf)
library(geojsonio)

# working directory
setwd("~/git/sdc.health_dev/Population Health/Health Opportunity Index/")

# data from HOI website
orig_df <- read_excel("data/original/well_disparity.xlsx")
df_tracts <- orig_df[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
                        "Ctfips", "Indicator Selector")] 
df_tracts <- df_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# assign quintiles
df_tracts <- df_tracts %>% mutate(
  quintiles = case_when(
    `Indicator Selector` == "Very Low" ~ 1, 
    `Indicator Selector` == "Low" ~2,
    `Indicator Selector` == "Average" ~3,
    `Indicator Selector` == "High" ~4,
    `Indicator Selector` == "Very High" ~5
  )
)
df_tracts["new_id"] <- paste0(df_tracts$Ctfips, "_", df_tracts$quintiles)
# remove duplicates
df_tracts_nodups <- df_tracts %>% distinct()
# check if the census tarct ID is unique
# df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique

# rename measures
out_df <- df_tracts_nodups %>% 
  rename( "geoid"= "Ctfips",
          "wellness_disparity_quintile" = "quintiles",
          "wellness_disparity_level" = "Indicator Selector")
out_df <- out_df[,c("geoid", "wellness_disparity_quintile", "wellness_disparity_level")]

# geographies
geos_data <- read_csv("~/git/dc.metadata/data/region_name.csv.xz")
va_tract <- geos_data %>% filter(region_type == "tract" & substr(geoid, 1,2) == "51")

# add geographies
out_df <- left_join(out_df, va_tract, by=c("geoid"))

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
write_csv(out_df, "data/distribution/va_tr_vdh_2017_wellness_diparity_profile.csv")
