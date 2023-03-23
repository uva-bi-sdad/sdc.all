# Prepare AHEC region geographies file provided by the VDH Primary Care Office

#packages
library(tidyverse)
library(readr)
library(readxl)
library(tigris)
library(sf)

# working directory
setwd("~/git/sdc.geographies_dev/VA/State Geographies/AHEC Regions")

# load AHEC regions (shared by Rexford Anson-Dwamena)
ahec <- read_excel("data/original/AHEC_RUCAs.xlsx")
# correct Blue Ridge AHEC name
ahec$`AHEC Name`[ahec$`AHEC Code` == 1] = "Blue Ridge AHEC"

# get county geographies
va_counties <- counties(state="VA", year =2021)
va_counties <- va_counties[, c("GEOID", "geometry")]

# add counties to ahec data
ahec <- left_join(ahec, va_counties, by=c("FIPS" = "GEOID")) 
ahec_df <- ahec %>% group_by(`AHEC Name`) %>%
  summarise(geometry = st_union(geometry))

names(ahec_df)[1] <- "region_name"

# assign region_type
ahec_df$region_type <- "AHEC region"

ahec_df <- ahec_df %>% 
  mutate(geoid = case_when(
    region_name == "Blue Ridge AHEC" ~ "51_ahec_01",
    region_name == "Capital AHEC" ~ "51_ahec_02",
    region_name == "Northern Virginia AHEC" ~ "51_ahec_04",
    region_name == "Eastern Virginia AHEC" ~ "51_ahec_03",
    region_name == "Rappahannock AHEC" ~ "51_ahec_05",
    region_name == "South Central AHEC" ~ "51_ahec_07",
    region_name == "Southside AHEC" ~ "51_ahec_06",
    region_name == "Southwest Virginia AHEC" ~ "51_ahec_08"))

# Assign year
ahec_df$year <- "2021"

# Select final columns
out_df <- ahec_df[, c("geoid", "region_name", "region_type", "year", "geometry")]
# Export final dataset
sf::st_write(out_df, "data/distribution/va_geo_vhd_2021_ahec_regions.geojson")

