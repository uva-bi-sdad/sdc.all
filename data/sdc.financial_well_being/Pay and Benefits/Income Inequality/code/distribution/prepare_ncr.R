# Calculate the GINI coefficient for census tracts in Virginia:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)
library(readr)
library(magrittr)

source('utils/distribution/aggregate.R')

yrs <- c(2015:2023)
ncr_counties <- c("51013" , "51059" , "51600" , "51107" , "51610" , "51683", "51685" , "51153" , "51510" , "24021" , "24031" , "24033" , "24017", "11001")

# GET TRACTS
if(exists("ncr_tract_ginis_all")) rm("ncr_tract_ginis_all")
# for each year
for (y in yrs) {
  ##for each state
  for (s in c("51", "24", "11")) {
    # Get state counties
    counties <- substr(ncr_counties[ncr_counties %like% paste0("^", s)], 3, 5)
    # Get ACS data
    ncr_tract_ginis <- get_acs(geography = "tract",
                               variables = "B19083_001",
                               state = s,
                               county = counties,
                               geometry = F,
                               output = "wide",
                               year = y)

    ncr_tract_ginis$geoid <- ncr_tract_ginis$GEOID
    ncr_tract_ginis$measure <- "gini_index"
    ncr_tract_ginis$measure_type <- "index"
    ncr_tract_ginis$region_name <- ncr_tract_ginis$NAME
    ncr_tract_ginis$region_type <- "tract"
    ncr_tract_ginis$value <- ncr_tract_ginis$B19083_001E
    ncr_tract_ginis$year <- y
    ncr_tract_ginis$moe <- ncr_tract_ginis$B19083_001M

    if (exists("ncr_tract_ginis_all")) {
      ncr_tract_ginis_all <- rbindlist(list(ncr_tract_ginis_all, ncr_tract_ginis))
    } else {
      ncr_tract_ginis_all <- ncr_tract_ginis
    }
  }
}

ncr_tract_ginis_all <- ncr_tract_ginis_all[, c("geoid", "measure", "value", "year", "moe")]

# GET COUNTIES
if(exists("ncr_county_ginis_all")) rm("ncr_county_ginis_all")
# for each year
for (y in yrs) {
  # for each state
  for (s in c("51", "24", "11")) {
    # Get state counties
    counties <- substr(ncr_counties[ncr_counties %like% paste0("^", s)], 3, 5)
    # Get ACS data
    ncr_county_ginis <- get_acs(geography = "county",
                               variables = "B19083_001",
                               state = s,
                               county = counties,
                               geometry = F,
                               output = "wide",
                               year = y)

    ncr_county_ginis$geoid <- ncr_county_ginis$GEOID
    ncr_county_ginis$measure <- "gini_index"
    ncr_county_ginis$measure_type <- "index"
    ncr_county_ginis$region_name <- ncr_county_ginis$NAME
    ncr_county_ginis$region_type <- "county"
    ncr_county_ginis$value <- ncr_county_ginis$B19083_001E
    ncr_county_ginis$year <- y
    ncr_county_ginis$moe <- ncr_county_ginis$B19083_001M

    if (exists("ncr_county_ginis_all")) {
      ncr_county_ginis_all <- rbindlist(list(ncr_county_ginis_all, ncr_county_ginis))
    } else {
      ncr_county_ginis_all <- ncr_county_ginis
    }
  }
}

ncr_county_ginis_all <- ncr_county_ginis_all[, c("geoid", "measure", "value", "year", "moe")]

# Combine tract and counties
ncr_cttr_income_inequality_gini_index <-
  rbindlist(list(ncr_tract_ginis_all, ncr_county_ginis_all))

#standardize
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
stnd <- standardize_all(ncr_cttr_income_inequality_gini_index)

# Write file
# fwrite(ncr_cttr_2015_2021_income_inequality_gini_index, "Pay and Benefits/Income Inequality/data/distribution/ncr_cttr_2015_2021_income_inequality_gini_index.csv")
readr::write_csv(stnd, xzfile(paste0("Pay and Benefits/Income Inequality/data/distribution/ncr_cttr_", min(yrs), "_", max(yrs), "_income_inequality_gini_index.csv.xz"), compression = 9))
