# Calculate the GINI coefficient for census tracts in Virginia:
# library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

source('utils/distribution/aggregate.R')

yrs <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)

# GET TRACTS
if(exists("va_tract_ginis_all")) rm("va_tract_ginis_all")
# for each year
for (y in yrs) {
  va_tract_ginis <- get_acs(geography = "tract",
                            variables = "B19083_001",
                            state = "VA",
                            # county = c("ARLINGTON", "FAIRFAX COUNTY"),
                            geometry = F,
                            output = "wide",
                            year = y)

  va_tract_ginis$geoid <- va_tract_ginis$GEOID
  va_tract_ginis$measure <- "gini_index"
  va_tract_ginis$measure_type <- "index"
  va_tract_ginis$region_name <- va_tract_ginis$NAME
  va_tract_ginis$region_type <- "tract"
  va_tract_ginis$value <- va_tract_ginis$B19083_001E
  va_tract_ginis$year <- y
  va_tract_ginis$moe <- va_tract_ginis$B19083_001M

  if (exists("va_tract_ginis_all")) {
    va_tract_ginis_all <- rbindlist(list(va_tract_ginis_all, va_tract_ginis))
  } else {
    va_tract_ginis_all <- va_tract_ginis
  }
}

va_tract_ginis_all <- va_tract_ginis_all[, c("geoid", "measure", "value", "year", "moe")]

# GET COUNTIES
if(exists("va_county_ginis_all")) rm("va_county_ginis_all")
# for each year
for (y in yrs) {
  va_county_ginis <- get_acs(geography = "county",
                            variables = "B19083_001",
                            state = "VA",
                            # county = c("ARLINGTON", "FAIRFAX COUNTY"),
                            geometry = F,
                            output = "wide",
                            year = y)

  va_county_ginis$geoid <- va_county_ginis$GEOID
  va_county_ginis$measure <- "gini_index"
  va_county_ginis$measure_type <- "index"
  va_county_ginis$region_name <- va_county_ginis$NAME
  va_county_ginis$region_type <- "county"
  va_county_ginis$value <- va_county_ginis$B19083_001E
  va_county_ginis$year <- y
  va_county_ginis$moe <- va_county_ginis$B19083_001M

  if (exists("va_county_ginis_all")) {
    va_county_ginis_all <- rbindlist(list(va_county_ginis_all, va_county_ginis))
  } else {
    va_county_ginis_all <- va_county_ginis
  }
}

va_county_ginis_all <- va_county_ginis_all[, c("geoid", "measure", "value", "year", "moe")]
county_healthdis <- aggregate(va_county_ginis_all, "county", weight_col = "B01003_001E")

# Combine
combined <- rbind(va_tract_ginis_all, county_healthdis)

# Write file
# fwrite(va_cttr_2015_2021_income_inequality_gini_index, "Pay and Benefits/Income Inequality/data/distribution/va_cttr_2015_2021_income_inequality_gini_index.csv")
readr::write_csv(combined, xzfile("Pay and Benefits/Income Inequality/data/distribution/va_hdcttr_2015_2021_income_inequality_gini_index.csv.xz", compression = 9))

# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## convert
rcsv <- read.csv(xzfile("Pay and Benefits/Income Inequality/data/distribution/va_hdcttr_2015_2021_income_inequality_gini_index.csv.xz", open = "r"))
stnd <- standardize_all(rcsv)

# save standardized file
write.csv(stnd, file = xzfile("Pay and Benefits/Income Inequality/data/distribution/va_hdcttr_2015_2021_income_inequality_gini_index_std.csv.xz"), row.names = FALSE)

