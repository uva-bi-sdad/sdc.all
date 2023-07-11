# Calculate the employment rate for census tracts and counties in Virginia:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

yrs <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)

# GET TRACTS
if(exists("va_tract_emprt_all")) rm("va_tract_emprt_all")
# for each year
for (y in yrs) {
  va_tract_emprt <- get_acs(geography = "tract",
                            variables = c("B23025_003", "B23025_004"),
                            state = "VA",
                            geometry = F,
                            output = "wide",
                            year = y)

  va_tract_emprt$geoid <- va_tract_emprt$GEOID
  va_tract_emprt$measure <- "emp_rate"
  va_tract_emprt$measure_type <- "percent"
  va_tract_emprt$region_name <- va_tract_emprt$NAME
  va_tract_emprt$region_type <- "tract"
  va_tract_emprt$value <- round((va_tract_emprt$B23025_004E/va_tract_emprt$B23025_003E) * 100, 2)
  va_tract_emprt$year <- y
  va_tract_emprt$moe <- ""

  if (exists("va_tract_emprt_all")) {
    va_tract_emprt_all <- rbindlist(list(va_tract_emprt_all, va_tract_emprt))
  } else {
    va_tract_emprt_all <- va_tract_emprt
  }
}

va_tract_emprt_all <- va_tract_emprt_all[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year", "moe")]

# GET COUNTIES
if(exists("va_county_emprt_all")) rm("va_county_emprt_all")
# for each year
for (y in yrs) {
  va_county_emprt <- get_acs(geography = "county",
                             variables = c("B23025_003", "B23025_004"),
                             state = "VA",
                             # county = c("ARLINGTON", "FAIRFAX COUNTY"),
                             geometry = F,
                             output = "wide",
                             year = y)

  va_county_emprt$geoid <- va_county_emprt$GEOID
  va_county_emprt$measure <- "emp_rate"
  va_county_emprt$measure_type <- "percent"
  va_county_emprt$region_name <- va_county_emprt$NAME
  va_county_emprt$region_type <- "county"
  va_county_emprt$value <- round((va_county_emprt$B23025_004E/va_county_emprt$B23025_003E) * 100, 2)
  va_county_emprt$year <- y
  va_county_emprt$moe <- ""

  if (exists("va_county_emprt_all")) {
    va_county_emprt_all <- rbindlist(list(va_county_emprt_all, va_county_emprt))
  } else {
    va_county_emprt_all <- va_county_emprt
  }
}

va_county_emprt_all <- va_county_emprt_all[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year", "moe")]

# Combine tract and counties
va_cttr_2015_2021_employment_rate <-
  rbindlist(list(va_tract_emprt_all, va_county_emprt_all))

# Write file
# fwrite(va_cttr_2015_2021_employment_rate, "Employment/data/distribution/va_cttr_2015_2021_employment_rate.csv")
readr::write_csv(va_cttr_2015_2021_employment_rate, xzfile("Employment/data/distribution/va_cttr_2015_2021_employment_rate.csv.xz", compression = 9))

