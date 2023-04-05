# Calculate the job participation rate for census tracts and counties in Virginia:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

yrs <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)

# GET TRACTS
if(exists("va_tract_partic_rt_all")) rm("va_tract_partic_rt_all")
# for each year
for (y in yrs) {
  va_tract_partic_rt <- get_acs(geography = "tract",
                            variables = c("B23025_001","B23025_004", "B23025_005"),
                            state = "VA",
                            geometry = F,
                            output = "wide",
                            year = y)

  va_tract_partic_rt$geoid <- va_tract_partic_rt$GEOID
  va_tract_partic_rt$measure <- "job_participate_rate"
  va_tract_partic_rt$measure_type <- "percent"
  va_tract_partic_rt$region_name <- va_tract_partic_rt$NAME
  va_tract_partic_rt$region_type <- "tract"
  va_tract_partic_rt$value <- round(((va_tract_partic_rt$B23025_004E + va_tract_partic_rt$B23025_005E) / va_tract_partic_rt$B23025_001E) * 100, 2)
  va_tract_partic_rt$year <- y
  va_tract_partic_rt$moe <- ""

  if (exists("va_tract_partic_rt_all")) {
    va_tract_partic_rt_all <- rbindlist(list(va_tract_partic_rt_all, va_tract_partic_rt))
  } else {
    va_tract_partic_rt_all <- va_tract_partic_rt
  }
}

va_tract_partic_rt_all <- va_tract_partic_rt_all[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year", "moe")]

# GET COUNTIES
if(exists("va_county_partic_rt_all")) rm("va_county_partic_rt_all")
# for each year
for (y in yrs) {
  va_county_partic_rt <- get_acs(geography = "county",
                             variables = c("B23025_001","B23025_004", "B23025_005"),
                             state = "VA",
                             geometry = F,
                             output = "wide",
                             year = y)

  va_county_partic_rt$geoid <- va_county_partic_rt$GEOID
  va_county_partic_rt$measure <- "job_participate_rate"
  va_county_partic_rt$measure_type <- "percent"
  va_county_partic_rt$region_name <- va_county_partic_rt$NAME
  va_county_partic_rt$region_type <- "county"
  va_county_partic_rt$value <- round(((va_county_partic_rt$B23025_004E + va_county_partic_rt$B23025_005E) / va_county_partic_rt$B23025_001E) * 100, 2)
  va_county_partic_rt$year <- y
  va_county_partic_rt$moe <- ""

  if (exists("va_county_partic_rt_all")) {
    va_county_partic_rt_all <- rbindlist(list(va_county_partic_rt_all, va_county_partic_rt))
  } else {
    va_county_partic_rt_all <- va_county_partic_rt
  }
}

va_county_partic_rt_all <- va_county_partic_rt_all[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year", "moe")]

# Combine tract and counties
va_cttr_2015_2021_job_participate_rate <-
  rbindlist(list(va_tract_partic_rt_all, va_county_partic_rt_all))

# Write file
fwrite(va_cttr_2015_2021_job_participate_rate, "Employment/data/distribution/va_cttr_2015_2021_job_participate_rate.csv")


