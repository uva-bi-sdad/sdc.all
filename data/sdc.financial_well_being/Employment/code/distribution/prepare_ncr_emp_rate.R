#
#
# acs_data <- get_acs(geography = "tract",
#                            variables = c("B23025_001", "B23025_002", "B23025_003", "B23025_004", "B23025_005"),
#                            state = "VA",
#                            # county = c("ARLINGTON", "FAIRFAX COUNTY"),
#                            geometry = F,
#                            output = "wide",
#                            year = 2019)
#
# # unemployment rate
# (acs_data$B23025_005E/acs_data$B23025_003E) * 100
#
# # job participation rate
# (acs_data$B23025_004E + acs_data$B23025_005E) / acs_data$B23025_001E


# Calculate the employment rate for census tracts and counties in the NCR:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

yrs <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)
ncr_counties <- c("51013" , "51059" , "51600" , "51107" , "51610" , "51683", "51685" , "51153" , "51510" , "24021" , "24031" , "24033" , "24017", "11001")

# GET TRACTS
if(exists("ncr_tract_emprt_all")) rm("ncr_tract_emprt_all")
# for each year
for (y in yrs) {
  ##for each state
  for (s in c("51", "24", "11")) {
    # Get state counties
    counties <- substr(ncr_counties[ncr_counties %like% paste0("^", s)], 3, 5)
    # Get ACS data
    ncr_tract_emprt <- get_acs(geography = "tract",
                               variables = c("B23025_003", "B23025_004"),
                               state = s,
                               county = counties,
                               geometry = F,
                               output = "wide",
                               year = y)

    ncr_tract_emprt$geoid <- ncr_tract_emprt$GEOID
    ncr_tract_emprt$measure <- "emp_rate"
    ncr_tract_emprt$measure_type <- "percent"
    ncr_tract_emprt$region_name <- ncr_tract_emprt$NAME
    ncr_tract_emprt$region_type <- "tract"
    ncr_tract_emprt$value <- round((ncr_tract_emprt$B23025_004E/ncr_tract_emprt$B23025_003E) * 100, 2)
    ncr_tract_emprt$year <- y
    ncr_tract_emprt$moe <- ""

    if (exists("ncr_tract_emprt_all")) {
      ncr_tract_emprt_all <- rbindlist(list(ncr_tract_emprt_all, ncr_tract_emprt))
    } else {
      ncr_tract_emprt_all <- ncr_tract_emprt
    }
  }
}

ncr_tract_emprt_all <- ncr_tract_emprt_all[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year", "moe")]

# GET COUNTIES
if(exists("ncr_county_emprt_all")) rm("ncr_county_emprt_all")
# for each year
for (y in yrs) {
  # for each state
  for (s in c("51", "24", "11")) {
    # Get state counties
    counties <- substr(ncr_counties[ncr_counties %like% paste0("^", s)], 3, 5)
    # Get ACS data
    ncr_county_emprt <- get_acs(geography = "county",
                                variables = c("B23025_003", "B23025_004"),
                                state = s,
                                county = counties,
                                geometry = F,
                                output = "wide",
                                year = y)

    ncr_county_emprt$geoid <- ncr_county_emprt$GEOID
    ncr_county_emprt$measure <- "emp_rate"
    ncr_county_emprt$measure_type <- "percent"
    ncr_county_emprt$region_name <- ncr_county_emprt$NAME
    ncr_county_emprt$region_type <- "county"
    ncr_county_emprt$value <- round((ncr_county_emprt$B23025_004E/ncr_county_emprt$B23025_003E) * 100, 2)
    ncr_county_emprt$year <- y
    ncr_county_emprt$moe <- ""

    if (exists("ncr_county_emprt_all")) {
      ncr_county_emprt_all <- rbindlist(list(ncr_county_emprt_all, ncr_county_emprt))
    } else {
      ncr_county_emprt_all <- ncr_county_emprt
    }
  }
}

ncr_county_emprt_all <- ncr_county_emprt_all[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year", "moe")]

# Combine tract and counties
ncr_cttr_2015_2021_employment_rate <-
  rbindlist(list(ncr_tract_emprt_all, ncr_county_emprt_all))

# Write file
# fwrite(ncr_cttr_2015_2021_employment_rate, "Employment/data/distribution/ncr_cttr_2015_2021_employment_rate.csv")
readr::write_csv(ncr_cttr_2015_2021_employment_rate, xzfile("Employment/data/distribution/ncr_cttr_2015_2021_employment_rate.csv.xz", compression = 9))

