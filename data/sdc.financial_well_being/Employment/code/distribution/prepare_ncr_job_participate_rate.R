# Calculate the job participation rate for census tracts and counties in the NCR:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

yrs <- 2015:2023
ncr_counties <- c("51013" , "51059" , "51600" , "51107" , "51610" , "51683", "51685" , "51153" , "51510" , "24021" , "24031" , "24033" , "24017", "11001")

# GET TRACTS
if(exists("ncr_tract_partic_rt_all")) rm("ncr_tract_partic_rt_all")
# for each year
for (y in yrs) {
  ##for each state
  for (s in c("51", "24", "11")) {
    # Get state counties
    counties <- substr(ncr_counties[ncr_counties %like% paste0("^", s)], 3, 5)
    # Get ACS data
    ncr_tract_partic_rt <- get_acs(geography = "tract",
                               variables = c("B23025_001","B23025_004", "B23025_005"),
                               state = s,
                               county = counties,
                               geometry = F,
                               output = "wide",
                               year = y)

    ncr_tract_partic_rt$geoid <- ncr_tract_partic_rt$GEOID
    ncr_tract_partic_rt$measure <- "job_participate_rate"
    ncr_tract_partic_rt$measure_type <- "percent"
    ncr_tract_partic_rt$region_name <- ncr_tract_partic_rt$NAME
    ncr_tract_partic_rt$region_type <- "tract"
    ncr_tract_partic_rt$value <- round(((ncr_tract_partic_rt$B23025_004E + ncr_tract_partic_rt$B23025_005E) / ncr_tract_partic_rt$B23025_001E) * 100, 2)
    ncr_tract_partic_rt$year <- y
    ncr_tract_partic_rt$moe <- ""

    if (exists("ncr_tract_partic_rt_all")) {
      ncr_tract_partic_rt_all <- rbindlist(list(ncr_tract_partic_rt_all, ncr_tract_partic_rt))
    } else {
      ncr_tract_partic_rt_all <- ncr_tract_partic_rt
    }
  }
}

ncr_tract_partic_rt_all <- ncr_tract_partic_rt_all[, c("geoid", "year", "measure", "value", "moe")]

# GET COUNTIES
if(exists("ncr_county_partic_rt_all")) rm("ncr_county_partic_rt_all")
# for each year
for (y in yrs) {
  # for each state
  for (s in c("51", "24", "11")) {
    # Get state counties
    counties <- substr(ncr_counties[ncr_counties %like% paste0("^", s)], 3, 5)
    # Get ACS data
    ncr_county_partic_rt <- get_acs(geography = "county",
                                variables = c("B23025_001","B23025_004", "B23025_005"),
                                state = s,
                                county = counties,
                                geometry = F,
                                output = "wide",
                                year = y)

    ncr_county_partic_rt$geoid <- ncr_county_partic_rt$GEOID
    ncr_county_partic_rt$measure <- "labor_participate_rate_geo20"
    ncr_county_partic_rt$measure_type <- "percent"
    ncr_county_partic_rt$region_name <- ncr_county_partic_rt$NAME
    ncr_county_partic_rt$region_type <- "county"
    ncr_county_partic_rt$value <- round(((ncr_county_partic_rt$B23025_004E + ncr_county_partic_rt$B23025_005E) / ncr_county_partic_rt$B23025_001E) * 100, 2)
    ncr_county_partic_rt$year <- y
    ncr_county_partic_rt$moe <- ""

    if (exists("ncr_county_partic_rt_all")) {
      ncr_county_partic_rt_all <- rbindlist(list(ncr_county_partic_rt_all, ncr_county_partic_rt))
    } else {
      ncr_county_partic_rt_all <- ncr_county_partic_rt
    }
  }
}

ncr_county_partic_rt_all <- ncr_county_partic_rt_all[, c("geoid", "year", "measure", "value", "moe")]

# Combine tract and counties
ncr_cttr_job_participate_rate <-
  rbindlist(list(ncr_tract_partic_rt_all, ncr_county_partic_rt_all))

# Write file
readr::write_csv(ncr_cttr_job_participate_rate, xzfile(paste0("Employment/data/distribution/ncr_cttr_", min(yrs), "_", max(yrs), "_job_participate_rate.csv.xz"), compression=9))

