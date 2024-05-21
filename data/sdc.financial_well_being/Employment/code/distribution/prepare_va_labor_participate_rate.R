# To obtain all the variables in ACS
library(tidycensus)
library(tidyverse)
var_acs_20 <- load_variables(2020, "acs5/subject", cache = TRUE) %>%
  filter(str_detect(label, "Labor Force Participation Rate"))

# To use: S2301_C02_001 = Estimate!!Labor Force Participation Rate!!Population 16 years and over


# Calculate the labor participation rate for census tracts, counties and health districts in Virginia:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)

yrs <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)


# GET TRACTS
if(exists("va_tract_partic_rt_all")) rm("va_tract_partic_rt_all")
# for each year
for (y in yrs) {
  va_tract_partic_rt <- get_acs(geography = "tract",
                                variables = c("S2301_C02_001"),
                                state = "VA",
                                geometry = F,
                                output = "wide",
                                year = y)

  va_tract_partic_rt$geoid <- va_tract_partic_rt$GEOID
  va_tract_partic_rt$measure <- "labor_participate_rate"
  va_tract_partic_rt$measure_type <- "percent"
  va_tract_partic_rt$region_name <- va_tract_partic_rt$NAME
  va_tract_partic_rt$region_type <- "tract"
  va_tract_partic_rt$year <- y
  va_tract_partic_rt$value <- va_tract_partic_rt$S2301_C02_001E
  va_tract_partic_rt$moe <- va_tract_partic_rt$S2301_C02_001M

  if (exists("va_tract_partic_rt_all")) {
    va_tract_partic_rt_all <- rbindlist(list(va_tract_partic_rt_all, va_tract_partic_rt))
  } else {
    va_tract_partic_rt_all <- va_tract_partic_rt
  }
}

va_tract_partic_rt_all <- va_tract_partic_rt_all[, c("geoid", "measure", "value", "year", "moe")]

# GET COUNTIES
if(exists("va_county_partic_rt_all")) rm("va_county_partic_rt_all")
# for each year
for (y in yrs) {
  va_county_partic_rt <- get_acs(geography = "county",
                                 variables = c("S2301_C02_001"),
                                 state = "VA",
                                 geometry = F,
                                 output = "wide",
                                 year = y)

  va_county_partic_rt$geoid <- va_county_partic_rt$GEOID
  va_county_partic_rt$measure <- "labor_participate_rate"
  va_county_partic_rt$measure_type <- "percent"
  va_county_partic_rt$region_name <- va_county_partic_rt$NAME
  va_county_partic_rt$region_type <- "county"
  va_county_partic_rt$year <- y
  va_county_partic_rt$value <- va_county_partic_rt$S2301_C02_001E
  va_county_partic_rt$moe <- va_county_partic_rt$S2301_C02_001M

  if (exists("va_county_partic_rt_all")) {
    va_county_partic_rt_all <- rbindlist(list(va_county_partic_rt_all, va_county_partic_rt))
  } else {
    va_county_partic_rt_all <- va_county_partic_rt
  }
}

va_county_partic_rt_all <- va_county_partic_rt_all[, c("geoid", "measure", "value", "year", "moe")]


#### Aggregate to Health Districts for Affordability Index and combine datasets to get hdcttr levels in a single dataset ####

source("~/git/sdc.financial_well_being_dev/utils/distribution/aggregate.R")


# Using aggregate function (found in utils folder) to aggregate from county to health district
# Aggregate function output includes hd and ct levels
va_hdct_partic_rt_all <- aggregate(va_county_partic_rt_all, "county", weight_col = "B01003_001E")


# Combine hdct with tr level
va_hdcttr_partic_rt_all <- rbind(va_hdct_partic_rt_all, va_tract_partic_rt_all)

write.csv(va_hdcttr_partic_rt_all, file = xzfile("~/git/sdc.financial_well_being_dev/Employment/data/distribution/va_hdcttr_2015_2022_labor_participate_rate.csv.xz"), row.names = FALSE)

# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## convert
rcsv <- read.csv(xzfile("Employment/data/distribution/va_hdcttr_2015_2022_labor_participate_rate.csv.xz", open = "r"))
stnd <- standardize_all(rcsv)

# save standardized file
write.csv(stnd, file = xzfile("Employment/data/distribution/va_hdcttr_2015_2022_labor_participate_rate_std.csv.xz"), row.names = FALSE)





