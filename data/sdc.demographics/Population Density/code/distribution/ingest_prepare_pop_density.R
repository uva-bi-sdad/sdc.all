library(tidycensus)
library(data.table)
library(sf)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
geos <- c("tract", "county")

if (exists("acsdata")) rm(acsdata)
for (yr in years) {
  for (g in geos) {
    acsyear <- get_acs(year = yr,
                       geography = g,
                       state = "VA",
                       variables = "B01001A_001",
                       geometry = F, 
                       output = "wide"
                       )
    acsyear$year <- yr
    if (!exists("acsdata")) acsdata <- acsyear else acsdata <- rbindlist(list(acsdata, acsyear))
  }
}


aland_tr_20 <- tigris::tracts(state = "VA", year = 2020, cb = T)[, c("GEOID", "ALAND")]
aland_tr_20$geometry <- NULL
aland_ct_20 <- tigris::counties(state = "VA", year = 2020, cb = T)[, c("GEOID", "ALAND")]
aland_ct_20$geometry <- NULL
aland_20 <- rbindlist(list(aland_tr_20, aland_ct_20))

acs_aland_20 <- merge(acsdata, aland_20, by = "GEOID", all.x = TRUE, allow.cartesian = TRUE)

aland_tr_19 <- tigris::tracts(state = "VA", year = 2019, cb = T)[, c("GEOID", "ALAND")]
aland_tr_19$geometry <- NULL

acs_aland_19_20 <- merge(acs_aland_20, aland_tr_19, by = "GEOID", all.x = TRUE, allow.cartesian = TRUE)

acs_aland_19_20$ALAND_MEAN <- rowMeans(acs_aland_19_20[,6:7], na.rm=TRUE)
acs_aland_19_20$ALAND_MEAN_SQMI <- acs_aland_19_20$ALAND_MEAN * 0.0000003861
acs_aland_19_20$pop_dens <- round(acs_aland_19_20$B01001A_001E/acs_aland_19_20$ALAND_MEAN_SQMI, 2)


pop_dens <- acs_aland_19_20[, .(geoid = GEOID, year, measure = "population_density", value = pop_dens, moe = NA)]

# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## get aggrgegate function for health districts
source("https://raw.githubusercontent.com/uva-bi-sdad/sdc.education/main/utils/distribution/aggregate.R")

## convert
stnd <- standardize_all(pop_dens)

# aggregate counties to health districts
hds <- aggregate(stnd[nchar(stnd$geoid)==5,], "county")
stnd <- rbindlist(list(stnd, hds[!nchar(hds$geoid)==5,]), use.names = T)

# save standardized file
write.csv(stnd, file = xzfile("Population Density/data/distribution/va_hdtrct_2015_2023_population_density.csv.xz"), row.names = FALSE)


