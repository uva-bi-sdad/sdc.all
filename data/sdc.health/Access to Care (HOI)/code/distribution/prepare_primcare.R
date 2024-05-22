library(data.table)
library(tidygeocoder)
library(sf)
library(readr)
library(dplyr)

# data
va_prim <- setDT(read_csv("Access to Care (HOI)/data/working/va_2017_2019_primary_care_phys.csv"))

nrow(unique(va_prim[, .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_City, year)]))

# Get Addresses
addresses <- va_prim[, .(Rndrng_NPI, address = paste0(Rndrng_Prvdr_St1, ", ", Rndrng_Prvdr_City, ", VA"))]
addresses_unq <- unique(addresses)

cascade_results1 <- addresses_unq[1:20] %>%
  geocode_combine(
    queries = list(
      list(method = 'census'),
      list(method = 'osm'),
      list(method = 'google')
    ),
    global_params = list(address = 'address')
  )

setDT(cascade_results1)
cascade_results1 <- cascade_results1[!is.na(lat) & !is.na(long)]

# # Function to get admin areas from lat lon
# geo2fips <- function(latitude, longitude) { 
#   url <- "https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json"
#   res <- jsonlite::fromJSON(sprintf(url, latitude, longitude))[["results"]][["block_fips"]][[1]]
#   #browser()
#   unique(res)
# }
# 
# geo2fips(cascade_results1[1,]$lat, cascade_results1[1,]$long)


# We will use the st_intersects function from the sf library and the county-level sf shapefiles to find 
# in which census tract a given lat/long pair is contained. To do that, we convert the latitude and longitude 
# data into an sf object. Note that if you have NA values for the latitude or longitude, you’ll need to filter them out.
census_tracts_19 <- tigris::tracts(state = "VA", cb = TRUE, year = 2019)

latlong_sf_19 <- cascade_results1 %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(census_tracts_19))

# Then, we use the st_intersects function to find the counties which intersect with the points. 
# This returns a list, where the ith element is the row number of census_tracts which contains the ith lat/long pair.
intersected_19 <- st_intersects(latlong_sf_19, census_tracts_19)

latlong_final_19 <- latlong_sf_19 %>%
  mutate(intersection = as.integer(intersected_19),
         fips = if_else(is.na(intersection), "",
                        census_tracts$GEOID[intersection]))
head(latlong_final_19)

# Then again for 2020 geographies
census_tracts_21 <- tigris::tracts(state = "VA", cb = TRUE, year = 2021)

latlong_sf_21 <- cascade_results1 %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(census_tracts_21))

intersected_21 <- st_intersects(latlong_sf_21, census_tracts_21)

latlong_final_21 <- latlong_sf_21 %>%
  mutate(intersection = as.integer(intersected_21),
         fips = if_else(is.na(intersection), "",
                        census_tracts$GEOID[intersection]))

head(latlong_final_21)




