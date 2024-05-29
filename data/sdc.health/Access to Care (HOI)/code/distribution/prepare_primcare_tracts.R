library(data.table)
library(tidygeocoder)
library(sf)
library(readr)
library(dplyr)

# data
va_prim <- setDT(read_csv("Access to Care (HOI)/data/working/va_2017_2019_primary_care_phys.csv"))
va_prim[, address := paste0(Rndrng_Prvdr_St1, ", ", Rndrng_Prvdr_City, ", VA")]

nrow(unique(va_prim[, .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_City, year)]))

# Get Addresses

addresses_unq <- unique(va_prim$address)

# Geocode Unique Addresses
cascade_results1 <- addresses_unq %>%
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

# Join geocodes to dataset
va_prim_geo <- merge(va_prim, cascade_results1, by = c("Rndrng_NPI", "address"), all.x = T)
va_prim_geo <- va_prim_geo[!is.na(lat) & !is.na(long)]


# We will use the st_intersects function from the sf library and the county-level sf shapefiles to find 
# in which census tract a given lat/long pair is contained. To do that, we convert the latitude and longitude 
# data into an sf object. Note that if you have NA values for the latitude or longitude, you’ll need to filter them out.
#census_tracts_19 <- tigris::tracts(state = "VA", cb = TRUE, year = 2019)
#latlong_sf_19 <- cascade_results1 %>%
#  st_as_sf(coords = c("long", "lat"), crs = st_crs(census_tracts_19))

for (y in c("17", "18", "19", "20", "21")) {
  assign(paste0("census_tracts_", y), value = tigris::tracts(state = "VA", cb = TRUE, year = paste0("20", y)))
}

for (y in c("17", "18", "19", "20", "21")) {
  assign(paste0("latlong_sf_", y), value = st_as_sf(va_prim_geo[year==paste0("20", y)],
                                                    coords = c("long", "lat"),
                                                    crs = st_crs(get(paste0("census_tracts_", y)))))
}


# Then, we use the st_intersects function to find the tracts which intersect with the points. 
# This returns a list, where the ith element is the row number of census_tracts which contains the ith lat/long pair.
#intersected_19 <- st_intersects(latlong_sf_19, census_tracts_19)
# latlong_final_19 <- latlong_sf_19 %>%
#   mutate(intersection = as.integer(intersected_19),
#          fips = if_else(is.na(intersection), "",
#                         census_tracts$GEOID[intersection]))

for (y in c("17", "18", "19", "20", "21")) {
  assign(paste0("intersected_", y), value = st_intersects(get(paste0("latlong_sf_", y)), get(paste0("census_tracts_", y))))
}
  
for (y in c("17", "18", "19", "20", "21")) {
  assign(paste0("latlong_final_", y),
         value = get(paste0("latlong_sf_", y)) %>%
           mutate(intersection = as.integer(get(paste0("intersected_", y))),
                  fips = if_else(is.na(intersection), "",
                                 get(paste0("census_tracts_", y))$GEOID[intersection])))
  
}

# Assemble Final Dataset
if (exists("va_prim_geo_tr")) rm(va_prim_geo_tr)
for (y in c("17", "18", "19", "20", "21")) {
  dt <- get(paste0("latlong_final_", y))
  if (!exists("va_prim_geo_tr")) va_prim_geo_tr <- dt else va_prim_geo_tr <- rbindlist(list(va_prim_geo_tr, dt))
}
va_prim_geo_tr <- va_prim_geo_tr[, .(Rndrng_NPI, 
                                     Year = year, 
                                     Rndrng_Prvdr_Crdntls, 
                                     Rndrng_Prvdr_Addr = address, 
                                     Rndrng_Prvdr_Type, 
                                     Rndrng_Prvdr_RUCA, Tract_FIPS = fips)]

# Write File
fwrite(va_prim_geo_tr, "Access to Care (HOI)/data/working/va_2017_2021_primary_care_phys_by_tract.csv")


