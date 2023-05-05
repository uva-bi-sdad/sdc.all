# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_census_tracts <- sf::st_read("NCR/Census Geographies/Tract/2010/data/original/us_geo_census_cb_2010_census_tracts.geojson")

ncr_counties <- yaml::read_yaml("NCR/ncr_counties.yml")
ncr_geoids <- character()
for (i in 1:length(ncr_counties$ncr_localities)) {
  ncr_geoids <- c(ncr_geoids, ncr_counties$ncr_localities[[i]]$geoid)
}

ncr_geo_census_cb_2010_census_tracts <- us_geo_census_cb_2010_census_tracts[substr(us_geo_census_cb_2010_census_tracts$geoid, 1, 5) %in% ncr_geoids,]

# # Assign geoid
# ncr_geo_census_2010_census_tracts$geoid <- ""
#
# # Assign region_type
# ncr_geo_census_2010_census_tracts$region_type <- "tracts"
#
# # Assign region_name
# ncr_geo_census_2010_census_tracts$region_name <- ""
#
# # Assign year
# ncr_geo_census_2010_census_tracts$year <- "2010"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- ncr_geo_census_2010_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(ncr_geo_census_cb_2010_census_tracts, "NCR/Census Geographies/Tract/2010/data/distribution/ncr_geo_census_cb_2010_census_tracts.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
