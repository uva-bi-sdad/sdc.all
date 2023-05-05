# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_census_tracts <- sf::st_read("DC/Census Geographies/Tract/2010/data/original/us_geo_census_cb_2010_census_tracts.geojson")
dc_geo_census_cb_2010_census_tracts <- us_geo_census_cb_2010_census_tracts[substr(us_geo_census_cb_2010_census_tracts$geoid, 1, 2) == "11",]

# # Assign geoid
# dc_geo_census_cb_2010_census_tracts$geoid <- ""
#
# # Assign region_type
# dc_geo_census_cb_2010_census_tracts$region_type <- "tract"
#
# # Assign region_name
# dc_geo_census_cb_2010_census_tracts$region_name <- ""
#
# # Assign year
# dc_geo_census_cb_2010_census_tracts$year <- "2010"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- dc_geo_census_cb_2010_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(dc_geo_census_cb_2010_census_tracts, "DC/Census Geographies/Tract/2010/data/distribution/dc_geo_census_cb_2010_census_tracts.geojson",
             delete_dsn = TRUE)

# Update file manifest
#data_file_checksums()
