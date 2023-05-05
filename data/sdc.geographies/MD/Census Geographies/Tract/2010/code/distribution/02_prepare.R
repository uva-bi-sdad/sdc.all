# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_census_tracts <- sf::st_read("MD/Census Geographies/Tract/2010/data/original/us_geo_census_cb_2010_census_tracts.geojson")
md_geo_census_cb_2010_census_tracts <- us_geo_census_cb_2010_census_tracts[substr(us_geo_census_cb_2010_census_tracts$geoid, 1, 2) == "24",]

# # Assign geoid
# md_geo_census_cb_2010_census_tracts$geoid <- ""
#
# # Assign region_type
# md_geo_census_cb_2010_census_tracts$region_type <- "tract"
#
# # Assign region_name
# md_geo_census_cb_2010_census_tracts$region_name <- ""
#
# # Assign year
# md_geo_census_cb_2010_census_tracts$year <- "2010"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- md_geo_census_cb_2010_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(md_geo_census_cb_2010_census_tracts, "MD/Census Geographies/Tract/2010/data/distribution/md_geo_census_cb_2010_census_tracts.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
