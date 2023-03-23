# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_counties <- sf::st_read("DC/Census\ Geographies/2010/County/data/original/us_geo_census_cb_2010_counties.geojson")
dc_geo_census_cb_2010_counties <- us_geo_census_cb_2010_counties[substr(us_geo_census_cb_2010_counties$geoid, 1, 2) == "11",]

# # Assign geoid
# dc_geo_census_cb_2010_counties$geoid <- ""
#
# # Assign region_type
# dc_geo_census_cb_2010_counties$region_type <- "county"
#
# # Assign region_name
# dc_geo_census_cb_2010_counties$region_name <- ""
#
# # Assign year
# dc_geo_census_cb_2010_counties$year <- "2010"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- dc_geo_census_cb_2010_counties[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(dc_geo_census_cb_2010_counties, "DC/Census\ Geographies/2010/County/data/distribution/dc_geo_census_cb_2010_counties.geojson")

# Update file manifest
data_file_checksums()
