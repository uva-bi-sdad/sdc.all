# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_census_tracts <- sf::st_read("data/va_geo_census_cb_2010_census_tracts/original/us_geo_census_cb_2010_census_tracts.geojson")
va_geo_census_cb_2010_census_tracts <- us_geo_census_cb_2010_census_tracts[substr(us_geo_census_cb_2010_census_tracts$geoid, 1, 2) == "51",]

# # Assign geoid
# va_geo_census_cb_2010_census_tracts$geoid <- substr(va_geo_census_cb_2010_census_tracts$GEO_ID, 10, 20)
#
# # Assign region_type
# va_geo_census_cb_2010_census_tracts$region_type <- "tract"
#
# # Assign region_name
#
# # Assign year
# va_geo_census_cb_2010_census_tracts$year <- "2010"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- va_geo_census_cb_2010_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(va_geo_census_cb_2010_census_tracts, "data/va_geo_census_cb_2010_census_tracts/distribution/va_geo_census_cb_2010_census_tracts.geojson")

# Update file manifest
data_file_checksums()
