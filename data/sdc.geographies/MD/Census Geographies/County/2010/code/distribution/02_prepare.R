# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_counties <- sf::st_read("data/md_geo_census_cb_2010_counties/original/us_geo_census_cb_2010_counties.geojson")
md_geo_census_cb_2010_counties <- us_geo_census_cb_2010_counties[substr(us_geo_census_cb_2010_counties$geoid, 1, 2) == "24",]

# # Assign geoid
# md_geo_census_cb_2010_counties$geoid <- ""
#
# # Assign region_type
# md_geo_census_cb_2010_counties$region_type <- "county"
#
# # Assign region_name
# md_geo_census_cb_2010_counties$region_name <- ""
#
# # Assign year
# md_geo_census_cb_2010_counties$year <- "2010"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- md_geo_census_cb_2010_counties[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(md_geo_census_cb_2010_counties, "data/md_geo_census_cb_2010_counties/distribution/md_geo_census_cb_2010_counties.geojson")

# Update file manifest
data_file_checksums()
