# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2020_counties <- sf::st_read("data/us_geo_census_cb_2020_counties/original/us_geo_census_cb_2020_counties.geojson")
us_geo_census_cb_2020_counties <- sf::st_transform(us_geo_census_cb_2020_counties, 4326)


# Assign geoid
us_geo_census_cb_2020_counties$geoid <- us_geo_census_cb_2020_counties$GEOID

# Assign region_type
us_geo_census_cb_2020_counties$region_type <- "county"

# Assign region_name
us_geo_census_cb_2020_counties$region_name <- stringr::str_to_title(paste0(us_geo_census_cb_2020_counties$NAMELSAD, ", ", us_geo_census_cb_2020_counties$STATE_NAME))

# Assign year
us_geo_census_cb_2020_counties$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- us_geo_census_cb_2020_counties[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
#final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(final_dataset, "data/us_geo_census_cb_2020_counties/distribution/us_geo_census_cb_2020_counties.geojson")

# Update file manifest
data_file_checksums()
