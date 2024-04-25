# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2020_states <- sf::st_read("US/Census Geographies/States/2020/data/original/us_geo_census_cb_2020_states.geojson")
us_geo_census_cb_2020_states <- sf::st_transform(us_geo_census_cb_2020_states, 4326)


# Assign geoid
us_geo_census_cb_2020_states$geoid <- us_geo_census_cb_2020_states$GEOID

# Assign region_type
us_geo_census_cb_2020_states$region_type <- "state"

# Assign region_name
us_geo_census_cb_2020_states$region_name <- stringr::str_to_title(us_geo_census_cb_2020_states$NAME)

# Assign year
us_geo_census_cb_2020_states$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- us_geo_census_cb_2020_states[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "US/Census Geographies/States/2020/data/distribution/us_geo_census_cb_2020_states.geojson",
             delete_dsn = TRUE)

# Update file manifest
#data_file_checksums()
