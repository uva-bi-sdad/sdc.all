# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
unzip("./US/Census Geographies/Tract/2020/data/original/us_geo_census_cb_2020_census_tracts.zip", exdir = "./US/Census Geographies/Tract/2020/data/original/us_geo_census_cb_2020_census_tracts")
us_geo_census_cb_2020_census_tracts <- sf::st_read("./US/Census Geographies/Tract/2020/data/original/us_geo_census_cb_2020_census_tracts/cb_2020_us_tract_500k.shp")
us_geo_census_cb_2020_census_tracts <- sf::st_transform(us_geo_census_cb_2020_census_tracts, 4326)

unlink("./US/Census Geographies/Tract/2020/data/original/us_geo_census_cb_2020_census_tracts", recursive = T)

# Assign geoid
us_geo_census_cb_2020_census_tracts$geoid <- us_geo_census_cb_2020_census_tracts$GEOID

# Assign region_type
us_geo_census_cb_2020_census_tracts$region_type <- "tract"

# Assign region_name
us_geo_census_cb_2020_census_tracts$region_name <-
  stringr::str_to_title(
    paste0(
      us_geo_census_cb_2020_census_tracts$NAMELSAD,
      ", ",
      us_geo_census_cb_2020_census_tracts$NAMELSADCO,
      ", ",
      us_geo_census_cb_2020_census_tracts$STATE_NAME
    )
  )

# Assign year
us_geo_census_cb_2020_census_tracts$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- us_geo_census_cb_2020_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "./US/Census Geographies/Tract/2020/data/distribution/us_geo_census_cb_2020_census_tracts.geojson",
             delete_dsn = TRUE)

# Update file manifest
#data_file_checksums()
