# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
unzip("DC/Census Geographies/Tract/2020/data/original/dc_geo_census_cb_2020_census_tracts.zip", exdir = "DC/Census Geographies/Tract/2020/data/original/dc_geo_census_cb_2020_census_tracts")
dc_geo_census_cb_2020_census_tracts <- sf::st_read("DC/Census Geographies/Tract/2020/data/original/dc_geo_census_cb_2020_census_tracts/cb_2020_11_tract_500k.shp")
dc_geo_census_cb_2020_census_tracts <- sf::st_transform(dc_geo_census_cb_2020_census_tracts, 4326)

unlink("DC/Census Geographies/Tract/2020/data/original/dc_geo_census_cb_2020_census_tracts", recursive = T)

# Assign geoid
dc_geo_census_cb_2020_census_tracts$geoid <- dc_geo_census_cb_2020_census_tracts$GEOID

# Assign region_type
dc_geo_census_cb_2020_census_tracts$region_type <- "tract"

# Assign region_name
dc_geo_census_cb_2020_census_tracts$region_name <-
  paste0(
    dc_geo_census_cb_2020_census_tracts$NAMELSAD,
    ", ",
    dc_geo_census_cb_2020_census_tracts$NAMELSADCO,
    ", ",
    dc_geo_census_cb_2020_census_tracts$STATE_NAME
  )

# Assign year
dc_geo_census_cb_2020_census_tracts$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- dc_geo_census_cb_2020_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "DC/Census Geographies/Tract/2020/data/distribution/dc_geo_census_cb_2020_census_tracts.geojson",
             delete_dsn = TRUE)

# Update file manifest
#data_file_checksums()
