# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
dc_geo_census_cb_2010_census_block_groups <- sf::st_read("DC/Census Geographies/Block Group/2010/data/original/dc_geo_census_cb_2010_census_block_groups.geojson")
dc_geo_census_cb_2010_census_block_groups <- sf::st_transform(dc_geo_census_cb_2010_census_block_groups, 4326)

us_geo_census_cb_2010_census_tracts <- sf::st_drop_geometry(sf::st_read("US/Census Geographies/Tract/2010/data/distribution/us_geo_census_cb_2010_census_tracts.geojson"))

dc_geo_census_cb_2010_census_block_groups$geoid_tract <- substr(dc_geo_census_cb_2010_census_block_groups$GEOID10, 1, 11)
dc_geo_census_cb_2010_census_block_groups <- merge(dc_geo_census_cb_2010_census_block_groups, us_geo_census_cb_2010_census_tracts, by.x = "geoid_tract", by.y = "geoid", all.x = T)

# Assign geoid
dc_geo_census_cb_2010_census_block_groups$geoid <- dc_geo_census_cb_2010_census_block_groups$GEOID10

# Assign region_type
dc_geo_census_cb_2010_census_block_groups$region_type <- "block group"

# Assign region_name
dc_geo_census_cb_2010_census_block_groups$region_name <- paste0(dc_geo_census_cb_2010_census_block_groups$NAMELSAD10, ", ", dc_geo_census_cb_2010_census_block_groups$region_name)

# Assign year
dc_geo_census_cb_2010_census_block_groups$year <- "2010"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- dc_geo_census_cb_2010_census_block_groups[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "DC/Census Geographies/Block Group/2010/data/distribution/dc_geo_census_cb_2010_census_block_groups.geojson",
             delete_dsn = TRUE)

# Update file manifest
#data_file_checksums()
