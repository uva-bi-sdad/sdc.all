# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
va_geo_census_cb_2010_census_block_groups <- sf::st_read("data/va_geo_census_cb_2010_census_block_groups/original/va_geo_census_cb_2010_census_block_groups.geojson")
va_geo_census_cb_2010_census_block_groups <- sf::st_transform(va_geo_census_cb_2010_census_block_groups, 4326)

us_geo_census_cb_2010_census_tracts <- sf::st_drop_geometry(sf::st_read("data/us_geo_census_cb_2010_census_tracts/distribution/us_geo_census_cb_2010_census_tracts.geojson"))

va_geo_census_cb_2010_census_block_groups$geoid_tract <- substr(va_geo_census_cb_2010_census_block_groups$GEOID10, 1, 11)
va_geo_census_cb_2010_census_block_groups <- merge(va_geo_census_cb_2010_census_block_groups, us_geo_census_cb_2010_census_tracts, by.x = "geoid_tract", by.y = "geoid", all.x = T)

# Assign geoid
va_geo_census_cb_2010_census_block_groups$geoid <- va_geo_census_cb_2010_census_block_groups$GEOID10

# Assign region_type
va_geo_census_cb_2010_census_block_groups$region_type <- "block group"

# Assign region_name
va_geo_census_cb_2010_census_block_groups$region_name <- paste0(va_geo_census_cb_2010_census_block_groups$NAMELSAD10, ", ", va_geo_census_cb_2010_census_block_groups$region_name)

# Assign year
va_geo_census_cb_2010_census_block_groups$year <- "2010"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- va_geo_census_cb_2010_census_block_groups[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(final_dataset_simplified, "data/va_geo_census_cb_2010_census_block_groups/distribution/va_geo_census_cb_2010_census_block_groups.geojson")

# Update file manifest
data_file_checksums()
