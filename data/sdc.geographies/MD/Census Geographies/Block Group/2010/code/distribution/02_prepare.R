# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
md_geo_census_cb_2010_census_block_groups <- sf::st_read("data/md_geo_census_cb_2010_census_block_groups/original/md_geo_census_cb_2010_census_block_groups.geojson")
md_geo_census_cb_2010_census_block_groups <- sf::st_transform(md_geo_census_cb_2010_census_block_groups, 4326)

us_geo_census_cb_2010_census_tracts <- sf::st_drop_geometry(sf::st_read("data/us_geo_census_cb_2010_census_tracts/distribution/us_geo_census_cb_2010_census_tracts.geojson"))

md_geo_census_cb_2010_census_block_groups$geoid_tract <- substr(md_geo_census_cb_2010_census_block_groups$GEOID10, 1, 11)
md_geo_census_cb_2010_census_block_groups <- merge(md_geo_census_cb_2010_census_block_groups, us_geo_census_cb_2010_census_tracts, by.x = "geoid_tract", by.y = "geoid", all.x = T)

# Assign geoid
md_geo_census_cb_2010_census_block_groups$geoid <- md_geo_census_cb_2010_census_block_groups$GEOID10

# Assign region_type
md_geo_census_cb_2010_census_block_groups$region_type <- "block group"

# Assign region_name
md_geo_census_cb_2010_census_block_groups$region_name <- paste0(md_geo_census_cb_2010_census_block_groups$NAMELSAD10, ", ", md_geo_census_cb_2010_census_block_groups$region_name)

# Assign year
md_geo_census_cb_2010_census_block_groups$year <- "2010"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- md_geo_census_cb_2010_census_block_groups[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(final_dataset_simplified, "data/md_geo_census_cb_2010_census_block_groups/distribution/md_geo_census_cb_2010_census_block_groups.geojson")

# Update file manifest
data_file_checksums()
