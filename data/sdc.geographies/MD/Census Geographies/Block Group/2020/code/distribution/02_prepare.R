# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
unzip("MD/Census Geographies/Block Group/2020/data/original/md_geo_census_cb_2020_census_block_groups.zip", exdir = "MD/Census Geographies/Block Group/2020/data/original/md_geo_census_cb_2020_census_block_groups")
md_geo_census_cb_2020_census_block_groups <- sf::st_read("MD/Census Geographies/Block Group/2020/data/original/md_geo_census_cb_2020_census_block_groups/cb_2020_24_bg_500k.shp")
md_geo_census_cb_2020_census_block_groups <- sf::st_transform(md_geo_census_cb_2020_census_block_groups, 4326)

unlink("MD/Census Geographies/Block Group/2020/data/original/md_geo_census_cb_2020_census_block_groups", recursive = T)

# Assign geoid
md_geo_census_cb_2020_census_block_groups$geoid <- md_geo_census_cb_2020_census_block_groups$GEOID

# Assign region_type
md_geo_census_cb_2020_census_block_groups$region_type <- "block group"

# Assign region_name
md_geo_census_cb_2020_census_tracts <- sf::st_read("MD/Census Geographies/Tract/2020/data/distribution/md_geo_census_cb_2020_census_tracts.geojson")
md_geo_census_cb_2020_census_tracts$region_name_tract <- md_geo_census_cb_2020_census_tracts$region_name

md_geo_census_cb_2020_census_block_groups$tract_id <- substr(md_geo_census_cb_2020_census_block_groups$geoid, 1, 11)

md_geo_census_cb_2020_census_block_groups <- merge(md_geo_census_cb_2020_census_block_groups,
                                                   sf::st_drop_geometry(md_geo_census_cb_2020_census_tracts[, c("geoid", "region_name_tract")]),
                                                   by.x = "tract_id", by.y = "geoid")

md_geo_census_cb_2020_census_block_groups$region_name <- paste0(md_geo_census_cb_2020_census_block_groups$NAMELSAD, ", ", md_geo_census_cb_2020_census_block_groups$region_name_tract)

# Assign year
md_geo_census_cb_2020_census_block_groups$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- md_geo_census_cb_2020_census_block_groups[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes = TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "MD/Census Geographies/Block Group/2020/data/distribution/md_geo_census_cb_2020_census_block_groups.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
