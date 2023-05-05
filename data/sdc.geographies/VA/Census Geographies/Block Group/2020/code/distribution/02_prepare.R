# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
path_orig <- "VA/Census Geographies/Block Group/2020/data/original/va_geo_census_cb_2020_census_block_groups.zip"
unzip(path_orig, exdir = tools::file_path_sans_ext(path_orig))
va_geo_census_cb_2020_census_block_groups <- sf::st_read(file.path(tools::file_path_sans_ext(path_orig), 
                                                             list.files(tools::file_path_sans_ext(path_orig), pattern = "shp$")[[1]]))
va_geo_census_cb_2020_census_block_groups <- sf::st_transform(va_geo_census_cb_2020_census_block_groups, 4326)

unlink(tools::file_path_sans_ext(path_orig), recursive = T)

# Assign geoid
va_geo_census_cb_2020_census_block_groups$geoid <- va_geo_census_cb_2020_census_block_groups$GEOID

# Assign region_type
va_geo_census_cb_2020_census_block_groups$region_type <- "block group"

# Assign region_name
va_geo_census_cb_2020_census_tracts <- sf::st_read("VA/Census Geographies/Tract/2020/data/distribution/va_geo_census_cb_2020_census_tracts.geojson")
va_geo_census_cb_2020_census_tracts$region_name_tract <- va_geo_census_cb_2020_census_tracts$region_name

va_geo_census_cb_2020_census_block_groups$tract_id <- substr(va_geo_census_cb_2020_census_block_groups$geoid, 1, 11)

va_geo_census_cb_2020_census_block_groups <- merge(va_geo_census_cb_2020_census_block_groups,
                                                   sf::st_drop_geometry(va_geo_census_cb_2020_census_tracts[, c("geoid", "region_name_tract")]),
                                                   by.x = "tract_id", by.y = "geoid")

va_geo_census_cb_2020_census_block_groups$region_name <- paste0(va_geo_census_cb_2020_census_block_groups$NAMELSAD, ", ", va_geo_census_cb_2020_census_block_groups$region_name_tract)

# Assign year
va_geo_census_cb_2020_census_block_groups$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- va_geo_census_cb_2020_census_block_groups[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes = TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "VA/Census Geographies/Block Group/2020/data/distribution/va_geo_census_cb_2020_census_block_groups.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
