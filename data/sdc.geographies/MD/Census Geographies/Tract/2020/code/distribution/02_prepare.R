# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
unzip("MD/Census Geographies/Tract/2020/data/original/md_geo_census_cb_2020_census_tracts.zip", exdir = "MD/Census Geographies/Tract/2020/data/original/md_geo_census_cb_2020_census_tracts")
md_geo_census_cb_2020_census_tracts <- sf::st_read("MD/Census Geographies/Tract/2020/data/original/md_geo_census_cb_2020_census_tracts/cb_2020_24_tract_500k.shp")
md_geo_census_cb_2020_census_tracts <- sf::st_transform(md_geo_census_cb_2020_census_tracts, 4326)

unlink("data/md_geo_census_cb_2020_census_tracts/original/md_geo_census_cb_2020_census_tracts", recursive = T)

# Assign geoid
md_geo_census_cb_2020_census_tracts$geoid <- md_geo_census_cb_2020_census_tracts$GEOID

# Assign region_type
md_geo_census_cb_2020_census_tracts$region_type <- "tract"

# Assign region_name
md_geo_census_cb_2020_census_tracts$region_name <-
  paste0(
    md_geo_census_cb_2020_census_tracts$NAMELSAD,
    ", ",
    md_geo_census_cb_2020_census_tracts$NAMELSADCO,
    ", ",
    md_geo_census_cb_2020_census_tracts$STATE_NAME
  )

# Assign year
md_geo_census_cb_2020_census_tracts$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- md_geo_census_cb_2020_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes = TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "MD/Census Geographies/Tract/2020/data/distribution/md_geo_census_cb_2020_census_tracts.geojson")

# Update file manifest
data_file_checksums()
