# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
va059_geo_ffxct_gis_2022_supervisor_districts <- sf::st_read("VA/Local Geographies/Fairfax County/Supervisor Districts/2022/data/original/va059_geo_ffxct_gis_2022_supervisor_districts.geojson")

# Assign geoid
va059_geo_ffxct_gis_2022_supervisor_districts$geoid <- tolower(paste0("51059_sd_", va059_geo_ffxct_gis_2022_supervisor_districts$DISTRICT))

# Assign region_type
va059_geo_ffxct_gis_2022_supervisor_districts$region_type <- "supervisor district"

# Assign region_name
va059_geo_ffxct_gis_2022_supervisor_districts$region_name <- va059_geo_ffxct_gis_2022_supervisor_districts$DISTRICT

# Assign year
va059_geo_ffxct_gis_2022_supervisor_districts$year <- "2022"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- va059_geo_ffxct_gis_2022_supervisor_districts[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "VA/Local Geographies/Fairfax County/Supervisor Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_supervisor_districts.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
