# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
va059_geo_ffxct_gis_2022_zip_codes <- sf::st_read("data/va059_geo_ffxct_gis_2022_zip_codes/original/va059_geo_ffxct_gis_2022_zip_codes.geojson")

# Assign geoid
va059_geo_ffxct_gis_2022_zip_codes$geoid <- va059_geo_ffxct_gis_2022_zip_codes$ZIPCODE

# Assign region_type
va059_geo_ffxct_gis_2022_zip_codes$region_type <- "zip code"

# Assign region_name
va059_geo_ffxct_gis_2022_zip_codes$region_name <- va059_geo_ffxct_gis_2022_zip_codes$ZIPCITY

# Assign year
va059_geo_ffxct_gis_2022_zip_codes$year <- "2022"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- va059_geo_ffxct_gis_2022_zip_codes[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(final_dataset_simplified, "data/va059_geo_ffxct_gis_2022_zip_codes/distribution/va059_geo_ffxct_gis_2022_zip_codes.geojson")

# Update file manifest
data_file_checksums()

# COPY Info Files to docs
file.copy("data/va059_geo_ffxct_gis_2022_zip_codes/original/va059_geo_ffxct_gis_2022_zip_codes.json", "docs/01_data/va059_geo_ffxct_gis_2022_zip_codes/")
