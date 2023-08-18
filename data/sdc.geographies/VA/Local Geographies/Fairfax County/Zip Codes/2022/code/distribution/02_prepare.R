# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
va059_geo_ffxct_gis_2022_zip_codes <- sf::st_read(
  "VA/Local Geographies/Fairfax County/Zip Codes/2022/data/original/va059_geo_ffxct_gis_2022_zip_codes.geojson"
)

# Assign geoid
va059_geo_ffxct_gis_2022_zip_codes$geoid <- paste0(
  "51059_zc_", va059_geo_ffxct_gis_2022_zip_codes$ZIPCODE
)

# Assign region_type
va059_geo_ffxct_gis_2022_zip_codes$region_type <- "zip code"

# Assign region_name
va059_geo_ffxct_gis_2022_zip_codes$region_name <- paste0(
  gsub("\\b(\\w)", "\\U\\1", tolower(va059_geo_ffxct_gis_2022_zip_codes$ZIPCITY), perl = TRUE),
  " (", va059_geo_ffxct_gis_2022_zip_codes$ZIPCODE, ")"
)

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- va059_geo_ffxct_gis_2022_zip_codes[, c("geoid", "region_name", "region_type", "geometry")]

# Export final dataset
sf::st_write(
  rmapshaper::ms_simplify(final_dataset, keep_shapes = TRUE),
  "VA/Local Geographies/Fairfax County/Zip Codes/2022/data/distribution/va059_geo_ffxct_gis_2022_zip_codes.geojson",
  delete_dsn = TRUE
)
