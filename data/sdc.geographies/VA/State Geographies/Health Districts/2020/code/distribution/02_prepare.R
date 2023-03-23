# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
unzip("data/va_geo_vhd_2020_health_districts/original/VDH_Health_Districts.zip", exdir = "data/va_geo_vhd_2020_health_districts/original/VDH_Health_Districts")
va_geo_vhd_2020_health_districts <- sf::st_read("data/va_geo_vhd_2020_health_districts/original/VDH_Health_Districts/geo_export_779d2774-2ad5-42e5-811d-8c3c8ea34e4f.shp")
va_geo_vhd_2020_health_districts <- sf::st_transform(va_geo_vhd_2020_health_districts, 4326)

unlink("data/va_geo_vhd_2020_health_districts/original/VDH_Health_Districts", recursive = T)

# Assign geoid


va_geo_vhd_2020_health_districts$geoid <-
  tolower(
    gsub("/", "-",
         gsub(" ","_", paste0("51_hd_", stringr::str_pad(va_geo_vhd_2020_health_districts$fid, 2, "left", "0")))))

# Assign region_type
va_geo_vhd_2020_health_districts$region_type <- "health district"

# Assign region_name
va_geo_vhd_2020_health_districts$region_name <- va_geo_vhd_2020_health_districts$vdh_hd

# Assign year
va_geo_vhd_2020_health_districts$year <- "2020"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- va_geo_vhd_2020_health_districts[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes = TRUE, input = )

# Export final dataset
sf::st_write(final_dataset, "data/va_geo_vhd_2020_health_districts/distribution/va_geo_vhd_2020_health_districts.geojson")

# Update file manifest
data_file_checksums()
