# dataset preparation 

`%>%` <- magrittr::`%>%` # defines operator

# Import file from original
va059_geo_ffxct_gis_2023_high_school_attendance_areas <- sf::st_read("VA/Local Geographies/Fairfax County/High School Attendance Areas/2023/data/original/va059_geo_ffxct_gis_2023_high_school_attendance_areas.geojson")

# Assign geoid
va059_geo_ffxct_gis_2023_high_school_attendance_areas$geoid <- tolower(paste0("51059_hsaa_", va059_geo_ffxct_gis_2023_high_school_attendance_areas$SCHOOL_NAME))

# Assign region_type
va059_geo_ffxct_gis_2023_high_school_attendance_areas$region_type <- "high school attendance area"

# Assign region_name
va059_geo_ffxct_gis_2023_high_school_attendance_areas$region_name <- va059_geo_ffxct_gis_2023_high_school_attendance_areas$SCHOOL_NAME

# Assign year
va059_geo_ffxct_gis_2023_high_school_attendance_areas$year <- "2023"

# Select final columns
final_dataset <- va059_geo_ffxct_gis_2023_high_school_attendance_areas[, c("geoid", "region_name", "region_type", "year", "geometry")] %>% 
  dplyr::mutate(geoid=sapply(strsplit(geoid, " "), paste, collapse="_"))

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE) 

# Export final dataset
sf::st_write(final_dataset_simplified, "VA/Local Geographies/Fairfax County/High School Attendance Areas/2023/data/distribution/va059_geo_ffxct_gis_2023_high_school_attendance_areas.geojson",
             delete_dsn=TRUE)