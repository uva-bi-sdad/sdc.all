# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2020_counties <- sf::st_read("NCR/Census Geographies/County/2020/data/original/us_geo_census_cb_2020_counties.geojson")

ncr_counties <- yaml::read_yaml("NCR/ncr_counties.yml")
ncr_geoids <- character()
for (i in 1:length(ncr_counties$ncr_localities)) {
  ncr_geoids <- c(ncr_geoids, ncr_counties$ncr_localities[[i]]$geoid)
}

ncr_geo_census_cb_2020_counties <- us_geo_census_cb_2020_counties[us_geo_census_cb_2020_counties$geoid %in% ncr_geoids,]

# # Assign geoid
# ncr_geo_census_cb_2020_counties$geoid <- ""
#
# # Assign region_type
# ncr_geo_census_cb_2020_counties$region_type <- "county"
#
# # Assign region_name
# ncr_geo_census_cb_2020_counties$region_name <- ""
#
# # Assign year
# ncr_geo_census_cb_2020_counties$year <- "2020"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- ncr_geo_census_cb_2020_counties[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(ncr_geo_census_cb_2020_counties, "NCR/Census Geographies/County/2020/data/distribution/ncr_geo_census_cb_2020_counties.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
