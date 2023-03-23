# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
md_geo_census_cb_2020_census_tracts <- sf::st_read("MD/Census Geographies/Tract/2020/data/distribution/md_geo_census_cb_2020_census_tracts.geojson")
dc_geo_census_cb_2020_census_tracts <- sf::st_read("DC/Census Geographies/Tract/2020/data/distribution/dc_geo_census_cb_2020_census_tracts.geojson")
va_geo_census_cb_2020_census_tracts <- sf::st_read("VA/Census Geographies/Tract/2020/data/distribution/va_geo_census_cb_2020_census_tracts.geojson")

dc_geo_census_cb_2020_census_tracts$geometry <- sf::st_cast(dc_geo_census_cb_2020_census_tracts$geometry, "MULTIPOLYGON")

ncr_geo_census_cb_2020_census_tracts <- data.table::rbindlist(list(md_geo_census_cb_2020_census_tracts, dc_geo_census_cb_2020_census_tracts, va_geo_census_cb_2020_census_tracts))

# us_geo_census_cb_2020_census_tracts <- sf::st_read("data/ncr_geo_census_cb_2020_census_tracts/original/us_geo_census_cb_2020_census_tracts.geojson")

ncr_counties <- yaml::read_yaml("NCR/ncr_counties.yml")
ncr_geoids <- character()
for (i in 1:length(ncr_counties$ncr_localities)) {
  ncr_geoids <- c(ncr_geoids, ncr_counties$ncr_localities[[i]]$geoid)
}

ncr_geo_census_cb_2020_census_tracts <- ncr_geo_census_cb_2020_census_tracts[substr(ncr_geo_census_cb_2020_census_tracts$geoid, 1, 5) %in% ncr_geoids,]


# # Assign geoid
# ncr_geo_census_cb_2020_census_tracts$geoid <- ""
#
# # Assign region_type
# ncr_geo_census_cb_2020_census_tracts$region_type <- "tracts"
#
# # Assign region_name
# ncr_geo_census_cb_2020_census_tracts$region_name <- ""
#
# # Assign year
# ncr_geo_census_cb_2020_census_tracts$year <- "2020"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- ncr_geo_census_cb_2020_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(ncr_geo_census_cb_2020_census_tracts, "NCR/Census Geographies/Tract/2020/data/distribution/ncr_geo_census_cb_2020_census_tracts.geojson")


