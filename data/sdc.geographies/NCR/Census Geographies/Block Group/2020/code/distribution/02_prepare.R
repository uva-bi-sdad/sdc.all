# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
md_geo_census_cb_2020_census_block_groups <- sf::st_read("MD/Census Geographies/Block Group/2020/data/distribution/md_geo_census_cb_2020_census_block_groups.geojson")
dc_geo_census_cb_2020_census_block_groups <- sf::st_read("DC/Census Geographies/Block Group/2020/data/distribution/dc_geo_census_cb_2020_census_block_groups.geojson")
va_geo_census_cb_2020_census_block_groups <- sf::st_read("VA/Census Geographies/Block Group/2020/data/distribution/va_geo_census_cb_2020_census_block_groups.geojson")

dc_geo_census_cb_2020_census_block_groups$geometry <- sf::st_cast(dc_geo_census_cb_2020_census_block_groups$geometry, "MULTIPOLYGON")

ncr_geo_census_cb_2020_census_block_groups <- data.table::rbindlist(list(md_geo_census_cb_2020_census_block_groups, dc_geo_census_cb_2020_census_block_groups, va_geo_census_cb_2020_census_block_groups))

ncr_counties <- yaml::read_yaml("NCR/ncr_counties.yml")
ncr_geoids <- character()
for (i in 1:length(ncr_counties$ncr_localities)) {
  ncr_geoids <- c(ncr_geoids, ncr_counties$ncr_localities[[i]]$geoid)
}

ncr_geo_census_cb_2020_census_block_groups <- ncr_geo_census_cb_2020_census_block_groups[substr(geoid, 1, 5) %in% ncr_geoids]
ncr_geo_census_cb_2020_census_block_groups <- sf::st_as_sf(ncr_geo_census_cb_2020_census_block_groups)

# # Assign geoid
# ncr_geo_census_cb_2020_census_block_groups$geoid <- ""
#
# # Assign region_type
# ncr_geo_census_cb_2020_census_block_groups$region_type <- "block group"
#
# # Assign region_name
# ncr_geo_census_cb_2020_census_block_groups$region_name <- ""
#
# # Assign year
# ncr_geo_census_cb_2020_census_block_groups$year <- "2020"
#
# # measure, measure_type, and value need to be included in non-geo datasets
#
# # Select final columns
# final_dataset <- ncr_geo_census_cb_2020_census_block_groups[, c("geoid", "region_name", "region_type", "year", "geometry")]
#
# # Simplify the geography
# final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(ncr_geo_census_cb_2020_census_block_groups, "NCR/Census Geographies/Block Group/2020/data/distribution/ncr_geo_census_cb_2020_census_block_groups.geojson")


