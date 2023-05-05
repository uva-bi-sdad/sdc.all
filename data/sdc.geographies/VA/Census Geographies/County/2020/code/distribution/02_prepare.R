# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2020_counties <- sf::st_read("VA/Census Geographies/County/2020/data/original/us_geo_census_cb_2020_counties.geojson")
va_geo_census_cb_2020_counties <- us_geo_census_cb_2020_counties[substr(us_geo_census_cb_2020_counties$geoid, 1, 2) == "51",]


# Export final dataset
sf::st_write(va_geo_census_cb_2020_counties, "VA/Census Geographies/County/2020/data/distribution/va_geo_census_cb_2020_counties.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()