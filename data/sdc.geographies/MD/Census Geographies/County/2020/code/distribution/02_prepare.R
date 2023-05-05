# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2020_counties <- sf::st_read("MD/Census Geographies/County/2020/data/original/us_geo_census_cb_2020_counties.geojson")
md_geo_census_cb_2020_counties <- us_geo_census_cb_2020_counties[substr(us_geo_census_cb_2020_counties$geoid, 1, 2) == "24",]


# Export final dataset
sf::st_write(md_geo_census_cb_2020_counties, "MD/Census Geographies/County/2020/data/distribution/md_geo_census_cb_2020_counties.geojson",
             delete_dsn=TRUE)

# Update file manifest
#data_file_checksums()
