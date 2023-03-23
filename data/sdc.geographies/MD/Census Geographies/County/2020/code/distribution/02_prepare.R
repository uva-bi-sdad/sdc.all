# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2020_counties <- sf::st_read("data/md_geo_census_cb_2020_counties/original/us_geo_census_cb_2020_counties.geojson")
md_geo_census_cb_2020_counties <- us_geo_census_cb_2020_counties[substr(us_geo_census_cb_2020_counties$geoid, 1, 2) == "24",]


# Export final dataset
sf::st_write(md_geo_census_cb_2020_counties, "data/md_geo_census_cb_2020_counties/distribution/md_geo_census_cb_2020_counties.geojson")

# Update file manifest
data_file_checksums()
