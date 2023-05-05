# dataset creation code - data source ingest - file 1
# source file: https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_500k.zip

# Import source file and save to original for backup
us_geo_census_cb_2020_counties <- tigris::counties(year = 2020, cb = T)
sf::st_write(us_geo_census_cb_2020_counties, "US/Census Geographies/County/2020/data/original/us_geo_census_cb_2020_counties.geojson",
             delete_dsn = TRUE)
