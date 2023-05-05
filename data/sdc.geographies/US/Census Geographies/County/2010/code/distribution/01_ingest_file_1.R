# dataset creation code - data source ingest - file 1
# source file: https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_500k.zip 
# Source file above might not be the cb version, needs to be checked

# Import source file and save to original for backup
us_geo_census_cb_2010_counties <- tigris::counties(year = 2010, cb = T)
sf::st_write(us_geo_census_cb_2010_counties, "./US/Census Geographies/County/2010/data/original/us_geo_census_cb_2010_counties.geojson",
             delete_dsn = TRUE)
