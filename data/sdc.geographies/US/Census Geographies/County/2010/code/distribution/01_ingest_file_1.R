# dataset creation code - data source ingest - file 1
# source file: https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_500k.zip

# Import source file and save to original for backup
us_geo_census_cb_2010_counties <- tigris::counties(year = 2010)
sf::st_write(us_geo_census_cb_2010_counties, "data/us_geo_census_cb_2010_counties/original/us_geo_census_cb_2010_counties.geojson")
