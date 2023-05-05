# dataset creation code - data source ingest - file 1
# source file: US/Census Geographies/County/2010/data/distribution/us_geo_census_cb_2010_counties.geojson

# Import source file and save to original for backup
source_file <- "US/Census Geographies/County/2010/data/distribution/us_geo_census_cb_2010_counties.geojson"
#download.file(source_file, "data/va_geo_census_cb_2010_counties/original/va_geo_census_cb_2010_counties.geojson")
file.copy(source_file, "VA/Census Geographies/County/2010/data/original/us_geo_census_cb_2010_counties.geojson")
