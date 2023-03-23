# dataset creation code - data source ingest - file 1
# source file: data/us_geo_census_cb_2010_census_tracts/distribution/us_geo_census_cb_2010_census_tracts.geojson

# Import source file and save to original for backup
source_file <- "data/us_geo_census_cb_2010_census_tracts/distribution/us_geo_census_cb_2010_census_tracts.geojson"
file.copy(source_file, "data/ncr_geo_census_2010_census_tracts/original/us_geo_census_cb_2010_census_tracts.geojson")
