# dataset creation code - data source ingest - file 3
# source file: data/va_geo_census_cb_2010_census_block_groups/distribution/va_geo_census_cb_2010_census_block_groups.geojson

# Import source file and save to original for backup
source_file <- "data/va_geo_census_cb_2010_census_block_groups/distribution/va_geo_census_cb_2010_census_block_groups.geojson"
file.copy(source_file, "data/ncr_geo_census_2010_census_block_groups/original/va_geo_census_cb_2010_census_block_groups.geojson")
