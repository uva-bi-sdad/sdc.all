# dataset creation code - data source ingest - file 1
# source file: https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_51_bg_500k.zip

# Import source file and save to original for backup
source_file <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_51_bg_500k.zip"
download.file(source_file, "VA/Census Geographies/Block Group/2020/data/original/va_geo_census_cb_2020_census_block_groups.zip")
