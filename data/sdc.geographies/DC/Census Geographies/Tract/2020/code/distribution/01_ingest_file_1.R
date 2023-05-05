# dataset creation code - data source ingest - file 1
# source file: https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_11_tract_500k.zip

# Import source file and save to original for backup
source_file <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_11_tract_500k.zip"
download.file(source_file, "DC/Census Geographies/Tract/2020/data/original/dc_geo_census_cb_2020_census_tracts.zip")
