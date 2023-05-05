# dataset creation code - data source ingest - file 1
dc_geo_census_cb_2010_census_block_groups <- tigris::block_groups(state = "DC", year = 2010)
sf::st_write(dc_geo_census_cb_2010_census_block_groups, "DC/Census Geographies/Block Group/2010/data/original/dc_geo_census_cb_2010_census_block_groups.geojson",
             delete_dsn=TRUE)
