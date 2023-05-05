# dataset creation code - data source ingest - file 1
va_geo_census_cb_2010_census_block_groups <- tigris::block_groups(state = "VA", year = 2010)
sf::st_write(va_geo_census_cb_2010_census_block_groups, "VA/Census Geographies/Block Group/2010/data/original/va_geo_census_cb_2010_census_block_groups.geojson")
