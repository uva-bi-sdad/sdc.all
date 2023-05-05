# dataset creation code - data source ingest - file 1
md_geo_census_cb_2010_census_block_groups <- tigris::block_groups(state = "MD", year = 2010)
sf::st_write(md_geo_census_cb_2010_census_block_groups, "MD/Census Geographies/Block Group/2010/data/original/md_geo_census_cb_2010_census_block_groups.geojson")
