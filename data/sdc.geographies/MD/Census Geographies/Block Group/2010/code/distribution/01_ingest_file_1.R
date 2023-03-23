# dataset creation code - data source ingest - file 1
md_geo_census_cb_2010_census_block_groups <- tigris::block_groups(state = "MD", year = 2010)
sf::st_write(md_geo_census_cb_2010_census_block_groups, "data/md_geo_census_cb_2010_census_block_groups/original/md_geo_census_cb_2010_census_block_groups.geojson")
