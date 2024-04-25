# Import source file and save to original for backup
us_geo_census_cb_2020_states <- tigris::states(year = 2020, cb = T)
sf::st_write(us_geo_census_cb_2020_states, "US/Census Geographies/States/2020/data/original/us_geo_census_cb_2020_states.geojson",
             delete_dsn = TRUE)
