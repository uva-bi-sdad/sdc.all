# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_counties <- sf::st_read("./US/Census Geographies/County/2010/data/original/us_geo_census_cb_2010_counties.geojson")
us_geo_census_cb_2010_counties <- sf::st_transform(us_geo_census_cb_2010_counties, 4326)

states2010 <- sf::st_drop_geometry(tigris::states(cb = T, year = 2010))

us_geo_census_cb_2010_counties <- merge(us_geo_census_cb_2010_counties, states2010, by.x = "STATEFP", by.y = "STATE", all.x = T)

# get names from the non-cb pull
counties2010 <- sf::st_drop_geometry(tigris::counties(year = 2010))
us_geo_census_cb_2010_counties$geoid <- paste0(us_geo_census_cb_2010_counties$STATEFP, us_geo_census_cb_2010_counties$COUNTYFP)
us_geo_census_cb_2010_counties <- merge(us_geo_census_cb_2010_counties, counties2010[ ,c("GEOID10", "NAMELSAD10")], 
                                        by.x = "geoid", by.y = "GEOID10", all.x = T)


# Change Encoding type of county names
us_geo_census_cb_2010_counties$NAMELSAD10 <- iconv(us_geo_census_cb_2010_counties$NAMELSAD10, from="LATIN1", to="UTF-8")


# Assign geoid
#us_geo_census_cb_2010_counties$geoid <- us_geo_census_cb_2010_counties$GEOID10

# Assign region_type
us_geo_census_cb_2010_counties$region_type <- "county"

# Assign region_name
us_geo_census_cb_2010_counties$region_name <-
  stringr::str_to_title(
    paste0(
      us_geo_census_cb_2010_counties$NAMELSAD10,
      ", ",
      us_geo_census_cb_2010_counties$NAME.y
    )
  )

# Assign year
us_geo_census_cb_2010_counties$year <- "2010"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- us_geo_census_cb_2010_counties[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset, keep_shapes=TRUE)

# Export final dataset
sf::st_write(final_dataset_simplified, "US/Census Geographies/County/2010/data/distribution/us_geo_census_cb_2010_counties.geojson",
             delete_dsn = TRUE)

# Update file manifest
#data_file_checksums()
