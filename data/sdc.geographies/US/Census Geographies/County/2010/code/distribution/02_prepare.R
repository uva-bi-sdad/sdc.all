# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
us_geo_census_cb_2010_counties <- sf::st_read("data/us_geo_census_cb_2010_counties/original/us_geo_census_cb_2010_counties.geojson")
us_geo_census_cb_2010_counties <- sf::st_transform(us_geo_census_cb_2010_counties, 4326)

states2010 <- sf::st_drop_geometry(tigris::states(cb = T, year = 2010))

us_geo_census_cb_2010_counties <- merge(us_geo_census_cb_2010_counties, states2010, by.x = "STATEFP", by.y = "STATE", all.x = T)


# Assign geoid
us_geo_census_cb_2010_counties$geoid <- us_geo_census_cb_2010_counties$GEOID10

# Assign region_type
us_geo_census_cb_2010_counties$region_type <- "county"

# Assign region_name
us_geo_census_cb_2010_counties$region_name <-
  stringr::str_to_title(
    paste0(
      us_geo_census_cb_2010_counties$NAMELSAD10,
      ", ",
      us_geo_census_cb_2010_counties$NAME
    )
  )

# Assign year
us_geo_census_cb_2010_counties$year <- "2010"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- us_geo_census_cb_2010_counties[, c("geoid", "region_name", "region_type", "year", "geometry")]

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
sf::st_write(final_dataset_simplified, "data/us_geo_census_cb_2010_counties/distribution/us_geo_census_cb_2010_counties.geojson")

# Update file manifest
data_file_checksums()
