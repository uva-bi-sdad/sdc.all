# dataset creation code - dataset preparation (transformation, new variables, linkage, etc)

# Import file from original
state_codes <- tigris::states(year = 2010)$STATEFP

if (exists("all_tracts")) rm(all_tracts)
for (s in state_codes) {
  print(s)
  t <- data.table::setDT(tigris::tracts(state = s, cb = T, year = 2010))
  t$geometry <- sf::st_cast(t$geometry, "MULTIPOLYGON")
  #dt[, c(convert_to_character) := lapply(.SD, as.character), .SDcols=convert_to_character]
  if (exists("all_tracts")) all_tracts <- data.table::rbindlist(list(all_tracts, t))
  else all_tracts <- t
}

us_geo_census_cb_2010_census_tracts <- all_tracts


states2010 <- sf::st_drop_geometry(tigris::states(cb = T, year = 2010))
counties2010 <- sf::st_drop_geometry(tigris::counties(year = 2010))

# Change Encoding type of counties2010
counties2010$NAME10 <- iconv(counties2010$NAME10, from="LATIN1", to="UTF-8")
counties2010$NAMELSAD10 <- iconv(counties2010$NAMELSAD10, from="LATIN1", to="UTF-8")

counties_states_2010 <- merge(counties2010, states2010, by.x = "STATEFP10", by.y = "STATE")
us_geo_census_cb_2010_census_tracts$GEOID10 <- paste0(us_geo_census_cb_2010_census_tracts$STATE, us_geo_census_cb_2010_census_tracts$COUNTYFP)
us_geo_census_cb_2010_census_tracts <- merge(us_geo_census_cb_2010_census_tracts, counties_states_2010, by = "GEOID10")

# Assign geoid
us_geo_census_cb_2010_census_tracts$geoid <- substr(us_geo_census_cb_2010_census_tracts$GEO_ID.x, 10, 20)

# Assign region_type
us_geo_census_cb_2010_census_tracts$region_type <- "tract"

# Assign region_name
us_geo_census_cb_2010_census_tracts$region_name <-
  stringr::str_to_title(
    paste0(
      "Census ",
      us_geo_census_cb_2010_census_tracts$LSAD.x,
      " ",
      us_geo_census_cb_2010_census_tracts$NAME.x,
      ", ",
      us_geo_census_cb_2010_census_tracts$NAMELSAD10,
      ", ",
      us_geo_census_cb_2010_census_tracts$NAME.y
    )
  )

# Assign year
us_geo_census_cb_2010_census_tracts$year <- "2010"

# measure, measure_type, and value need to be included in non-geo datasets

# Select final columns
final_dataset <- sf::st_as_sf(us_geo_census_cb_2010_census_tracts[, c("geoid", "region_name", "region_type", "year", "geometry")])
final_dataset <- sf::st_transform(final_dataset, 4326)

# Simplify the geography
final_dataset_simplified <- rmapshaper::ms_simplify(final_dataset)

# Export final dataset
#sf::st_write(final_dataset_simplified, "data/us_geo_census_cb_2010_census_tracts/distribution/us_geo_census_cb_2010_census_tracts.geojson")
sf::st_write(final_dataset_simplified, "./US/Census Geographies/Tract/2010/data/distribution/us_geo_census_cb_2010_census_tracts.geojson")

# Update file manifest
data_file_checksums()
