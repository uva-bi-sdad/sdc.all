library(dplyr)
library(sf)
library(httr)
library(rjson)
library(sp)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(tigris)


# ---------------------- upload DC blocks groups, ssl geometry and housing units
DC_blocks <- blocks("DC", "001", 2020)
DC_blocks_wgs84 <-st_transform(DC_blocks, 4326) %>%
  dplyr::select(STATEFP20, COUNTYFP20, TRACTCE20, BLOCKCE20, GEOID20, NAME20, geometry)
DC_blocks_wgs84$GEOID <- DC_blocks_wgs84$GEOID20  #substr(DC_blocks_wgs84$GEOID20,1,12)

# upload DC adress points. Comments: some address doesn't have SSL. An address can be located in many SSL.
DC_address_points <- sf::read_sf('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Location_WebMercator/MapServer/0/query?outFields=*&where=1%3D1&f=geojson') 
#DC_address_points <- st_read("~/Github/vdh/dc_data/Address_Points/Address_Points.shp")
DC_address_points_wgs84 <- st_transform(DC_address_points, 4326) %>%
  dplyr::select(ADDRESS_ID,SSL,CENSUS_BLOCK,ACTIVE_RES_UNIT_COUNT,geometry)

# remove space in the census block id.
DC_address_points_wgs84$CENSUS_BLOCK <- str_replace_all(DC_address_points_wgs84$CENSUS_BLOCK," ","")

#upload DC lot. correct the geometry
DC_lots <- sf::read_sf('https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Property_and_Land_WebMercator/MapServer/40/query?outFields=*&where=1%3D1&f=geojson') %>% select(SSL,geometry) 
#DC_lots <- st_read("~/Github/vdh/dc_data/Common_Ownership_Lots/Common_Ownership_Lots.shp")
DC_lots_wgs84 <-st_transform(DC_lots, 4326)
DC_lots_wgs84 <- sf::st_make_valid(DC_lots_wgs84)


# assign adress points to each census blocks
DC_address_points_blocks_wgs84 <- st_join(DC_address_points_wgs84, DC_blocks_wgs84, join = st_within) 

# Drop geometry from block and transform into data.table
DC_address_points_blocks <- setDT(st_drop_geometry(DC_address_points_blocks_wgs84))

# Eliminate duplicates address by SSL- Group by SSL and select the first one in each group. assign adress to geometry lot and connect with census block
DC_address_points_blocks_unq <- DC_address_points_blocks[, .SD[1], "SSL"]
DC_lot_blocks_wgs84 <- merge(DC_lots_wgs84, DC_address_points_blocks_unq, by.x = "SSL", by.y = "SSL", all.x = TRUE)

# keep the main variables
DC_lot_blocks_wgs84 <- DC_lot_blocks_wgs84 %>%
  dplyr::select(SSL,ACTIVE_RES_UNIT_COUNT,CENSUS_BLOCK,GEOID,geometry)

# remove the na GEOID
DC_lot_blocks_wgs84 <- DC_lot_blocks_wgs84[!is.na(DC_lot_blocks_wgs84$GEOID),]

# compress and save the data 
path = "population/DC/overall_DC/new_geography_synthetic/housing_units_distribution_method/ssl/data/working/"
st_write(DC_lot_blocks_wgs84, paste0(path,"DC_block_ssl.geojson"))
zip(zipfile = paste0(path,"DC_block_ssl.geojson.zip"), files = paste0(path,"DC_block_ssl.geojson"))
file.remove(paste0(path,"DC_block_ssl.geojson"))


