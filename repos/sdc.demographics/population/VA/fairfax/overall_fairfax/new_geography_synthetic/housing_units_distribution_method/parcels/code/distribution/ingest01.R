library(dplyr)
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(httr)
library(rjson)
library(data.table)

##------------------------ Get parcels block data

fairfax_blocks <- blocks("VA", "059", 2020)
fairfax_blocks_wgs84 <-st_transform(fairfax_blocks, 4326)

#fairfax_address_points <- st_read("data-raw/Fairfax Address_Points/Address_Points.shp")
fairfax_address_points <- sf::read_sf('https://services1.arcgis.com/ioennV6PpG5Xodq0/arcgis/rest/services/OpenData_A2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')
fairfax_address_points_wgs84 <- st_transform(fairfax_address_points, 4326)

#fairfax_parcels <- st_read("data-raw/Fairfax County Parcels/Parcels.shp")
fairfax_parcels <- sf::read_sf('https://www.fairfaxcounty.gov/mercator/rest/services/OpenData/OpenData_A9/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json')
fairfax_parcels_wgs84 <-st_transform(fairfax_parcels, 4326)
fairfax_address_points_blocks_wgs84 <- st_join(fairfax_address_points_wgs84, fairfax_blocks_wgs84, join = st_within)

# Drop geometry and set as data.table for easier filtering
fairfax_address_points_blocks <- setDT(st_drop_geometry(fairfax_address_points_blocks_wgs84))

# Eliminate duplicates - Group by PARCEL_PIN and select the first one in each group
fairfax_address_points_blocks_unq <- fairfax_address_points_blocks[, .SD[1], "PARCEL_PIN"]

# Merge (Left Join) Parcels
fairfax_parcels_blocks_wgs84 <- merge(fairfax_parcels_wgs84, fairfax_address_points_blocks_unq, by.x = "PIN", by.y = "PARCEL_PIN", all.x = TRUE)
fairfax_parcels_blocks <- fairfax_parcels_blocks_wgs84 %>% dplyr::select(PIN,GEOID20) 



#--------------------- add tax files
fairfax_housing_units <- sf::read_sf('https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_A6/FeatureServer/1/query?where=1%3D1&outFields=LIVUNIT,PARID&outSR=4326&f=json') 

# merge the two data
fairfax_parcels_blocks <- merge(fairfax_housing_units, fairfax_parcels_blocks, by.x='PARID', by.y='PIN')

# Remove all NA units
fairfax_parcels_blocks <- fairfax_parcels_blocks[!is.na(fairfax_parcels_blocks$LIVUNIT),]
fairfax_parcels_blocks <- fairfax_parcels_blocks[!is.na(fairfax_parcels_blocks$GEOID20),]

# save the data in the working folder (cautious: the size of the data may be too big)
fairfax_parcels_blocks <- sf::st_as_sf(fairfax_parcels_blocks)
fairfax_parcels_blocks <- fairfax_parcels_blocks %>% select(PARID,GEOID20,LIVUNIT,geometry)


#-------------------------------------- compress and save the data 
path = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"

st_write(fairfax_parcels_blocks, paste0(path,"fairfax_parcels_blocks.geojson"))
zip(zipfile = paste0(path,"fairfax_parcels_blocks.geojson.zip"), files = paste0(path,"fairfax_parcels_blocks.geojson"))
file.remove(paste0(path,"fairfax_parcels_blocks.geojson"))
