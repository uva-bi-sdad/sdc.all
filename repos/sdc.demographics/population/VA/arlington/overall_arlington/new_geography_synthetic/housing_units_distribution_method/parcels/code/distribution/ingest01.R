library(dplyr)
library(sf)
library(httr)
library(rjson)
library(sp)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")



##------------------------ Get housing units from OpenData
va_arl_housing_units <- sf::st_read("https://opendata.arcgis.com/datasets/628f6de7205641169273ea684a74fb0f_0.geojson")

# filter to unique parcel, with census block and total housing units on the parcel
va_arl_block_parcels <- unique(va_arl_housing_units[, c("RPC_Master", "Full_Block", "Total_Units")])

# save the data in the working folder (cautious: the size of the data may be too big)
va_arl_block_parcels <- sf::st_as_sf(va_arl_block_parcels)

# compress and save the data 
#-------------------------------------- compress and save the data 
path = "population/VA/arlington/overall_arlington/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"
st_write(va_arl_block_parcels, paste0(path,"va_arl_block_parcels.geojson"))
zip(zipfile = paste0(path,"va_arl_block_parcels.geojson.zip"), files = paste0(path,"va_arl_block_parcels.geojson"))
file.remove(paste0(path,"va_arl_block_parcels.geojson"))


