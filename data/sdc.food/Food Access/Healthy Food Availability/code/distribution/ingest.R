## RUN LOCALLY, OSM server does not like my Rivanna instance 

library(osmdata)
library(tidyverse)
library(sf)
library(geojsonsf)

bb <- c("Virginia")
# building the query
q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature("shop")

va_shops <- osmdata_sf(q)

bb <- c("Maryland")
# building the query
q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature("shop")

md_shops <- osmdata_sf(q)

bb <- c("Washington, DC")
# building the query
q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature("shop")

dc_shops <- osmdata_sf(q)

# combine lists
shops <- c(va_shops, md_shops, dc_shops)

supermarkets <- shops$osm_points %>%
  filter(str_detect(shop, "supermarket")) %>% select(osm_id, geometry) %>% distinct()
convenience <- shops$osm_points %>%
  filter(str_detect(shop, "convenience")) %>% select(osm_id, geometry) %>% distinct()
dollar <- shops$osm_points %>%
  filter(str_detect(shop, "variety")) %>% select(osm_id, geometry) %>% distinct()
department <- shops$osm_points %>%
  filter(str_detect(shop, "department"), str_detect(name, "Target|Walmart")) %>% select(osm_id, geometry) %>% distinct()

# Combine WalMart and Target with supermarkets list
supermarkets <- supermarkets %>% rbind(department)

bb <- c("Virginia")

q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature(key = "amenity", value = "fast_food")

# query
va_amenities <- osmdata_sf(q)

bb <- c("Maryland")

q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature(key = "amenity", value = "fast_food")

# query
md_amenities <- osmdata_sf(q)

bb <- c("Washington, DC")

q <- bb %>%
  opq(timeout = 25*100) %>%
  add_osm_feature(key = "amenity", value = "fast_food")

# query
dc_amenities <- osmdata_sf(q)

# combine lists
amenities <- c(va_amenities, md_amenities, dc_amenities)

# filter point layer to get fast food locations
fastfood <- amenities$osm_points %>%
  filter(str_detect(amenity, "fast_food")) %>% select(osm_id, geometry) %>% distinct()

# Combine Convience and Dollar Stores with Fast food list
fastfood <- fastfood %>% rbind(convenience) %>% rbind(dollar)

st_write(supermarkets, "supermarkets.geojson")
st_write(fastfood, "fastfood.geojson")