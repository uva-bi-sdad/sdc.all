library(tidyverse)
library(sf)
library(geojsonsf)

supermarkets <- geojson_sf("Food Access/Healthy Food Availability/data/original/supermarkets.geojson")
fastfood <- geojson_sf("Food Access/Healthy Food Availability/data/original/fastfood.geojson")

## COMPUTE MRFEI FOR BLOCK GROUP

blockgroups <- geojson_sf("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Block%20Group/2020/data/distribution/ncr_geo_census_cb_2020_census_block_groups.geojson")

bg_supermarkets <- st_join(blockgroups, supermarkets, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(super_count = n()) %>% select(geoid, super_count) %>% st_drop_geometry()
bg_fastfood <- st_join(blockgroups, fastfood, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(fast_count = n()) %>% select(geoid, fast_count) %>% st_drop_geometry()
bg_RFEI <- blockgroups %>% left_join(bg_supermarkets, by = "geoid") %>% 
  left_join(bg_fastfood, by = "geoid") %>% 
  mutate(RFEI = (super_count/(super_count + fast_count)) * 100) %>% mutate(RFEI = ifelse(is.na(RFEI), 0, RFEI)) %>% distinct()

## COMPUTE MRFEI FOR TRACT

tracts <- geojson_sf("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/Tract/2020/data/distribution/ncr_geo_census_cb_2020_census_tracts.geojson")

tr_supermarkets <- st_join(tracts, supermarkets, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(super_count = n()) %>% select(geoid, super_count) %>% st_drop_geometry()
tr_fastfood <- st_join(tracts, fastfood, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(fast_count = n()) %>% select(geoid, fast_count) %>% st_drop_geometry()
tr_RFEI <- tracts %>% left_join(tr_supermarkets, by = "geoid") %>% 
  left_join(tr_fastfood, by = "geoid") %>% 
  mutate(RFEI = (super_count/(super_count + fast_count)) * 100) %>% mutate(RFEI = ifelse(is.na(RFEI), 0, RFEI)) %>% distinct()

## COMPUTE MRFEI FOR COUNTIES

counties <- geojson_sf("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/NCR/Census%20Geographies/County/2020/data/distribution/ncr_geo_census_cb_2020_counties.geojson")

ct_supermarkets <- st_join(counties, supermarkets, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(super_count = n()) %>% select(geoid, super_count) %>% st_drop_geometry() %>% distinct()
ct_fastfood <- st_join(counties, fastfood, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(fast_count = n()) %>% select(geoid, fast_count) %>% st_drop_geometry() %>% distinct()
ct_RFEI <- counties %>% left_join(ct_supermarkets, by = "geoid") %>% 
  left_join(ct_fastfood, by = "geoid") %>% 
  mutate(RFEI = (super_count/(super_count + fast_count)) * 100) %>% mutate(RFEI = ifelse(is.na(RFEI), 0, RFEI))

## COMPUTE MRFEI FOR CIVIC

civic <- geojson_sf("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/Arlington%20County/Civic%20Associations/2021/data/distribution/va013_geo_arl_2021_civic_associations.geojson")

civic_supermarkets <- st_join(civic, supermarkets, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(super_count = n()) %>% select(geoid, super_count) %>% st_drop_geometry() %>% distinct()
civic_fastfood <- st_join(civic, fastfood, st_is_within_distance, dist = 804.672) %>% filter(!is.na(osm_id)) %>% group_by(geoid) %>% mutate(fast_count = n()) %>% select(geoid, fast_count) %>% st_drop_geometry() %>% distinct()
civic_RFEI <- civic %>% left_join(civic_supermarkets, by = "geoid") %>% 
  left_join(civic_fastfood, by = "geoid") %>% 
  mutate(RFEI = (super_count/(super_count + fast_count)) * 100) %>% mutate(RFEI = ifelse(is.na(RFEI), 0, RFEI))

RFEI <- bg_RFEI %>% rbind(tr_RFEI) %>% rbind(ct_RFEI) %>% rbind(civic_RFEI)

# create dataset
ncr_cttrbgca_sdad_2023_mrfei <- RFEI %>% st_drop_geometry() %>% select(geoid, region_name, region_type, value = RFEI) %>% mutate(year = 2023, measure = "mrfei", measure_type = "index", moe = NA)
write_csv(ncr_cttrbgca_sdad_2023_mrfei, "Food Access/Healthy Food Availability/data/distribution/ncr_cttrbgca_sdad_2023_mrfei.csv.xz")

