library(dplyr)
library(sf)
library(httr)
library(sp)
library(data.table)
library(stringr)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(tidyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(rjson)


# upload the data ---------------------------------------------------------------------------------------------
# load the parcels demograhics estimates 
uploadpath = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"
parcels_dmgs <- read.csv( paste0(uploadpath,"va059_pc_sdad_2019_demographics.csv.xz"))

# upload the human service regions geographies and parcels geography
hsr_geoinfos <- sf::st_read("https://github.com/uva-bi-sdad/sdc.geographies/blob/faa53d48b3f91693a7894bd4e0c976f8a2bfd80d/VA/Local%20Geographies/Fairfax%20County/Human%20Services%20Regions/2022/data/distribution/va059_geo_ffxct_gis_2022_human_services_regions.geojson?raw=T")

# parcels geo infos
parcels_geoinfos <- sf::st_read(utils::unzip(paste0(uploadpath,"fairfax_parcel_geometry.geojson.zip"), paste0(uploadpath,"fairfax_parcel_geometry.geojson")))
file.remove(paste0(uploadpath,"fairfax_parcel_geometry.geojson"))

#parcels_geoinfos <- sf::st_read(utils::unzip(paste0(uploadpath,"fairfax_parcel_geometry.geojson.zip"), paste0(uploadpath,"fairfax_parcel_geometry.geojson")))
#file.remove(paste0(uploadpath,"fairfax_parcel_geometry.geojson"))

# merge files ----------------------------------------------------
# add geometry to the parcels dmgs
parcels_dmgs_geo <- merge(parcels_dmgs, parcels_geoinfos, by = "geoid")

# intersect with the new geography
sf::sf_use_s2(FALSE)
parcels_dmgs_hsr_geo <- st_join(hsr_geoinfos, sf::st_as_sf(parcels_dmgs_geo), join = st_intersects)

# aggregate the all the variable the new geography ------------------------------------------------------------

# select the demography
dmgs = c("total_pop","pop_white","pop_black","pop_AAPI","pop_native","pop_hispanic_or_latino","pop_male","pop_female","pop_under_20","pop_20_64","pop_65_plus")
parcels_dmgs_hsr_geo_dt <- setDT(st_drop_geometry(parcels_dmgs_hsr_geo)) %>%
  filter(measure %in% dmgs) %>%
  select(geoid=geoid.x, region_name=region_name.x,region_type=region_type.x,year,measure,value,measure_type,MOE) %>%
  group_by(geoid,region_name,region_type,year,measure,measure_type,MOE) %>%
  summarise(value = sum(value, na.rm=T))


# compute other statistics (if necessary) such as proportion ------------------------------------------------------  
  
  
  

# compress and save the data -------------------------------------------------------------------------------------------
savepath = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/human_service_regions/data/distribution/"
readr::write_csv(parcels_dmgs_hsr_geo_dt, xzfile(paste0(savepath,"va059_hsr_sdad_2019_demographics.csv.xz"), compression = 9))




