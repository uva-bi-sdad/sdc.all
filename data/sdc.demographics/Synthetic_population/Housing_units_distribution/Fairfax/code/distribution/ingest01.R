library(dplyr)
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(httr)
library(rjson)
library(data.table)
library(stringr)

# Get parcels block data ------------------------------------------------------------------
# census blocks data from 2010
fairfax_blocks <- blocks("VA", "059", 2020)
fairfax_blocks_wgs84 <-st_transform(fairfax_blocks, 4326)

#fairfax_address_points <- st_read("data-raw/Fairfax Address_Points/Address_Points.shp")
fairfax_address_points <- sf::read_sf('https://services1.arcgis.com/ioennV6PpG5Xodq0/arcgis/rest/services/OpenData_A2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')
fairfax_address_points_wgs84 <- st_transform(fairfax_address_points, 4326)

#fairfax_parcels <- st_read("data-raw/Fairfax County Parcels/Parcels.shp")
fairfax_parcels <- sf::read_sf('https://www.fairfaxcounty.gov/mercator/rest/services/OpenData/OpenData_A9/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json')
fairfax_parcels_wgs84 <-st_transform(fairfax_parcels, 4326)
fairfax_address_points_blocks_wgs84 <- st_join(fairfax_address_points_wgs84, fairfax_blocks_wgs84, join = st_within)

# drop geometry and set as data.table for easier filtering
fairfax_address_points_blocks <- setDT(st_drop_geometry(fairfax_address_points_blocks_wgs84))

# eliminate duplicates - Group by PARCEL_PIN and select the first one in each group
fairfax_address_points_blocks_unq <- fairfax_address_points_blocks[, .SD[1], "PARCEL_PIN"]

# merge (left Join) parcels
fairfax_parcels_blocks_wgs84 <- merge(fairfax_parcels_wgs84, fairfax_address_points_blocks_unq, by.x = "PIN", by.y = "PARCEL_PIN", all.x = TRUE)
fairfax_parcels_blocks <- fairfax_parcels_blocks_wgs84 %>% dplyr::select(PIN,GEOID10) 





# Tax files reports housings units per parcels -------------------------------------------
# upload the data
fairfax_housing_units <- sf::read_sf('https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_A6/FeatureServer/1/query?where=1%3D1&outFields=LIVUNIT,PARID&outSR=4326&f=json') 

# merge the two data
fairfax_parcels_blocks <- merge(fairfax_housing_units, fairfax_parcels_blocks, by.x='PARID', by.y='PIN')

# Remove all NA units
fairfax_parcels_blocks <- fairfax_parcels_blocks[!is.na(fairfax_parcels_blocks$LIVUNIT),]
fairfax_parcels_blocks <- fairfax_parcels_blocks[!is.na(fairfax_parcels_blocks$GEOID20),]

# save the data in the working folder (cautious: the size of the data may be too big)
fairfax_parcels_blocks <- sf::st_as_sf(fairfax_parcels_blocks)
fairfax_parcels_blocks <- fairfax_parcels_blocks %>% select(PARID,GEOID10,LIVUNIT,geometry)




# Create the parcel geoid -----------------------------------------------------------------
# parcel ID (PARID) has different length, create a uniform length by replacing all spaces with 'x'.fill text in PARID to reach the max length(PARID) (which is 14)
sf::sf_use_s2(FALSE)
temp <- fairfax_parcels_blocks %>%
  mutate(PARID=str_replace_all(PARID," ","_"),
         length=nchar(PARID),
         PARID=str_pad(PARID, max(nchar(PARID)), side="left", pad="x"),
         bg_geoid=substr(GEOID10, 1, 12)) %>%
  group_by(PARID,bg_geoid) %>%
  summarise(liv_unit=sum(LIVUNIT, na.rm=T),
            geometry = st_union(geometry))
  
temp01 <- temp %>%
  mutate(region_name=paste0("parcel ",PARID),
         region_type="parcel",
         year=format(Sys.Date(), "%Y"),
         geoid=paste0(bg_geoid,PARID)) %>%
  select(geoid,region_name,region_type,year,liv_unit)

fairfax_parcel_geometry <- temp01 %>%
  select(geoid,region_name,region_type,year)

parcel_livunit <- setDT(temp01) %>%
  select(geoid,liv_unit)


# save the parcel geometry
savepath = "Synthetic_population/Housing_units_distribution/Fairfax/data/working/"
#savepath = "Github/sdc.geographies_dev/VA/Local Geographies/Fairfax County/parcels/2022/data/working/"
st_write(fairfax_parcel_geometry, paste0(savepath,"fairfax_parcel_geometry.geojson"))
zip(zipfile = paste0(savepath,"fairfax_parcel_geometry.geojson.zip"), files = paste0(savepath,"fairfax_parcel_geometry.geojson"))
file.remove(paste0(savepath,"fairfax_parcel_geometry.geojson"))

# save the living units distribution
savepath = "Synthetic_population/Housing_units_distribution/Fairfax/data/working/"
readr::write_csv(parcel_livunit, xzfile(paste0(savepath,"va059_sdad_parcel_bg_livingunits.csv.xz"), compression = 9))

