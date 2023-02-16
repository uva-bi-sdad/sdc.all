library(dplyr)
library(sf)
library(httr)
library(rjson)
library(sp)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(dplyr)
library(tigris)
library(stringr)
library(data.table)



# get the housing data from opendata DC -------------------------------------------------------------
# get the housing units
va_arl_housing_units <- sf::st_read("https://arlgis.arlingtonva.us/arcgis/rest/services/Open_Data/od_MHUD_Polygons/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

# filter to unique parcel, with census block and total housing units on the parcel
va_arl_block_parcels <- unique(va_arl_housing_units[, c("RPC_Master", "Full_Block", "Total_Units")])

# save the data in the working folder (cautious: the size of the data may be too big)
va_arl_block_parcels <- sf::st_as_sf(va_arl_block_parcels)

# count the number of living units per 
sf::sf_use_s2(FALSE)
temp <- va_arl_block_parcels %>%
  mutate(RPC_Master=str_replace_all(RPC_Master," ","_"),
         length=nchar(RPC_Master),
         RPC_Master=str_pad(RPC_Master, max(nchar(RPC_Master)), side="left", pad="x"),
         bg_geoid=substr(Full_Block, 1, 12)) %>%
  group_by(RPC_Master,bg_geoid) %>%
  summarise(liv_unit=sum(Total_Units, na.rm=T),
            geometry = st_union(geometry))

temp01 <- temp %>%
  mutate(region_name=paste0("parcel ",RPC_Master),
         region_type="parcel",
         year=format(Sys.Date(), "%Y"),
         geoid=paste0(bg_geoid,RPC_Master)) %>%
  select(geoid,region_name,region_type,year,liv_unit)

arl_parcel_geometry <- temp01 %>%
  select(geoid,region_name,region_type,year)

parcel_livunit <- setDT(temp01) %>%
  select(geoid,liv_unit)


# save the data -------------------------------------------------------------------------------------
savepath = "Synthetic_population/Housing_units_distribution/Arlington/data/working/"
st_write(arl_parcel_geometry, paste0(savepath,"arl_parcel_geometry.geojson"))
zip(zipfile = paste0(savepath,"arl_parcel_geometry.geojson.zip"), files = paste0(savepath,"arl_parcel_geometry.geojson"))
file.remove(paste0(savepath,"arl_parcel_geometry.geojson"))

# save the living units distribution
savepath = "Synthetic_population/Housing_units_distribution/Arlington/data/working/"
readr::write_csv(parcel_livunit, xzfile(paste0(savepath,"va013_sdad_parcel_bg_livingunits.csv.xz"), compression = 9))




