# mergent intellect data: geo-located all the business in fairfax
#     - subset data to fairfax 
#     - add geo-references (assign to a census block using adress and census block geoid)
#
# notes the census blocks geoid changes at every census. even if the companies adress and location didn't change the geoid may have.
# note that the year goes from 2010 to 2020. So use the geoid of 2010 and 2020


# Library
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(tidygeocoder)


# upload the data --------------------------------------------------------------------------------
# upload mergent intellect companies details data
uploadpath = "ownership_diversity/VA/fairfax/overall/data/working/"
mi <-  read_csv(paste0(uploadpath,"mi_companies_details.csv.xz"))



# subset the data to the fairfax county ----------------------------------------------------------
mi_fairfax <- mi %>% 
  filter(`Physical County`=='FAIRFAX') %>% 
  select(company_name=`Company Name`, 
         minority, 
         duns = `D-U-N-S@ Number`, 
         address= `Physical Address`, 
         county= `Physical County`, 
         city=`Physical City`, 
         zipcode=`Physical Zipcode`, 
         state = `Physical State`,
         primary_naics = `Primary NAICS Code`,
         Latitude, 
         Longitude=Longtitude)

# subset to companies listed in 2020
mi_financial_info <-  read_csv(paste0(uploadpath,"mi_financial_info.csv.xz"))
mi_faifax_recent <- mi_financial_info %>% 
  select(duns=`D-U-N-S@ Number`, company_name=`Company Name`, year) %>% 
  filter(year=='2020', company_name %in% unique(mi_fairfax$company_name))

mi_faifax_recent <- unique(mi_faifax_recent[,c('duns','company_name')])
mi_fairfax2020 <- mi_fairfax %>% 
  filter(company_name %in% unique(mi_faifax_recent$company_name))



# use google map API  to correct address and get the latitude and longitude  ---------------------
# full address
mi_fairfax2020$full_address <- paste(
  str_trim(mi_fairfax2020$address),
  str_trim(mi_fairfax2020$county),
  str_trim(mi_fairfax2020$city),
  str_trim(mi_fairfax2020$zipcode),
  str_trim(mi_fairfax2020$state))

# remove white spaces
mi_fairfax2020$full_address <- str_squish(mi_fairfax2020$full_address)

# installed google api key
readRenviron("~/.Renviron")
Sys.getenv("GOOGLEGEOCODE_API_KEY")

#geocode the addresses
fairfax_lonlat <- mi_fairfax2020[1:10,] %>%
  geocode(full_address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = T)



# Assign each companies to a census blocks using lat and lon --------------------------------------

# upload the data if it is already exist
if (("mi_fairfax_google_geo.csv.xz" %in% list.files("ownership_diversity/VA/fairfax/overall/data/working/"))){
  uploadpath = "ownership_diversity/VA/fairfax/overall/data/working/"
  mi_fairfax_adress <-  read_csv(paste0(uploadpath,"mi_fairfax_google_geo.csv.xz")) %>%
    select(company_name, duns, address, formatted_address, county, city, zipcode, state, latitude, longitude) %>%
    mutate(longitude=if_else(is.na(longitude),0,longitude),
           latitude=if_else(is.na(latitude),0,latitude))
}

# get the geometry based on the latitude and longitude
mi_fairfax_sf <- st_as_sf(mi_fairfax_adress, coords = c("longitude", "latitude"), crs = 4269, agr = "constant") 

# get the geolocation of census blocks
fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(countyid=COUNTYFP,
         geoid_2010=GEOID,
         geometry)

fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(countyid=COUNTYFP,
         geoid_2020=GEOID,
         geometry)

# assign to a census block based on census year
mi_fairfax_geo2010 <- st_join(fairfax_bg2010, mi_fairfax_sf, left=F, join=st_intersects)  
temp1 <- setDT(st_drop_geometry(mi_fairfax_geo2010)) %>%
  select(company_name, duns,geoid_2010)

mi_fairfax_geo2020 <- st_join(fairfax_bg2020, mi_fairfax_sf, left=F, join=st_intersects)
temp2 <- setDT(st_drop_geometry(mi_fairfax_geo2020)) 

mi_fairfax_geo_dt <- merge(temp1, temp2, by.x=c('company_name', 'duns'), by.y=c('company_name', 'duns'))



# save the data ------------------------------------------------------------------------------------
savepath = "ownership_diversity/VA/fairfax/overall/data/working/"
readr::write_csv(mi_fairfax_geo_dt, xzfile(paste0(savepath,"mi_fairfax_geolocated.csv.xz"), compression = 9))



