library(dplyr)
library(sf)
library(httr)
library(sp)
library(data.table)
library(stringr)
library("rgdal", lib.loc="/usr/local/lib/R/site-library")
library(readr)

#---------------------- upload the data
# Load housing units data
uploadpath = "population/DC/overall_DC/new_geography_synthetic/housing_units_distribution_method/ssl/data/working/"
DC_block_ssl <- st_read(unzip(paste0(uploadpath,"DC_block_ssl.geojson.zip"),paste0(uploadpath,"DC_block_ssl.geojson")))
file.remove(paste0(uploadpath,"DC_block_ssl.geojson"))

# Load ACS data
acs_data <- read.csv(paste0(uploadpath,"acs_data.csv.xz"), colClasses=c("bg_geoid"="character"))




# ------------------------------- Create a demographic multiplier for each parcel
# upload data from fairfax estimate at the parcel level
DC_block_ssl_sf <- DC_block_ssl %>% select(SSL,GEOID,ACTIVE_RES_UNIT_COUNT)

# set as data.table to perform easy group by aggregation
setDT(DC_block_ssl_sf)

# get count of parcels per block group: group by block group (first 12 integers of Full_Block) and get count of units per block group (sum(Total_Units))
# remove rows where nchar(substr)!=10
DC_block_ssl_sf_cnt <- DC_block_ssl_sf[, .(cnt = sum(ACTIVE_RES_UNIT_COUNT)), substr(GEOID, 1, 12)][nchar(substr)==12]

# update column names
colnames(DC_block_ssl_sf_cnt) <- c("bg_geoid", "prcl_cnt")

# set va_arl_block_parcels_sf back to sf for geo functions
DC_block_ssl_sf <- sf::st_as_sf(DC_block_ssl_sf)

# create block group geoid (to make merge easier)
DC_block_ssl_sf$bg_geoid <- substr(DC_block_ssl_sf$GEOID, 1, 12)

# merge on bg_geoid
DC_block_ssl_sf <- merge(DC_block_ssl_sf, DC_block_ssl_sf_cnt, by = "bg_geoid")

# create parcel-level demographic multiplier by dividing Total_Units per parcel by total count of parcels in the block group (prcl_cnt) 
DC_block_ssl_sf$mult <- DC_block_ssl_sf$ACTIVE_RES/DC_block_ssl_sf$prcl_cnt



# ----------------------------- Estimates demographics at the parcels level (using ACS)
# merge with ACS data and generate parcel demographic estimates by multiplying ACS estimate by parcel multipliers
DC_block_ssl_dmgs_sf <- merge(DC_block_ssl_sf, acs_data, by='bg_geoid', allow.cartesian=TRUE)
DC_block_ssl_dmgs_sf$prcl_estimate <- DC_block_ssl_dmgs_sf$mult * DC_block_ssl_dmgs_sf$estimate




# -------------------------- generate a new geography ID for parcel (bloc group ID + ssl ID)
#compute the length of SSL. The length varies from 7 to 11. file the length with x.
DC_block_ssl_dmgs_sf$SSL <- str_replace_all(DC_block_ssl_dmgs_sf$SSL," ","")
DC_block_ssl_dmgs_sf$length <- nchar(DC_block_ssl_dmgs_sf$SSL)
DC_block_ssl_dmgs_sf$SSL <- str_pad(DC_block_ssl_dmgs_sf$SSL, max(nchar(DC_block_ssl_dmgs_sf$SSL)), side="left", pad="x")

# create a new name and ID for the geography
DC_block_ssl_dmgs_sf$name <- paste("SSL ",DC_block_ssl_dmgs_sf$SSL,", ",DC_block_ssl_dmgs_sf$name, sep="")
DC_block_ssl_dmgs_sf$SSL <- paste(DC_block_ssl_dmgs_sf$bg_geoid, DC_block_ssl_dmgs_sf$SSL, sep="")
DC_block_ssl_dmgs_sf <- DC_block_ssl_dmgs_sf %>% select(-bg_geoid,-GEOID)

# select unique geo charcateristics (name and ID) and geometry, 
DC_sslid_geo <- DC_block_ssl_dmgs_sf[, c("SSL","name")]
DC_sslid_geo_sf <- sf::st_as_sf(setDF(DC_sslid_geo))
DC_sslid_geo_sf_unq <- unique(DC_sslid_geo_sf)
colnames(DC_sslid_geo_sf_unq) <- c("geoid", "name", "geometry")

# Save the ID of new geometry in distribution (compress the file)
savepath = "population/DC/overall_DC/new_geography_synthetic/housing_units_distribution_method/ssl/data/distribution/"
st_write(DC_sslid_geo_sf_unq, paste0(savepath,"dc_ssl_geometry.geojson"))
zip(zipfile = paste0(savepath,"dc_ssl_geometry.geojson.zip"), files = paste0(savepath,"dc_ssl_geometry.geojson"))
file.remove(paste0(savepath,"dc_ssl_geometry.geojson"))




# ----------------------------- Create "wide" table of demographic counts per parcel
# switch to data.table
DC_block_ssl_cnts_dmgs_dt <- data.table::as.data.table(DC_block_ssl_dmgs_sf)

# drop geometry column - huge because so many repeats and not needed here
DC_block_ssl_cnts_dmgs_dt$geometry <- NULL

# filter to needed columns
DC_block_ssl_cnts_dmgs_dt <- DC_block_ssl_cnts_dmgs_dt[, .(geoid = SSL, name=name, measure = variable, value = prcl_estimate, mult=mult)]

# Cast long file to wide
DC_block_ssl_cnts_dmgs_dt_wide <- data.table::dcast(DC_block_ssl_cnts_dmgs_dt, geoid + name + mult ~ measure, value.var = "value", fun.aggregate = sum)

# compute additional metric (percentatge)
dc_dmg_dt_geo <- DC_block_ssl_cnts_dmgs_dt_wide %>% mutate(prct_afr_amer_alone = 100*afr_amer_alone/total_pop,
                                                                       prct_amr_ind_alone = 100*amr_ind_alone/total_pop,
                                                                       prct_asian_alone = 100*asian_alone/total_pop,
                                                                       prct_wht_alone = 100*wht_alone/total_pop,
                                                                       prct_hispanic = 100*hispanic/total_pop,
                                                                       prct_male = 100*male/total_pop,
                                                                       prct_female = 100*female/total_pop,
                                                                       prct_pop_under_20 = 100*pop_under_20/total_pop,
                                                                       prct_pop_20_64 = 100*pop_20_64/total_pop,
                                                                       prct_pop_65_plus = 100*pop_65_plus/total_pop)

# rename all variables
dc_dmg_dt_geo <- dc_dmg_dt_geo %>% 
  rename(region_name = "name",
         pop_black = "afr_amer_alone",
         pop_native = "amr_ind_alone",
         pop_AAPI = "asian_alone",
         pop_white = "wht_alone",
         pop_hispanic_or_latino = "hispanic",
         pop_male = "male",
         pop_female = "female",
         pop_under_20 = "pop_under_20",
         pop_20_64 = "pop_20_64",
         pop_65_plus = "pop_65_plus",
         perc_black = "prct_afr_amer_alone",
         perc_native = "prct_amr_ind_alone",
         perc_AAPI = "prct_asian_alone",
         perc_white = "prct_wht_alone",
         perc_hispanic_or_latino = "prct_hispanic",
         perc_male = "prct_male",
         perc_female = "prct_female",
         perc_under_20 = "prct_pop_under_20",
         perc_20_64 = "prct_pop_20_64",
         perc_65_plus = "prct_pop_65_plus")





# ------------------------- cast long and save the data
# Cast long and create variables
dc_dmg_dt_geo_long <-  melt(setDT(dc_dmg_dt_geo), id.vars = c("geoid","region_name"))
dc_dmg_dt_geo_long$region_type <- "Parcel"
dc_dmg_dt_geo_long$year <- "2019"

# order the variable
dc_dmg_dt_geo_long <- dc_dmg_dt_geo_long %>% select(geoid,region_type,region_name,year,variable,value)
colnames(dc_dmg_dt_geo_long) <- c("geoid", "region_type", "region_name", "year", "measure","value")

# compress and save the data
readr::write_csv(dc_dmg_dt_geo_long, xzfile(paste0(savepath,"dc001_ssl_sdad_2019_demographics.csv.xz"), compression = 9))





