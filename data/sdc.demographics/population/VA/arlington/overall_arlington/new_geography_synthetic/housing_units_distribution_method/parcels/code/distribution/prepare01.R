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
uploadpath = 'population/VA/arlington/overall_arlington/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/'
va_arl_block_parcels <- st_read(unzip(paste0(uploadpath,"va_arl_block_parcels.geojson.zip"),paste0(uploadpath,"va_arl_block_parcels.geojson")))
file.remove(paste0(uploadpath,"va_arl_block_parcels.geojson"))

# Load ACS data
acs_data <- read.csv(paste0(uploadpath,"acs_data.csv.xz"), colClasses=c("bg_geoid"="character"))




# ------------------------------- Create a demographic multiplier for each parcel
# save a copy of the data
va_arl_block_parcels_sf <- va_arl_block_parcels

# set as data.table to perform easy group by aggregation
setDT(va_arl_block_parcels_sf)

# get count of parcels per block group: group by block group (first 12 integers of Full_Block) and get count of units per block group (sum(Total_Units))
# remove rows where nchar(substr)!=12
arl_bg_prcel_cnt <- va_arl_block_parcels_sf[, .(cnt = sum(Total_Units)), substr(Full_Block, 1, 12)][nchar(substr)==12]

# update column names
colnames(arl_bg_prcel_cnt) <- c("bg_geoid", "prcl_cnt")

# set va_arl_block_parcels_sf back to sf for geo functions
va_arl_block_parcels_sf <- sf::st_as_sf(va_arl_block_parcels_sf)

# create block group geoid (to make merge easier)
va_arl_block_parcels_sf$bg_geoid <- substr(va_arl_block_parcels_sf$Full_Block, 1, 12)

# merge on bg_geoid
va_arl_block_parcels_cnts_sf <- merge(va_arl_block_parcels_sf, arl_bg_prcel_cnt, by = "bg_geoid")

# create parcel-level demographic multiplier by dividing Total_Units per parcel by total count of parcels in the block group (prcl_cnt) 
va_arl_block_parcels_cnts_sf$mult <- va_arl_block_parcels_cnts_sf$Total_Units/va_arl_block_parcels_cnts_sf$prcl_cnt




# ----------------------------- Estimates demographics at the parcels level (using ACS)
# merge with ACS data and generate parcel demographic estimates by multiplying ACS estimate by parcel multipliers
va_arl_block_parcels_cnts_dmgs_sf <- merge(va_arl_block_parcels_cnts_sf, acs_data, by = "bg_geoid", allow.cartesian=TRUE)
va_arl_block_parcels_cnts_dmgs_sf$prcl_estimate <- va_arl_block_parcels_cnts_dmgs_sf$mult * va_arl_block_parcels_cnts_dmgs_sf$estimate




# -------------------------- generate a new geography ID for parcel (bloc group ID + rpc_master ID)
# Nb: rpc_master is a 8 digits parcels code ID. Create a parcel id and combine with geoid
va_arl_block_parcels_cnts_dmgs_sf$RPC_Master <- str_replace_all(va_arl_block_parcels_cnts_dmgs_sf$RPC_Master," ","")
va_arl_block_parcels_cnts_dmgs_sf$length <- nchar(va_arl_block_parcels_cnts_dmgs_sf$RPC_Master)

# we have the same length (8 digits)
va_arl_block_parcels_cnts_dmgs_sf$name <- paste("RPC_Master ",va_arl_block_parcels_cnts_dmgs_sf$RPC_Master,", ",va_arl_block_parcels_cnts_dmgs_sf$name, sep="")
va_arl_block_parcels_cnts_dmgs_sf$RPC_Master <- paste(va_arl_block_parcels_cnts_dmgs_sf$bg_geoid, va_arl_block_parcels_cnts_dmgs_sf$RPC_Master, sep="")
va_arl_block_parcels_cnts_dmgs_sf <- va_arl_block_parcels_cnts_dmgs_sf %>% select(-bg_geoid)

# select unique geo charcateristics (name and ID) and geometry, 
va_arl_parcel_geo <- va_arl_block_parcels_cnts_dmgs_sf[, c("RPC_Master","name")]
va_arl_parcel_geo_sf <- sf::st_as_sf(setDF(va_arl_parcel_geo))
va_arl_parcel_geo_sf_unq <- unique(va_arl_parcel_geo_sf)
va_arl_parcel_geo_sf_unq$year <- format(Sys.Date(), "%Y")
colnames(va_arl_parcel_geo_sf_unq) <- c("geoid", "name", "geometry","year")

# Save the ID of new geometry in distribution (compress the file)
savepath = 'population/VA/arlington/overall_arlington/new_geography_synthetic/housing_units_distribution_method/parcels/data/distribution/'
st_write(va_arl_parcel_geo_sf_unq, paste0(savepath,"va_arl_parcel_geometry.geojson"))
zip(zipfile = paste0(savepath,"va_arl_parcel_geometry.geojson.zip"), files = paste0(savepath,"va_arl_parcel_geometry.geojson"))
file.remove(paste0(savepath,"va_arl_parcel_geometry.geojson"))




# ----------------------------- Create "wide" table of demographic counts per parcel
# switch to data.table
va_arl_block_parcels_cnts_dmgs_dt <- data.table::as.data.table(va_arl_block_parcels_cnts_dmgs_sf)

# drop geometry column - huge because so many repeats and not needed here
va_arl_block_parcels_cnts_dmgs_dt$geometry <- NULL

# filter to needed columns
va_arl_block_parcels_cnts_dmgs_dt <- va_arl_block_parcels_cnts_dmgs_dt[, .(geoid = RPC_Master, name=name, measure = variable, value = prcl_estimate)]

# Cast long file to wide
va_arl_block_parcels_cnts_dmgs_dt_wide <- data.table::dcast(va_arl_block_parcels_cnts_dmgs_dt, geoid + name ~ measure, value.var = "value", fun.aggregate = sum)

# compute additional metric (percentatge)
va_arl_dmg_dt_geo <- va_arl_block_parcels_cnts_dmgs_dt_wide %>% mutate(prct_afr_amer_alone = 100*afr_amer_alone/total_pop,
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
va_arl_dmg_dt_geo <- va_arl_dmg_dt_geo %>% 
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
va_arl_dmg_dt_geo_long <-  melt(setDT(va_arl_dmg_dt_geo), id.vars = c("geoid","region_name"))
va_arl_dmg_dt_geo_long$region_type <- "Parcel"
va_arl_dmg_dt_geo_long$year <- "2019"

# order the variable
va_arl_dmg_dt_geo_long <- va_arl_dmg_dt_geo_long %>% select(geoid,region_type,region_name,year,variable,value)
colnames(va_arl_dmg_dt_geo_long) <- c("geoid", "region_type", "region_name", "year", "measure","value")

# compress and save the data
readr::write_csv(va_arl_dmg_dt_geo_long, xzfile(paste0(savepath,"va013_pc_sdad_2019_demographics.csv.xz"), compression = 9))





