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




# ----------------------------- Load the data
# Load housing units data
uploadpath = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"
fairfax_parcels_blocks <- st_read(unzip(paste0(uploadpath,"fairfax_parcels_blocks.geojson.zip"), paste0(uploadpath,"fairfax_parcels_blocks.geojson")))
file.remove(paste0(uploadpath,"fairfax_parcels_blocks.geojson"))

# Load ACS data
acs_data <- read.csv( paste0(uploadpath,"acs_data.csv.xz"), colClasses=c("bg_geoid"="character"))




# -------------------------------- Create a demographic multiplier for each parcel
# copy of the data
fairfax_parcels_blocks_sf <- fairfax_parcels_blocks

# set as data.table to perform easy group by aggregation
setDT(fairfax_parcels_blocks_sf)

# get count of parcels per block group: group by block group (first 12 integers of Full_Block) and get count of units per block group (sum(Total_Units))
# remove rows where nchar(substr)!=12
faifax_bg_prcel_cnt <- fairfax_parcels_blocks_sf[, .(cnt = sum(LIVUNIT)), substr(GEOID20, 1, 12)][nchar(substr)==12]

# update column names
colnames(faifax_bg_prcel_cnt) <- c("bg_geoid", "prcl_cnt")

# set va_arl_block_parcels_sf back to sf for geo functions
fairfax_parcels_blocks_sf <- sf::st_as_sf(fairfax_parcels_blocks_sf)

# create block group geoid (to make merge easier)
fairfax_parcels_blocks_sf$bg_geoid <- substr(fairfax_parcels_blocks_sf$GEOID20, 1, 12)

# merge on bg_geoid
fairfax_parcels_blocks_cnts_sf <- merge(fairfax_parcels_blocks_sf, faifax_bg_prcel_cnt, by = "bg_geoid")

# create parcel-level demographic multiplier by dividing Total_Units per parcel by total count of parcels in the block group (prcl_cnt) 
fairfax_parcels_blocks_cnts_sf$mult <- fairfax_parcels_blocks_cnts_sf$LIVUNIT/fairfax_parcels_blocks_cnts_sf$prcl_cnt




# ------------------------------- Estimates demographics at the parcels level (using ACS)
# merge with ACS data and generate parcel demographic estimates by multiplying ACS estimate by parcel multipliers
fairfax_parcels_blocks_dmgs_sf <- merge(fairfax_parcels_blocks_cnts_sf, acs_data, by='bg_geoid', allow.cartesian=TRUE)
fairfax_parcels_blocks_dmgs_sf$prcl_estimate <- fairfax_parcels_blocks_dmgs_sf$mult * fairfax_parcels_blocks_dmgs_sf$estimate




#-------------------------- generate a new geography ID for parcel (bloc group ID + rpc_master ID)
# parcel ID (PARID) has different length, create a uniform length
fairfax_parcels_blocks_dmgs_sf$PARID <- str_replace_all(fairfax_parcels_blocks_dmgs_sf$PARID," ","")
fairfax_parcels_blocks_dmgs_sf$length <- nchar(fairfax_parcels_blocks_dmgs_sf$PARID)

# PARID has different length. fill text in PARID to reach the max length(PARID) (which is 14). fill with "x" because some ID start with 0.
fairfax_parcels_blocks_dmgs_sf$PARID <- str_pad(fairfax_parcels_blocks_dmgs_sf$PARID, max(nchar(fairfax_parcels_blocks_dmgs_sf$PARID)), side="left", pad="x")

# create a parcel id and combine with geoid
fairfax_parcels_blocks_dmgs_sf$name <- paste("Parcel ",fairfax_parcels_blocks_dmgs_sf$PARID,", ",fairfax_parcels_blocks_dmgs_sf$name, sep="")
fairfax_parcels_blocks_dmgs_sf$PARID <- paste(fairfax_parcels_blocks_dmgs_sf$bg_geoid, fairfax_parcels_blocks_dmgs_sf$PARID, sep="")
fairfax_parcels_blocks_dmgs_sf <- fairfax_parcels_blocks_dmgs_sf %>% select(-bg_geoid)

# select unique geo charcateristics (name and ID) and geometry, 
fairfax_parcel_geo <- fairfax_parcels_blocks_dmgs_sf[, c("PARID","name")]
fairfax_parcel_geo_sf <- sf::st_as_sf(setDF(fairfax_parcel_geo))
fairfax_parcel_geo_sf_unq <- unique(fairfax_parcel_geo_sf)
fairfax_parcel_geo_sf_unq$year <- format(Sys.Date(), "%Y")
colnames(fairfax_parcel_geo_sf_unq) <- c("geoid", "name", "geometry","year")

# Save the ID of new geometry in distribution (compress the file)
savepath = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"
st_write(fairfax_parcel_geo_sf_unq, paste0(savepath,"fairfax_parcel_geometry.geojson"))
zip(zipfile = paste0(savepath,"fairfax_parcel_geometry.geojson.zip"), files = paste0(savepath,"fairfax_parcel_geometry.geojson"))
file.remove(paste0(savepath,"fairfax_parcel_geometry.geojson"))





# ---------------------------- Create "wide" table of demographic counts per parcel
# switch to data.table
fairfax_parcels_blocks_cnts_dmgs_dt <- data.table::as.data.table(fairfax_parcels_blocks_dmgs_sf)

# drop geometry column - huge because so many repeats and not needed here
fairfax_parcels_blocks_cnts_dmgs_dt$geometry <- NULL

# filter to needed columns
fairfax_parcels_blocks_cnts_dmgs_dt <-fairfax_parcels_blocks_cnts_dmgs_dt[, .(geoid = PARID, name=name, measure = variable, value = prcl_estimate, mult=mult)]

# Cast long file to wide
fairfax_parcels_blocks_cnts_dmgs_dt_wide <- data.table::dcast(fairfax_parcels_blocks_cnts_dmgs_dt, geoid + name + mult ~ measure, value.var = "value", fun.aggregate = sum)

# Compute the percentage (not the overall size of the population) by new geography 
fairfax_dmg_dt_geo <- fairfax_parcels_blocks_cnts_dmgs_dt_wide %>% mutate(prct_afr_amer_alone = 100*afr_amer_alone/total_pop,
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
fairfax_dmg_dt_geo <- fairfax_dmg_dt_geo %>% 
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
fairfax_dmg_dt_geo_long <-  melt(setDT(fairfax_dmg_dt_geo), id.vars = c("geoid","region_name")) %>%
  mutate(region_type='Parcel',
         year=2019,
         measure_type=case_when(
           grepl('pop',variable)==T ~ "count",
           grepl('mult',variable)==T ~ "scale",
           grepl('perc',variable)==T ~ "percentage"),
         MOE='') %>%
  select(geoid,region_type,region_name,year,measure=variable,value,measure_type,MOE)


# compress and save the data
savepath = "population/VA/fairfax/overall_fairfax/new_geography_synthetic/housing_units_distribution_method/parcels/data/working/"
readr::write_csv(fairfax_dmg_dt_geo_long, xzfile(paste0(savepath,"va059_pc_sdad_2019_demographics.csv.xz"), compression = 9))




