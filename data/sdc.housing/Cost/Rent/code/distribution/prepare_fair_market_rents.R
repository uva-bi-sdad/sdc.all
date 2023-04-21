# Prepare HUD Fair Market Rent data

library(dplyr)
library(sf)
library(tigris)
library(readxl)
library(xlsx)
library(tidycensus)
library(tidyverse)
library(readr)

#
# NCR Counties --------------------------------
#

# ZCTA: census zip code tabulation areas (good approximations to zip codes), 2020 file is incomplete so we used 2010 file
# file has population in the intersection of zcta and county - these are 2010 numbers
# we are using the assumption that the population has grown in the same way since 2010

zip_county <- read.table("Cost/Rent/data/original/zcta_county_rel_10.txt", 
                         sep =",", header = TRUE, dec =".")

counties <- get_acs(geography = "county", variables = "B19013_001",
                    state = c("VA","MD","DC"), year = 2020, 
                    key = Sys.getenv("census_api_key"))

zip_county_va <- zip_county %>%
  filter(substr(as.character(GEOID),1,2) == "51")

zip_county <- zip_county %>%
  filter (GEOID == 24021|GEOID == 24031|GEOID == 24017|GEOID == 24033
          |GEOID == 11001|GEOID == 51107|GEOID == 51059|GEOID == 51153|
            GEOID == 51013|GEOID == 51510|GEOID == 51683|GEOID == 51600|
            GEOID == 51610|GEOID == 51685)

zip_county <- zip_county[,c(1,4,5)]
zip_county_va <- zip_county_va[,c(1,4,5)]

fy2023_safmrs_revised <- readxl::read_excel("Cost/Rent/data/original/fy2023_safmrs.xlsx")

fy2023_safmrs_revised <- fy2023_safmrs_revised[,-c(2,3,5,6,8,9,11,12,14,15,17,18)]
colnames(fy2023_safmrs_revised)[1] <- "ZCTA5"
zip_county$ZCTA5 <- as.character(zip_county$ZCTA5)
zip_county_va$ZCTA5 <- as.character(zip_county_va$ZCTA5)
fy2023_safmrs_revised_va <- left_join(zip_county_va, fy2023_safmrs_revised, by = "ZCTA5")
fy2023_safmrs_revised <- left_join(zip_county, fy2023_safmrs_revised, by = "ZCTA5")

colnames(fy2023_safmrs_revised) <- c("ZCTA5", "GEOID", "POPPT","b0","b1","b2","b3","b4")
colnames(fy2023_safmrs_revised_va) <- c("ZCTA5", "GEOID", "POPPT","b0","b1","b2","b3","b4")

# population weighted estimates ncr

housing_per_county <- data.frame(matrix(ncol = 1, nrow = 14))
housing_per_county[,1] <- c("24021","24031","24017","24033","11001","51107","51059",
                            "51153","51013","51510","51683","51600","51610","51685")
colnames(housing_per_county) <- c("GEOID")
housing_per_county$rent_0br <- 0
housing_per_county$rent_1br <- 0
housing_per_county$rent_2br <- 0
housing_per_county$rent_3br <- 0
housing_per_county$rent_4br <- 0

for (i in 1:length(housing_per_county$GEOID)){
  rent_0 <- 0
  rent_1 <- 0
  rent_2 <- 0
  rent_3 <- 0
  rent_4 <- 0
  pop <- 0
  for (j in 1:length(fy2023_safmrs_revised$GEOID)){
    if (housing_per_county$GEOID[i] == fy2023_safmrs_revised$GEOID[j]){
      rent_0 <- rent_0 + fy2023_safmrs_revised$b0[j] * fy2023_safmrs_revised$POPPT[j]
      rent_1 <- rent_1 + fy2023_safmrs_revised$b1[j] * fy2023_safmrs_revised$POPPT[j]
      rent_2 <- rent_2 + fy2023_safmrs_revised$b2[j] * fy2023_safmrs_revised$POPPT[j]
      rent_3 <- rent_3 + fy2023_safmrs_revised$b3[j] * fy2023_safmrs_revised$POPPT[j]
      rent_4 <- rent_4 + fy2023_safmrs_revised$b4[j] * fy2023_safmrs_revised$POPPT[j]
      pop <- pop + fy2023_safmrs_revised$POPPT[j]
    }
  }
  if (pop > 0){
    housing_per_county[i,2] <- rent_0/pop
    housing_per_county[i,3] <- rent_1/pop
    housing_per_county[i,4] <- rent_2/pop
    housing_per_county[i,5] <- rent_3/pop
    housing_per_county[i,6] <- rent_4/pop}
}


ncr_ct_hud_2022_housing_cost <- data.frame(matrix(ncol = 7, nrow = 5*14))
colnames(ncr_ct_hud_2022_housing_cost) <-
  c("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")
ncr_ct_hud_2022_housing_cost$year <- 2022
ncr_ct_hud_2022_housing_cost$measure_type <-"dollars"
ncr_ct_hud_2022_housing_cost$measure <- c("monthly_rent_0br","monthly_rent_1br","monthly_rent_2br",
                                          "monthly_rent_3br","monthly_rent_4br")
ncr_ct_hud_2022_housing_cost$region_type <- "county"
for (i in 1:length(housing_per_county$GEOID)){
  ncr_ct_hud_2022_housing_cost$geoid[[(i-1)*5+1]] <- 
    housing_per_county$GEOID[[i]]
  ncr_ct_hud_2022_housing_cost$geoid[[(i-1)*5+2]] <- 
    housing_per_county$GEOID[[i]]
  ncr_ct_hud_2022_housing_cost$geoid[[(i-1)*5+3]] <- 
    housing_per_county$GEOID[[i]]
  ncr_ct_hud_2022_housing_cost$geoid[[(i-1)*5+4]] <- 
    housing_per_county$GEOID[[i]]
  ncr_ct_hud_2022_housing_cost$geoid[[(i-1)*5+5]] <- 
    housing_per_county$GEOID[[i]]
}
for (i in 1:5){
  for (j in 1:length(housing_per_county$GEOID)){
    ncr_ct_hud_2022_housing_cost$value[[(j-1)*5+i]] <- 
      as.numeric(housing_per_county[j,i+1])
  }
}

ncr_ct_hud_2022_housing_cost$geoid <- as.character(ncr_ct_hud_2022_housing_cost$geoid)
ncr_ct_hud_2022_housing_cost <- merge(ncr_ct_hud_2022_housing_cost, counties, by.x = "geoid", by.y = "GEOID")
ncr_ct_hud_2022_housing_cost$region_name <- ncr_ct_hud_2022_housing_cost$NAME
ncr_ct_hud_2022_housing_cost <- ncr_ct_hud_2022_housing_cost[,c(1:7)]

# population weighted estimates va

virginia_counties <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/County/2020/data/distribution/va_geo_census_cb_2020_counties.geojson")
virginia_counties <- virginia_counties %>%
  rename(GEOID = geoid,
         NAME = region_name)

housing_per_county <- data.frame(matrix(ncol = 1, nrow = 133))
housing_per_county[,1] <- virginia_counties$GEOID

colnames(housing_per_county) <- c("GEOID")
housing_per_county$rent_0br <- 0
housing_per_county$rent_1br <- 0
housing_per_county$rent_2br <- 0
housing_per_county$rent_3br <- 0
housing_per_county$rent_4br <- 0

for (i in 1:length(housing_per_county$GEOID)){
  rent_0 <- 0
  rent_1 <- 0
  rent_2 <- 0
  rent_3 <- 0
  rent_4 <- 0
  pop <- 0
  for (j in 1:length(fy2023_safmrs_revised_va$GEOID)){
    if (housing_per_county$GEOID[i] == fy2023_safmrs_revised_va$GEOID[j]){
      rent_0 <- rent_0 + fy2023_safmrs_revised_va$b0[j] * fy2023_safmrs_revised_va$POPPT[j]
      rent_1 <- rent_1 + fy2023_safmrs_revised_va$b1[j] * fy2023_safmrs_revised_va$POPPT[j]
      rent_2 <- rent_2 + fy2023_safmrs_revised_va$b2[j] * fy2023_safmrs_revised_va$POPPT[j]
      rent_3 <- rent_3 + fy2023_safmrs_revised_va$b3[j] * fy2023_safmrs_revised_va$POPPT[j]
      rent_4 <- rent_4 + fy2023_safmrs_revised_va$b4[j] * fy2023_safmrs_revised_va$POPPT[j]
      pop <- pop + fy2023_safmrs_revised_va$POPPT[j]
    }
  }
  if (pop > 0){
    housing_per_county[i,2] <- rent_0/pop
    housing_per_county[i,3] <- rent_1/pop
    housing_per_county[i,4] <- rent_2/pop
    housing_per_county[i,5] <- rent_3/pop
    housing_per_county[i,6] <- rent_4/pop}
}

housing_county <- readxl::read_excel("Cost/Rent/data/original/FY23_FMRs.xlsx")
housing_county$GEOID <- substr(housing_county$fips, 1, 5)
housing_county <- housing_county %>%
  filter(substr(fips,1,2) == '51')
housing_county <- housing_county[,c(11,12,13,14,15,16)]
housing_per_county <- left_join(housing_per_county, housing_county, by = 'GEOID')

for (i in 1:length(housing_per_county$GEOID)){
  housing_per_county$rent_0br[i] <- ifelse( is.na(housing_per_county$rent_0br[i]), housing_per_county$fmr_0[i] , housing_per_county$rent_0br[i]  )
  housing_per_county$rent_1br[i] <- ifelse( is.na(housing_per_county$rent_1br[i]), housing_per_county$fmr_1[i] , housing_per_county$rent_1br[i]  )
  housing_per_county$rent_2br[i] <- ifelse( is.na(housing_per_county$rent_2br[i]), housing_per_county$fmr_2[i] , housing_per_county$rent_2br[i]  )
  housing_per_county$rent_3br[i] <- ifelse( is.na(housing_per_county$rent_3br[i]), housing_per_county$fmr_3[i] , housing_per_county$rent_3br[i]  )
  housing_per_county$rent_4br[i] <- ifelse( is.na(housing_per_county$rent_4br[i]), housing_per_county$fmr_4[i] , housing_per_county$rent_4br[i]  )
}

housing_per_county <- housing_per_county[,-c(7:11)]


va_ct_hud_2022_housing_cost <- data.frame(matrix(ncol = 7, nrow = 5*133))
colnames(va_ct_hud_2022_housing_cost) <-
  c("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")
va_ct_hud_2022_housing_cost$year <- 2022
va_ct_hud_2022_housing_cost$measure_type <-"dollars"
va_ct_hud_2022_housing_cost$measure <- c("monthly_rent_0br","monthly_rent_1br","monthly_rent_2br",
                                          "monthly_rent_3br","monthly_rent_4br")
va_ct_hud_2022_housing_cost$region_type <- "county"
for (i in 1:length(housing_per_county$GEOID)){
  va_ct_hud_2022_housing_cost$geoid[[(i-1)*5+1]] <- 
    housing_per_county$GEOID[[i]]
  va_ct_hud_2022_housing_cost$geoid[[(i-1)*5+2]] <- 
    housing_per_county$GEOID[[i]]
  va_ct_hud_2022_housing_cost$geoid[[(i-1)*5+3]] <- 
    housing_per_county$GEOID[[i]]
  va_ct_hud_2022_housing_cost$geoid[[(i-1)*5+4]] <- 
    housing_per_county$GEOID[[i]]
  va_ct_hud_2022_housing_cost$geoid[[(i-1)*5+5]] <- 
    housing_per_county$GEOID[[i]]
}
for (i in 1:5){
  for (j in 1:length(housing_per_county$GEOID)){
    va_ct_hud_2022_housing_cost$value[[(j-1)*5+i]] <- 
      as.numeric(housing_per_county[j,i+1])
  }
}

va_ct_hud_2022_housing_cost$geoid <- as.character(va_ct_hud_2022_housing_cost$geoid)
va_ct_hud_2022_housing_cost <- merge(va_ct_hud_2022_housing_cost, counties, by.x = "geoid", by.y = "GEOID")
va_ct_hud_2022_housing_cost$region_name <- va_ct_hud_2022_housing_cost$NAME
va_ct_hud_2022_housing_cost <- va_ct_hud_2022_housing_cost[,c(1:7)]

dc_md <- ncr_ct_hud_2022_housing_cost %>%
  filter(substr(geoid,1,2) != "51")
county_level_combined <- rbind(va_ct_hud_2022_housing_cost, dc_md)
#
# DC tracts ----------------------------------------
#

dc_tracts <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/DC/Census%20Geographies/Tract/2020/data/distribution/dc_geo_census_cb_2020_census_tracts.geojson")
dc_tracts <- dc_tracts %>%
  rename(NAME = region_name)

zip_pop <- get_acs(geography = "zcta", variables = "DP05_0001E",
                   year = 2021,
                   key = Sys.getenv("census_api_key"))

fy2023_safmrs_revised <- readxl::read_excel("Cost/Rent/data/original/fy2023_safmrs.xlsx")

fy2023_safmrs_revised <- fy2023_safmrs_revised[,-c(2,3,5,6,8,9,11,12,14,15,17,18)]
colnames(fy2023_safmrs_revised)[1] <- "zip"
zip_tract_crosswalk_2020 <- read_csv("Cost/Rent/data/original/ZIP_TRACT_122021.csv") %>%
  rename(tract = geoid)
zip_tract_crosswalk_2020 <- zip_tract_crosswalk_2020[,c(1,2)]

zip_tract_crosswalk_2020$zip <- as.character(zip_tract_crosswalk_2020$zip)
zip_tract_crosswalk_2020 <- zip_tract_crosswalk_2020 %>%
  filter(substr(tract,1,2) == '11')
fy2023_safmrs_revised <- left_join(zip_tract_crosswalk_2020, fy2023_safmrs_revised, by = "zip")
colnames(zip_pop)[1] <- "zip"
zip_pop <- zip_pop[,c(1,4)]
fy2023_safmrs_revised <- left_join(fy2023_safmrs_revised, zip_pop, by = "zip")


housing_per_tract <- unique(dc_tracts[,1])
housing_per_tract <- dplyr::select(as.data.frame(housing_per_tract), -geometry)
housing_per_tract$rent_0br <- 0
housing_per_tract$rent_1br <- 0
housing_per_tract$rent_2br <- 0
housing_per_tract$rent_3br <- 0
housing_per_tract$rent_4br <- 0

colnames(fy2023_safmrs_revised) <- c("zip", "tract", "b0","b1","b2","b3","b4","zip_population")


# getting tract data from zip code data, weighted by zip code populations

for (i in 1:length(housing_per_tract$geoid)){
  rent_0 <- 0
  rent_1 <- 0
  rent_2 <- 0
  rent_3 <- 0
  rent_4 <- 0
  pop <- 0
  for (j in 1:length(fy2023_safmrs_revised$tract)){
    if (housing_per_tract$geoid[i] == fy2023_safmrs_revised$tract[j]){
      if (is.na(fy2023_safmrs_revised$zip_population[j]) == FALSE){
        rent_0 <- rent_0 + fy2023_safmrs_revised$b0[j] * fy2023_safmrs_revised$zip_population[j]
        rent_1 <- rent_1 + fy2023_safmrs_revised$b1[j] * fy2023_safmrs_revised$zip_population[j]
        rent_2 <- rent_2 + fy2023_safmrs_revised$b2[j] * fy2023_safmrs_revised$zip_population[j]
        rent_3 <- rent_3 + fy2023_safmrs_revised$b3[j] * fy2023_safmrs_revised$zip_population[j]
        rent_4 <- rent_4 + fy2023_safmrs_revised$b4[j] * fy2023_safmrs_revised$zip_population[j]
        pop <- pop + fy2023_safmrs_revised$zip_population[j]
      }
    }
  }
  #print(pop)
  if (pop > 0){
    housing_per_tract[i,2] <- rent_0/pop
    housing_per_tract[i,3] <- rent_1/pop
    housing_per_tract[i,4] <- rent_2/pop
    housing_per_tract[i,5] <- rent_3/pop
    housing_per_tract[i,6] <- rent_4/pop}
}

housing_per_tract$geoid <- as.numeric(housing_per_tract$geoid)

housing_county <- readxl::read_excel("Cost/Rent/data/original/FY23_FMRs.xlsx")
housing_county$fips <- substr(housing_county$fips, 1, 5)
housing_county <- housing_county %>%
  filter(substr(fips,1,2) == '11')
housing_county$fips <- as.numeric(housing_county$fips)
housing_county <- housing_county[,c(2,11,12,13,14,15)]

housing_per_tract$fips <- as.numeric(substr(housing_per_tract$geoid, 1, 5))
housing_per_tract <- left_join(housing_per_tract, housing_county, by = 'fips')

# if tract data is missing or 0, fill in county number

housing_per_tract$avg_county_cost0 <- 0
housing_per_tract$avg_county_cost1 <- 0
housing_per_tract$avg_county_cost2 <- 0
housing_per_tract$avg_county_cost3 <- 0
housing_per_tract$avg_county_cost4 <- 0

for (i in 1:length(housing_per_tract$geoid)){
  if (match(as.character(substr(housing_per_tract$geoid[i],1,5)), county_level_combined$geoid, nomatch = 0) != 0){
    x <- match(as.character(substr(housing_per_tract$geoid[i],1,5)), county_level_combined$geoid, nomatch = 0)
    housing_per_tract$avg_county_cost0[i] <- county_level_combined$value[x]
    housing_per_tract$avg_county_cost1[i] <- county_level_combined$value[x + 1]
    housing_per_tract$avg_county_cost2[i] <- county_level_combined$value[x + 2]
    housing_per_tract$avg_county_cost3[i] <- county_level_combined$value[x + 3]
    housing_per_tract$avg_county_cost4[i] <- county_level_combined$value[x + 4]
}}

for (i in 1:length(housing_per_tract$geoid)){
  housing_per_tract$rent_0br[i] <- ifelse( housing_per_tract$rent_0br[i] == 0, housing_per_tract$avg_county_cost0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( housing_per_tract$rent_1br[i] == 0, housing_per_tract$avg_county_cost1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( housing_per_tract$rent_2br[i] == 0, housing_per_tract$avg_county_cost2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( housing_per_tract$rent_3br[i] == 0, housing_per_tract$avg_county_cost3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( housing_per_tract$rent_4br[i] == 0, housing_per_tract$avg_county_cost4[i] , housing_per_tract$rent_4br[i]  )
  
  housing_per_tract$rent_0br[i] <- ifelse( is.na(housing_per_tract$rent_0br[i]), housing_per_tract$avg_county_cost0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( is.na(housing_per_tract$rent_1br[i]), housing_per_tract$avg_county_cost1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( is.na(housing_per_tract$rent_2br[i]), housing_per_tract$avg_county_cost2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( is.na(housing_per_tract$rent_3br[i]), housing_per_tract$avg_county_cost3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( is.na(housing_per_tract$rent_4br[i]), housing_per_tract$avg_county_cost4[i] , housing_per_tract$rent_4br[i]  )
  
  
  housing_per_tract$rent_0br[i] <- ifelse( is.na(housing_per_tract$rent_0br[i]), housing_per_tract$fmr_0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( is.na(housing_per_tract$rent_1br[i]), housing_per_tract$fmr_1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( is.na(housing_per_tract$rent_2br[i]), housing_per_tract$fmr_2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( is.na(housing_per_tract$rent_3br[i]), housing_per_tract$fmr_3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( is.na(housing_per_tract$rent_4br[i]), housing_per_tract$fmr_4[i] , housing_per_tract$rent_4br[i]  )
  
  housing_per_tract$rent_0br[i] <- ifelse( housing_per_tract$rent_0br[i] == 0, housing_per_tract$fmr_0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( housing_per_tract$rent_1br[i] == 0, housing_per_tract$fmr_1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( housing_per_tract$rent_2br[i] == 0, housing_per_tract$fmr_2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( housing_per_tract$rent_3br[i] == 0, housing_per_tract$fmr_3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( housing_per_tract$rent_4br[i] == 0, housing_per_tract$fmr_4[i] , housing_per_tract$rent_4br[i]  )
}

housing_per_tract <- housing_per_tract[,-c(7:12)]

dc_tr_hud_2022_housing_cost_imputations_short_form <- housing_per_tract 
dc_tr_hud_2022_housing_cost <- data.frame(matrix(ncol = 7, nrow = 5*206))
colnames(dc_tr_hud_2022_housing_cost) <-
  c("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")
dc_tr_hud_2022_housing_cost$year <- 2022
dc_tr_hud_2022_housing_cost$measure_type <-"dollars"
dc_tr_hud_2022_housing_cost$measure <- c("monthly_rent_0br","monthly_rent_1br","monthly_rent_2br",
                                         "monthly_rent_3br","monthly_rent_4br")
dc_tr_hud_2022_housing_cost$region_type <- "tract"
for (i in 1:length(dc_tr_hud_2022_housing_cost_imputations_short_form$geoid)){
  dc_tr_hud_2022_housing_cost$geoid[[(i-1)*5+1]] <- 
    dc_tr_hud_2022_housing_cost_imputations_short_form$geoid[[i]]
  dc_tr_hud_2022_housing_cost$geoid[[(i-1)*5+2]] <- 
    dc_tr_hud_2022_housing_cost_imputations_short_form$geoid[[i]]
  dc_tr_hud_2022_housing_cost$geoid[[(i-1)*5+3]] <- 
    dc_tr_hud_2022_housing_cost_imputations_short_form$geoid[[i]]
  dc_tr_hud_2022_housing_cost$geoid[[(i-1)*5+4]] <- 
    dc_tr_hud_2022_housing_cost_imputations_short_form$geoid[[i]]
  dc_tr_hud_2022_housing_cost$geoid[[(i-1)*5+5]] <- 
    dc_tr_hud_2022_housing_cost_imputations_short_form$geoid[[i]]
}
for (i in 1:5){
  for (j in 1:length(dc_tr_hud_2022_housing_cost_imputations_short_form$geoid)){
    dc_tr_hud_2022_housing_cost$value[[(j-1)*5+i]] <- 
      as.numeric(dc_tr_hud_2022_housing_cost_imputations_short_form[j,i+1])
  }
}

dc_tr_hud_2022_housing_cost$geoid <- as.character(dc_tr_hud_2022_housing_cost$geoid)
dc_tr_hud_2022_housing_cost <- merge(dc_tr_hud_2022_housing_cost, dc_tracts[, c(1:2)], by = "geoid")
dc_tr_hud_2022_housing_cost$region_name <- dc_tr_hud_2022_housing_cost$NAME
dc_tr_hud_2022_housing_cost <- dc_tr_hud_2022_housing_cost[,c(1:7)]


#
# MD tracts ----------------------------------------
#

maryland_tracts <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/MD/Census%20Geographies/Tract/2020/data/distribution/md_geo_census_cb_2020_census_tracts.geojson")
maryland_tracts <- maryland_tracts %>%
  rename(GEOID = geoid,
    NAME = region_name)

zip_pop <- get_acs(geography = "zcta", variables = "DP05_0001E",
                   year = 2021,
                   key = Sys.getenv("census_api_key"))

fy2023_safmrs_revised <- readxl::read_excel("Cost/Rent/data/original/fy2023_safmrs.xlsx")  # tract data

fy2023_safmrs_revised <- fy2023_safmrs_revised[,-c(2,3,5,6,8,9,11,12,14,15,17,18)]
colnames(fy2023_safmrs_revised)[1] <- "zip"
zip_tract_crosswalk_2020 <- read_csv("Cost/Rent/data/original/ZIP_TRACT_122021.csv") %>%
  rename(tract = geoid)
zip_tract_crosswalk_2020 <- zip_tract_crosswalk_2020[,c(1,2)]

zip_tract_crosswalk_2020$zip <- as.character(zip_tract_crosswalk_2020$zip)
zip_tract_crosswalk_2020 <- zip_tract_crosswalk_2020 %>%
  filter(substr(tract,1,2) == '24')
fy2023_safmrs_revised <- left_join(zip_tract_crosswalk_2020, fy2023_safmrs_revised, by = "zip")
colnames(zip_pop)[1] <- "zip"
zip_pop <- zip_pop[,c(1,4)]
fy2023_safmrs_revised <- left_join(fy2023_safmrs_revised, zip_pop, by = "zip")

housing_per_tract <- unique(maryland_tracts[,1])
housing_per_tract <- dplyr::select(as.data.frame(housing_per_tract), -geometry)
housing_per_tract$rent_0br <- 0
housing_per_tract$rent_1br <- 0
housing_per_tract$rent_2br <- 0
housing_per_tract$rent_3br <- 0
housing_per_tract$rent_4br <- 0

colnames(fy2023_safmrs_revised) <- c("zip", "tract", "b0","b1","b2","b3","b4","zip_population")


# getting tract data from zip code data, weighted by zip code populations

for (i in 1:length(housing_per_tract$GEOID)){
  rent_0 <- 0
  rent_1 <- 0
  rent_2 <- 0
  rent_3 <- 0
  rent_4 <- 0
  pop <- 0
  for (j in 1:length(fy2023_safmrs_revised$tract)){
    if (housing_per_tract$GEOID[i] == fy2023_safmrs_revised$tract[j]){
      if (is.na(fy2023_safmrs_revised$zip_population[j]) == FALSE){
        rent_0 <- rent_0 + fy2023_safmrs_revised$b0[j] * fy2023_safmrs_revised$zip_population[j]
        rent_1 <- rent_1 + fy2023_safmrs_revised$b1[j] * fy2023_safmrs_revised$zip_population[j]
        rent_2 <- rent_2 + fy2023_safmrs_revised$b2[j] * fy2023_safmrs_revised$zip_population[j]
        rent_3 <- rent_3 + fy2023_safmrs_revised$b3[j] * fy2023_safmrs_revised$zip_population[j]
        rent_4 <- rent_4 + fy2023_safmrs_revised$b4[j] * fy2023_safmrs_revised$zip_population[j]
        pop <- pop + fy2023_safmrs_revised$zip_population[j]
      }
    }
  }
  #print(pop)
  if (pop > 0){
    housing_per_tract[i,2] <- rent_0/pop
    housing_per_tract[i,3] <- rent_1/pop
    housing_per_tract[i,4] <- rent_2/pop
    housing_per_tract[i,5] <- rent_3/pop
    housing_per_tract[i,6] <- rent_4/pop}
}

housing_per_tract$GEOID <- as.numeric(housing_per_tract$GEOID)

housing_county <- readxl::read_excel("Cost/Rent/data/original/FY23_FMRs.xlsx")  # county data
housing_county$fips <- substr(housing_county$fips, 1, 5)
housing_county <- housing_county %>%
  filter(substr(fips,1,2) == '24')
housing_county$fips <- as.numeric(housing_county$fips)
housing_county <- housing_county[,c(2,11,12,13,14,15)]

housing_per_tract$fips <- as.numeric(substr(housing_per_tract$GEOID, 1, 5))
housing_per_tract <- left_join(housing_per_tract, housing_county, by = 'fips')

# if tract data is missing or 0, fill in county number

housing_per_tract$avg_county_cost0 <- 0
housing_per_tract$avg_county_cost1 <- 0
housing_per_tract$avg_county_cost2 <- 0
housing_per_tract$avg_county_cost3 <- 0
housing_per_tract$avg_county_cost4 <- 0

for (i in 1:length(housing_per_tract$GEOID)){
  if (match(as.character(substr(housing_per_tract$GEOID[i],1,5)), county_level_combined$geoid, nomatch = 0) != 0){
    x <- match(as.character(substr(housing_per_tract$GEOID[i],1,5)), county_level_combined$geoid, nomatch = 0)
    housing_per_tract$avg_county_cost0[i] <- county_level_combined$value[x]
    housing_per_tract$avg_county_cost1[i] <- county_level_combined$value[x + 1]
    housing_per_tract$avg_county_cost2[i] <- county_level_combined$value[x + 2]
    housing_per_tract$avg_county_cost3[i] <- county_level_combined$value[x + 3]
    housing_per_tract$avg_county_cost4[i] <- county_level_combined$value[x + 4]
  }}

for (i in 1:length(housing_per_tract$GEOID)){
  housing_per_tract$rent_0br[i] <- ifelse( housing_per_tract$rent_0br[i] == 0, housing_per_tract$avg_county_cost0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( housing_per_tract$rent_1br[i] == 0, housing_per_tract$avg_county_cost1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( housing_per_tract$rent_2br[i] == 0, housing_per_tract$avg_county_cost2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( housing_per_tract$rent_3br[i] == 0, housing_per_tract$avg_county_cost3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( housing_per_tract$rent_4br[i] == 0, housing_per_tract$avg_county_cost4[i] , housing_per_tract$rent_4br[i]  )
  
  housing_per_tract$rent_0br[i] <- ifelse( is.na(housing_per_tract$rent_0br[i]), housing_per_tract$avg_county_cost0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( is.na(housing_per_tract$rent_1br[i]), housing_per_tract$avg_county_cost1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( is.na(housing_per_tract$rent_2br[i]), housing_per_tract$avg_county_cost2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( is.na(housing_per_tract$rent_3br[i]), housing_per_tract$avg_county_cost3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( is.na(housing_per_tract$rent_4br[i]), housing_per_tract$avg_county_cost4[i] , housing_per_tract$rent_4br[i]  )
  
  
  housing_per_tract$rent_0br[i] <- ifelse( is.na(housing_per_tract$rent_0br[i]), housing_per_tract$fmr_0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( is.na(housing_per_tract$rent_1br[i]), housing_per_tract$fmr_1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( is.na(housing_per_tract$rent_2br[i]), housing_per_tract$fmr_2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( is.na(housing_per_tract$rent_3br[i]), housing_per_tract$fmr_3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( is.na(housing_per_tract$rent_4br[i]), housing_per_tract$fmr_4[i] , housing_per_tract$rent_4br[i]  )
  
  housing_per_tract$rent_0br[i] <- ifelse( housing_per_tract$rent_0br[i] == 0, housing_per_tract$fmr_0[i] , housing_per_tract$rent_0br[i]  )
  housing_per_tract$rent_1br[i] <- ifelse( housing_per_tract$rent_1br[i] == 0, housing_per_tract$fmr_1[i] , housing_per_tract$rent_1br[i]  )
  housing_per_tract$rent_2br[i] <- ifelse( housing_per_tract$rent_2br[i] == 0, housing_per_tract$fmr_2[i] , housing_per_tract$rent_2br[i]  )
  housing_per_tract$rent_3br[i] <- ifelse( housing_per_tract$rent_3br[i] == 0, housing_per_tract$fmr_3[i] , housing_per_tract$rent_3br[i]  )
  housing_per_tract$rent_4br[i] <- ifelse( housing_per_tract$rent_4br[i] == 0, housing_per_tract$fmr_4[i] , housing_per_tract$rent_4br[i]  )
}

housing_per_tract <- housing_per_tract[,-c(7:12)]

md_tr_hud_2022_housing_cost_imputations_short_form <- housing_per_tract 
md_tr_hud_2022_housing_cost <- data.frame(matrix(ncol = 7, nrow = 5*1465))
colnames(md_tr_hud_2022_housing_cost) <-
  c("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")
md_tr_hud_2022_housing_cost$year <- 2022
md_tr_hud_2022_housing_cost$measure_type <-"dollars"
md_tr_hud_2022_housing_cost$measure <- c("monthly_rent_0br","monthly_rent_1br","monthly_rent_2br",
                                         "monthly_rent_3br","monthly_rent_4br")
md_tr_hud_2022_housing_cost$region_type <- "tract"
for (i in 1:length(md_tr_hud_2022_housing_cost_imputations_short_form$GEOID)){
  md_tr_hud_2022_housing_cost$geoid[[(i-1)*5+1]] <- 
    md_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  md_tr_hud_2022_housing_cost$geoid[[(i-1)*5+2]] <- 
    md_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  md_tr_hud_2022_housing_cost$geoid[[(i-1)*5+3]] <- 
    md_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  md_tr_hud_2022_housing_cost$geoid[[(i-1)*5+4]] <- 
    md_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  md_tr_hud_2022_housing_cost$geoid[[(i-1)*5+5]] <- 
    md_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
}
for (i in 1:5){
  for (j in 1:length(md_tr_hud_2022_housing_cost_imputations_short_form$GEOID)){
    md_tr_hud_2022_housing_cost$value[[(j-1)*5+i]] <- 
      as.numeric(md_tr_hud_2022_housing_cost_imputations_short_form[j,i+1])
  }
}

md_tr_hud_2022_housing_cost$geoid <- as.character(md_tr_hud_2022_housing_cost$geoid)
md_tr_hud_2022_housing_cost <- merge(md_tr_hud_2022_housing_cost, maryland_tracts[, c(1,2)], by.x = "geoid", by.y = "GEOID")
md_tr_hud_2022_housing_cost$region_name <- md_tr_hud_2022_housing_cost$NAME
md_tr_hud_2022_housing_cost <- md_tr_hud_2022_housing_cost[,c(1:7)]


#
# VA tracts ----------------------------------------
#

virginia_tracts <- sf::st_read("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Tract/2020/data/distribution/va_geo_census_cb_2020_census_tracts.geojson")
virginia_tracts <- virginia_tracts %>%
  rename(GEOID = geoid,
         NAME = region_name)

zip_pop <- get_acs(geography = "zcta", variables = "DP05_0001E",
                   year = 2021,
                   key = Sys.getenv("census_api_key"))

fy2023_safmrs_revised <- readxl::read_excel("Cost/Rent/data/original/fy2023_safmrs.xlsx")

fy2023_safmrs_revised <- fy2023_safmrs_revised[,-c(2,3,5,6,8,9,11,12,14,15,17,18)]
colnames(fy2023_safmrs_revised)[1] <- "zip"
zip_tract_crosswalk_2020 <- read_csv("Cost/Rent/data/original/ZIP_TRACT_122021.csv") %>%
  rename(tract = geoid)
zip_tract_crosswalk_2020 <- zip_tract_crosswalk_2020[,c(1,2)]

zip_tract_crosswalk_2020$zip <- as.character(zip_tract_crosswalk_2020$zip)
zip_tract_crosswalk_2020 <- zip_tract_crosswalk_2020 %>%
  filter(substr(tract,1,2) == '51')
fy2023_safmrs_revised <- left_join(zip_tract_crosswalk_2020, fy2023_safmrs_revised, by = "zip")
colnames(zip_pop)[1] <- "zip"
zip_pop <- zip_pop[,c(1,4)]
fy2023_safmrs_revised <- left_join(fy2023_safmrs_revised, zip_pop, by = "zip")

housing_per_tract <- unique(virginia_tracts[,1])
housing_per_tract <- dplyr::select(as.data.frame(housing_per_tract), -geometry)
housing_per_tract$rent_0br <- NA
housing_per_tract$rent_1br <- NA
housing_per_tract$rent_2br <- NA
housing_per_tract$rent_3br <- NA
housing_per_tract$rent_4br <- NA

colnames(fy2023_safmrs_revised) <- c("zip", "tract", "b0","b1","b2","b3","b4","zip_population")


# getting tract data from zip code data, weighted by zip code populations

for (i in 1:length(housing_per_tract$GEOID)){
  rent_0 <- 0
  rent_1 <- 0
  rent_2 <- 0
  rent_3 <- 0
  rent_4 <- 0
  pop <- 0
  for (j in 1:length(fy2023_safmrs_revised$tract)){
    if (housing_per_tract$GEOID[i] == fy2023_safmrs_revised$tract[j]){
      if (is.na(fy2023_safmrs_revised$zip_population[j]) == FALSE){
        rent_0 <- rent_0 + fy2023_safmrs_revised$b0[j] * fy2023_safmrs_revised$zip_population[j]
        rent_1 <- rent_1 + fy2023_safmrs_revised$b1[j] * fy2023_safmrs_revised$zip_population[j]
        rent_2 <- rent_2 + fy2023_safmrs_revised$b2[j] * fy2023_safmrs_revised$zip_population[j]
        rent_3 <- rent_3 + fy2023_safmrs_revised$b3[j] * fy2023_safmrs_revised$zip_population[j]
        rent_4 <- rent_4 + fy2023_safmrs_revised$b4[j] * fy2023_safmrs_revised$zip_population[j]
        pop <- pop + fy2023_safmrs_revised$zip_population[j]
      }
    }
  }
  #print(pop)
  if (pop > 0){
    housing_per_tract[i,2] <- rent_0/pop
    housing_per_tract[i,3] <- rent_1/pop
    housing_per_tract[i,4] <- rent_2/pop
    housing_per_tract[i,5] <- rent_3/pop
    housing_per_tract[i,6] <- rent_4/pop}
}

housing_per_tract$GEOID <- as.numeric(housing_per_tract$GEOID)

housing_county <- readxl::read_excel("Cost/Rent/data/original/FY23_FMRs.xlsx")
housing_county$fips <- substr(housing_county$fips, 1, 5)
housing_county <- housing_county %>%
  filter(substr(fips,1,2) == '51')
housing_county$fips <- as.numeric(housing_county$fips)
housing_county <- housing_county[,c(2,11,12,13,14,15)]

housing_per_tract$fips <- as.numeric(substr(housing_per_tract$GEOID, 1, 5))
housing_per_tract <- left_join(housing_per_tract, housing_county, by = 'fips')

# if tract data is missing or 0, fill in county number

housing_per_tract$avg_county_cost0 <- 0
housing_per_tract$avg_county_cost1 <- 0
housing_per_tract$avg_county_cost2 <- 0
housing_per_tract$avg_county_cost3 <- 0
housing_per_tract$avg_county_cost4 <- 0

for (i in 1:length(housing_per_tract$GEOID)){
  if (match(as.character(substr(housing_per_tract$GEOID[i],1,5)), county_level_combined$geoid, nomatch = 0) != 0){
    x <- match(as.character(substr(housing_per_tract$GEOID[i],1,5)), county_level_combined$geoid, nomatch = 0)
    housing_per_tract$avg_county_cost0[i] <- county_level_combined$value[x]
    housing_per_tract$avg_county_cost1[i] <- county_level_combined$value[x + 1]
    housing_per_tract$avg_county_cost2[i] <- county_level_combined$value[x + 2]
    housing_per_tract$avg_county_cost3[i] <- county_level_combined$value[x + 3]
    housing_per_tract$avg_county_cost4[i] <- county_level_combined$value[x + 4]
  }}

for (i in 1:length(housing_per_tract$GEOID)){
housing_per_tract$rent_0br[i] <- ifelse( housing_per_tract$rent_0br[i] == 0, housing_per_tract$avg_county_cost0[i] , housing_per_tract$rent_0br[i]  )
housing_per_tract$rent_1br[i] <- ifelse( housing_per_tract$rent_1br[i] == 0, housing_per_tract$avg_county_cost1[i] , housing_per_tract$rent_1br[i]  )
housing_per_tract$rent_2br[i] <- ifelse( housing_per_tract$rent_2br[i] == 0, housing_per_tract$avg_county_cost2[i] , housing_per_tract$rent_2br[i]  )
housing_per_tract$rent_3br[i] <- ifelse( housing_per_tract$rent_3br[i] == 0, housing_per_tract$avg_county_cost3[i] , housing_per_tract$rent_3br[i]  )
housing_per_tract$rent_4br[i] <- ifelse( housing_per_tract$rent_4br[i] == 0, housing_per_tract$avg_county_cost4[i] , housing_per_tract$rent_4br[i]  )

housing_per_tract$rent_0br[i] <- ifelse( is.na(housing_per_tract$rent_0br[i]), housing_per_tract$avg_county_cost0[i] , housing_per_tract$rent_0br[i]  )
housing_per_tract$rent_1br[i] <- ifelse( is.na(housing_per_tract$rent_1br[i]), housing_per_tract$avg_county_cost1[i] , housing_per_tract$rent_1br[i]  )
housing_per_tract$rent_2br[i] <- ifelse( is.na(housing_per_tract$rent_2br[i]), housing_per_tract$avg_county_cost2[i] , housing_per_tract$rent_2br[i]  )
housing_per_tract$rent_3br[i] <- ifelse( is.na(housing_per_tract$rent_3br[i]), housing_per_tract$avg_county_cost3[i] , housing_per_tract$rent_3br[i]  )
housing_per_tract$rent_4br[i] <- ifelse( is.na(housing_per_tract$rent_4br[i]), housing_per_tract$avg_county_cost4[i] , housing_per_tract$rent_4br[i]  )


housing_per_tract$rent_0br[i] <- ifelse( is.na(housing_per_tract$rent_0br[i]), housing_per_tract$fmr_0[i] , housing_per_tract$rent_0br[i]  )
housing_per_tract$rent_1br[i] <- ifelse( is.na(housing_per_tract$rent_1br[i]), housing_per_tract$fmr_1[i] , housing_per_tract$rent_1br[i]  )
housing_per_tract$rent_2br[i] <- ifelse( is.na(housing_per_tract$rent_2br[i]), housing_per_tract$fmr_2[i] , housing_per_tract$rent_2br[i]  )
housing_per_tract$rent_3br[i] <- ifelse( is.na(housing_per_tract$rent_3br[i]), housing_per_tract$fmr_3[i] , housing_per_tract$rent_3br[i]  )
housing_per_tract$rent_4br[i] <- ifelse( is.na(housing_per_tract$rent_4br[i]), housing_per_tract$fmr_4[i] , housing_per_tract$rent_4br[i]  )

housing_per_tract$rent_0br[i] <- ifelse( housing_per_tract$rent_0br[i] == 0, housing_per_tract$fmr_0[i] , housing_per_tract$rent_0br[i]  )
housing_per_tract$rent_1br[i] <- ifelse( housing_per_tract$rent_1br[i] == 0, housing_per_tract$fmr_1[i] , housing_per_tract$rent_1br[i]  )
housing_per_tract$rent_2br[i] <- ifelse( housing_per_tract$rent_2br[i] == 0, housing_per_tract$fmr_2[i] , housing_per_tract$rent_2br[i]  )
housing_per_tract$rent_3br[i] <- ifelse( housing_per_tract$rent_3br[i] == 0, housing_per_tract$fmr_3[i] , housing_per_tract$rent_3br[i]  )
housing_per_tract$rent_4br[i] <- ifelse( housing_per_tract$rent_4br[i] == 0, housing_per_tract$fmr_4[i] , housing_per_tract$rent_4br[i]  )
}

housing_per_tract <- housing_per_tract[,-c(7:12)]

va_tr_hud_2022_housing_cost_imputations_short_form <- housing_per_tract 
va_tr_hud_2022_housing_cost <- data.frame(matrix(ncol = 7, nrow = 5*2190))
colnames(va_tr_hud_2022_housing_cost) <-
  c("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")
va_tr_hud_2022_housing_cost$year <- 2022
va_tr_hud_2022_housing_cost$measure_type <-"dollars"
va_tr_hud_2022_housing_cost$measure <- c("monthly_rent_0br","monthly_rent_1br","monthly_rent_2br",
                                         "monthly_rent_3br","monthly_rent_4br")
va_tr_hud_2022_housing_cost$region_type <- "tract"
for (i in 1:length(va_tr_hud_2022_housing_cost_imputations_short_form$GEOID)){
  va_tr_hud_2022_housing_cost$geoid[[(i-1)*5+1]] <- 
    va_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  va_tr_hud_2022_housing_cost$geoid[[(i-1)*5+2]] <- 
    va_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  va_tr_hud_2022_housing_cost$geoid[[(i-1)*5+3]] <- 
    va_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  va_tr_hud_2022_housing_cost$geoid[[(i-1)*5+4]] <- 
    va_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
  va_tr_hud_2022_housing_cost$geoid[[(i-1)*5+5]] <- 
    va_tr_hud_2022_housing_cost_imputations_short_form$GEOID[[i]]
}
for (i in 1:5){
  for (j in 1:length(va_tr_hud_2022_housing_cost_imputations_short_form$GEOID)){
    va_tr_hud_2022_housing_cost$value[[(j-1)*5+i]] <- 
      as.numeric(va_tr_hud_2022_housing_cost_imputations_short_form[j,i+1])
  }
}

va_tr_hud_2022_housing_cost$geoid <- as.character(va_tr_hud_2022_housing_cost$geoid)
va_tr_hud_2022_housing_cost <- merge(va_tr_hud_2022_housing_cost, virginia_tracts[, c(1,2)], by.x = "geoid", by.y = "GEOID")
va_tr_hud_2022_housing_cost$region_name <- va_tr_hud_2022_housing_cost$NAME
va_tr_hud_2022_housing_cost <- va_tr_hud_2022_housing_cost[,c(1:7)]



#
####Combine into one table####
#

dc_housing <- dc_tr_hud_2022_housing_cost
md_housing <- md_tr_hud_2022_housing_cost
va_housing <- va_tr_hud_2022_housing_cost

dc_housing$moe <- ""
md_housing$moe <- ""
va_housing$moe <- ""
county_level_combined$moe <- ""

housing_data <- rbind(county_level_combined, dc_housing, md_housing, va_housing)

# filter to NCR
ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")
ncr_housing <- housing_data %>% dplyr::filter(str_detect(geoid, ncr_counties))

write_csv(ncr_housing, xzfile("Cost/Rent/data/distribution/ncr_cttr_hud_2022_housing_cost.csv.xz", compression = 9))

#filter to VA
va_housing <- housing_data %>% dplyr::filter(substr(geoid,1,2) == "51")

write_csv(va_housing, xzfile("Cost/Rent/data/distribution/va_cttr_hud_2022_housing_cost.csv.xz", compression = 9))
