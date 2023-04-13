# Cost Burdened Households: Renters and mortgage-holders that pay more 
# than 30% of income for housing by race/ethnicity. 


# packages 
library(dplyr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(RPostgreSQL)
library(data.table)

# set working directory
setwd("~/git/sdc.housing")

#######################
# COUNTIES in NCR AREA
#######################

# county shapes and geoids
dmv_counties <- list(
  dc = "District of Columbia",
  md = c("Charles", "Frederick", "Montgomery", "Prince George's"),
  va = c(
    "Alexandria", "Arlington", "Fairfax", "Falls Church", "Loudoun", "Manassas",
    "Manassas Park", "Prince William"
  )
)
shapes <- list()
for(state in c("dc", "md", "va")){
  # shapes
  counties <- counties(state)
  ## store subsets to combine later
  counties <- counties[counties$NAME %in% dmv_counties[[state]],]
  shapes[[state]] <- list(
    counties = counties)
}
for(level in names(shapes$dc)){
  counties <- do.call(rbind, lapply(shapes, "[[", level))}

counties_GEOID <- counties$GEOID

###################################
# BLOCK GROUP
###################################

# Comment: all NAs for block groups for the selected years

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

years <- lst(2014,2015,2016,2017,2018, 2019) 

dmv.bg <- map(
  years,
  ~ get_acs(geography = "block group",
            year = .x,
            variables = c("B25074_006", "B25074_007", "B25074_008", "B25074_009",
                          "B25074_015", "B25074_016", "B25074_017", "B25074_018",
                          "B25074_024", "B25074_025", "B25074_026", "B25074_027", 
                          "B25074_033", "B25074_034", "B25074_035", "B25074_036",
                          "B25074_042", "B25074_043", "B25074_044", "B25074_045",
                          "B25074_051", "B25074_052", "B25074_053", "B25074_054",
                          "B25074_060", "B25074_061", "B25074_062", "B25074_063", # rent as above 30%
                          "B25101_001", # tot owners
                          "B25101_002", # owners with morgage
                          "B25101_006", "B25101_010", "B25101_014", "B25101_018",
                          "B25101_022", # morgage pay >30% of income
                          "B25003_002", # owners
                          "B25003_003" # renters
                          ),
            state = c("VA", "DC", "MD"),
            survey = "acs5",
            output = "wide",
            geometry = TRUE)
) %>% map2(years, ~ mutate(.x, year = .y))

# renters simplified
# "C25074_001", # total renters
# "C25074_004", "C25074_008", "C25074_012", "C25074_016", 
# "C25074_020", "C25074_024", # rent >30% of income


dmv.bg.red <- reduce(dmv.bg, rbind) %>% filter(substr(GEOID, 1, 5) %in% counties_GEOID) %>% 
  transmute(
    GEOID=GEOID,
    NAME = NAME,
    rent30 = B25074_006E + B25074_007E + B25074_008E + B25074_009E + B25074_015E +
            B25074_016E + B25074_017E + B25074_018E + B25074_024E + B25074_025E +
            B25074_026E + B25074_027E + B25074_033E + B25074_034E + B25074_035E + 
            B25074_036E + B25074_042E + B25074_043E + B25074_044E + B25074_045E +
            B25074_051E + B25074_052E + B25074_053E + B25074_054E + B25074_060E +
            B25074_061E + B25074_062E + B25074_063E, 
    morgage30 = B25101_006E + B25101_010E + B25101_014E + B25101_018E + B25101_022E,
    owners = B25101_001E,
    own_morgage = B25101_002E,
    renters = B25003_003E,
    owners2 = B25003_002E,
    tot_above30 = rent30 + morgage30,
    tot_units = own_morgage + renters,
    per_above30 = tot_above30/tot_units,
    year = year,
    geometry = geometry
  )

###########################
# CENSUS TRACT
###########################

dmv.tr <- map(
  years,
  ~ get_acs(geography = "tract",
            year = .x,
            variables = c("B25074_006", "B25074_007", "B25074_008", "B25074_009",
                          "B25074_015", "B25074_016", "B25074_017", "B25074_018",
                          "B25074_024", "B25074_025", "B25074_026", "B25074_027", 
                          "B25074_033", "B25074_034", "B25074_035", "B25074_036",
                          "B25074_042", "B25074_043", "B25074_044", "B25074_045",
                          "B25074_051", "B25074_052", "B25074_053", "B25074_054",
                          "B25074_060", "B25074_061", "B25074_062", "B25074_063", # rent as above 30%
                          "B25101_001", # tot owners
                          "B25101_002", # owners with morgage
                          "B25101_006", "B25101_010", "B25101_014", "B25101_018",
                          "B25101_022", # morgage pay >30% of income
                          "B25003_002", # owners
                          "B25003_003" # renters
            ),
            state = c("VA", "DC", "MD"),
            survey = "acs5",
            output = "wide",
            geometry = TRUE)
) %>% map2(years, ~ mutate(.x, year = .y))

# renters simplified
# "C25074_001", # total renters
# "C25074_004", "C25074_008", "C25074_012", "C25074_016", 
# "C25074_020", "C25074_024", # rent >30% of income


dmv.tr.red <- reduce(dmv.tr, rbind) %>% filter(substr(GEOID, 1, 5) %in% counties_GEOID) %>% 
  transmute(
    GEOID=GEOID,
    NAME = NAME,
    rent30 = B25074_006E + B25074_007E + B25074_008E + B25074_009E + B25074_015E +
      B25074_016E + B25074_017E + B25074_018E + B25074_024E + B25074_025E +
      B25074_026E + B25074_027E + B25074_033E + B25074_034E + B25074_035E + 
      B25074_036E + B25074_042E + B25074_043E + B25074_044E + B25074_045E +
      B25074_051E + B25074_052E + B25074_053E + B25074_054E + B25074_060E +
      B25074_061E + B25074_062E + B25074_063E, 
    morgage30 = B25101_006E + B25101_010E + B25101_014E + B25101_018E + B25101_022E,
    owners = B25101_001E,
    own_morgage = B25101_002E,
    renters = B25003_003E,
    #owners2 = B25003_002E,
    tot_above30 = rent30 + morgage30,
    tot_units = own_morgage + renters,
    per_above30 = tot_above30/tot_units,
    year = year,
    geometry = geometry
  )


###########################
# COUNTY
###########################

dmv.ct <- map(
  years,
  ~ get_acs(geography = "county",
            year = .x,
            variables = c("B25074_006", "B25074_007", "B25074_008", "B25074_009",
                          "B25074_015", "B25074_016", "B25074_017", "B25074_018",
                          "B25074_024", "B25074_025", "B25074_026", "B25074_027", 
                          "B25074_033", "B25074_034", "B25074_035", "B25074_036",
                          "B25074_042", "B25074_043", "B25074_044", "B25074_045",
                          "B25074_051", "B25074_052", "B25074_053", "B25074_054",
                          "B25074_060", "B25074_061", "B25074_062", "B25074_063", # rent as above 30%
                          "B25101_001", # tot owners
                          "B25101_002", # owners with morgage
                          "B25101_006", "B25101_010", "B25101_014", "B25101_018",
                          "B25101_022", # morgage pay >30% of income
                          "B25003_002", # owners
                          "B25003_003" # renters
            ),
            state = c("VA", "DC", "MD"),
            survey = "acs5",
            output = "wide",
            geometry = TRUE)
) %>% map2(years, ~ mutate(.x, year = .y))

# renters simplified
# "C25074_001", # total renters
# "C25074_004", "C25074_008", "C25074_012", "C25074_016", 
# "C25074_020", "C25074_024", # rent >30% of income


dmv.ct.red <- reduce(dmv.ct, rbind) %>% filter(substr(GEOID, 1, 5) %in% counties_GEOID) %>% 
  transmute(
    GEOID=GEOID,
    NAME = NAME,
    rent30 = B25074_006E + B25074_007E + B25074_008E + B25074_009E + B25074_015E +
      B25074_016E + B25074_017E + B25074_018E + B25074_024E + B25074_025E +
      B25074_026E + B25074_027E + B25074_033E + B25074_034E + B25074_035E + 
      B25074_036E + B25074_042E + B25074_043E + B25074_044E + B25074_045E +
      B25074_051E + B25074_052E + B25074_053E + B25074_054E + B25074_060E +
      B25074_061E + B25074_062E + B25074_063E, 
    morgage30 = B25101_006E + B25101_010E + B25101_014E + B25101_018E + B25101_022E,
    owners = B25101_001E,
    own_morgage = B25101_002E,
    renters = B25003_003E,
    #owners2 = B25003_002E,
    tot_above30 = rent30 + morgage30,
    tot_units = own_morgage + renters,
    per_above30 = tot_above30/tot_units *100,
    year = year
  )

########################
# GEOGRAPHY NAMES
########################

con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd"))

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")

dbDisconnect(con)

tr_names <- geo_names %>% filter(region_type == "tract")
ct_names <- geo_names %>% filter(region_type == "county")

#######################
# FORMAT
#######################

# add geographies names
out_df_tr <- left_join(dmv.tr.red, tr_names, by = c("GEOID"="geoid"))
out_df_ct <- left_join(dmv.ct.red, ct_names, by = c("GEOID"="geoid"))

# select vars
out_df_tr <- out_df_tr %>% select(-c(geometry, NAME))
out_df_ct <- out_df_ct %>% select(-c(geometry, NAME))


# long format
out_tr_long <- melt(out_df_tr,
                    id.vars=c("GEOID", "region_type", "region_name", "year", "geometry"),
                    variable.name="measure",
                    value.name="value"
)
out_ct_long <- melt(out_df_ct,
                    id.vars=c("GEOID", "region_type", "region_name", "year", "geometry"),
                    variable.name="measure",
                    value.name="value"
)

out_ct_long <- out_ct_long %>% select(-geometry)
out_tr_long <- out_tr_long %>% select(-geometry)

out_tr_long['measure_type'] = "count"
indx1 <- grepl('per', out_tr_long$measure) 
out_tr_long$measure_type[indx1] <- 'percent'

out_ct_long['measure_type'] = "count"
indx2 <- grepl('per', out_ct_long$measure) 
out_ct_long$measure_type[indx2] <- 'percent'

# save working files
# write_csv(out_ct_long, "data/cost_burden_hhs/working/ncr_ct_acs_2014_2019_cost_burdened_hhs.csv")
# write_csv(out_tr_long, "data/cost_burden_hhs/working/ncr_tr_acs_2014_2019_cost_burdened_hhs.csv")

#######################
# ADD to DB
#######################
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 password = Sys.getenv("db_pwd"))

dbWriteTable(con, c("dc_transportation_housing", "vadcmd_tr_acs_2014_2019_cost_burdened_hhs"), 
             out_tr_long,  row.names = F)
dbWriteTable(con, c("dc_transportation_housing", "vadcmd_ct_acs_2014_2019_cost_burdened_hhs"), 
             out_ct_long,  row.names = F)

#dbRemoveTable(con, c("dc_transportation_housing", "vadcmd_tr_acs_2014_2019_cost_burdened_hhs"))
#dbRemoveTable(con, c("dc_transportation_housing", "vadcmd_ct_acs_2014_2019_cost_burdened_hhs"))

dbSendStatement(con, "ALTER TABLE dc_transportation_housing.vadcmd_tr_acs_2014_2019_cost_burdened_hhs
                OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_transportation_housing.vadcmd_ct_acs_2014_2019_cost_burdened_hhs
                OWNER TO data_commons")

dbDisconnect(con)