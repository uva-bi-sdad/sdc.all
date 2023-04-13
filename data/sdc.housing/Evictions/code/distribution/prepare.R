# Evictions filings
# The data is collected from Evictions Lab at Princeton

# packages
library(readr)
library(tigris)
library(dplyr)
library(RPostgreSQL)
library(reshape2)

# working directory
setwd("~/git/sdc.housing")

# load in data
# too large files
# all_evictions_bg <- read_csv("data/evictions/original/block-groups.csv")

# append all the chunks together
setwd("~/git/sdc.housing/data/evictions/original/evictions_bg")
# concatenate csv into one file
for (data in list.files("~/git/sdc.housing/data/evictions/original/evictions_bg/")){
  
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read_csv(data)
  }
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read_csv(data)
    dataset <-unique(rbind(dataset, tempory))
    rm(tempory)
  }
}

# counties in DMV area
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

# filter by the county ID
evictions_dmv_bg <- dataset %>% filter(substr(GEOID, 1, 5) %in% counties_GEOID)

# select variables
evictions_dmv_bg <-  evictions_dmv_bg %>% select(c("GEOID", "year", "eviction-filings", "evictions"))

# aggregate to tracts

evictions_dmv_tr <- evictions_dmv_bg
evictions_dmv_tr$GEOID <- substr(evictions_dmv_tr$GEOID, 1, 11)
evictions_dmv_tr <- evictions_dmv_tr %>%
  group_by(GEOID,year) %>%
  summarise_all(sum, na.rm=T) %>%
  as.data.frame()

# county aggregate
evictions_dmv_ct <- evictions_dmv_bg
evictions_dmv_ct$GEOID <- substr(evictions_dmv_ct$GEOID, 1, 5)
evictions_dmv_ct <- evictions_dmv_ct %>%
  group_by(GEOID, year) %>%
  summarise_all(sum, na.rm=T) %>%
  as.data.frame()

# geography names
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 password = Sys.getenv("db_pwd"))

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")

dbDisconnect(con)

bg_names <- geo_names %>% filter(region_type == "block group")
tr_names <- geo_names %>% filter(region_type == "tract")
ct_names <- geo_names %>% filter(region_type == "county")

# add geo names
evictions_dmv_bg <- left_join(evictions_dmv_bg, bg_names, by=c("GEOID"="geoid"))
evictions_dmv_tr <- left_join(evictions_dmv_tr, tr_names, by=c("GEOID"="geoid"))
evictions_dmv_ct <- left_join(evictions_dmv_ct, ct_names, by=c("GEOID"="geoid"))

# rename geoid col
names(evictions_dmv_bg)[1] <- "geoid"
names(evictions_dmv_tr)[1] <- "geoid"
names(evictions_dmv_ct)[1] <- "geoid"

# long format
evictions_dmv_bg <- melt(evictions_dmv_bg,
                       id.vars=c("geoid", "region_type", "region_name", "year"),
                       variable.name="measure",
                       value.name="value"
)

evictions_dmv_tr <- melt(evictions_dmv_tr,
                         id.vars=c("geoid", "region_type", "region_name", "year"),
                         variable.name="measure",
                         value.name="value"
)
evictions_dmv_ct <- melt(evictions_dmv_ct,
                         id.vars=c("geoid", "region_type", "region_name", "year"),
                         variable.name="measure",
                         value.name="value"
)

# add measure name
evictions_dmv_bg['measure_type'] = "count"
evictions_dmv_tr['measure_type'] = "count"
evictions_dmv_ct['measure_type'] = "count"

#######################
# SAVE TO WORKING 
#######################
write_csv(evictions_dmv_bg, "data/evictions/working/ncr_bg_evictionlab_2000_2016_evictions.csv")
write_csv(evictions_dmv_tr, "data/evictions/working/ncr_tr_evictionlab_2000_2016_evictions.csv")
write_csv(evictions_dmv_ct, "data/evictions/working/ncr_ct_evictionlab_2000_2016_evictions.csv")

#######################
# ADD to DB
#######################
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 password = Sys.getenv("db_pwd"))

dbWriteTable(con, c("dc_transportation_housing", "vadcmd_bg_evictionlab_2000_2016_evictions"),
             evictions_dmv_bg,  row.names = F)
dbWriteTable(con, c("dc_transportation_housing", "vadcmd_tr_evictionlab_2000_2016_evictions"),
             evictions_dmv_tr,  row.names = F)
dbWriteTable(con, c("dc_transportation_housing", "vadcmd_ct_evictionlab_2000_2016_evictions"),
             evictions_dmv_ct,  row.names = F)

#dbRemoveTable(con, c("dc_transportation_housing", "vadcmd_bg_evictionlab_2000_2016_evictions"))
#dbRemoveTable(con, c("dc_transportation_housing", "vadcmd_tr_evictionlab_2000_2016_evictions"))
#dbRemoveTable(con, c("dc_transportation_housing", "vadcmd_ct_evictionlab_2000_2016_evictions"))

dbSendStatement(con, "ALTER TABLE dc_transportation_housing.vadcmd_bg_evictionlab_2000_2016_evictions
                OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_transportation_housing.vadcmd_tr_evictionlab_2000_2016_evictions
                OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_transportation_housing.vadcmd_ct_evictionlab_2000_2016_evictions
                OWNER TO data_commons")

dbDisconnect(con)
