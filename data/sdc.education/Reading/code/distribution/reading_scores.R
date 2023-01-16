# packages
library(readxl)
library(tidyverse)
library(dplyr)
library(RPostgreSQL)
library(fuzzyjoin)
library(reshape2)
library(sf)


# school_test <- read_excel("2020-2021-school-test-by-test.xlsx")
# # keep Subject = English: Reading and Grade = Gr 3
# reading_score <- subset(school_test, Subject == "English: Reading" & Grade == "Gr 3")
#
# # add aka address
# reading_score$address <- paste(reading_score$`Sch Name`, reading_score$`Div Name`)
#
# # geocode addresses
# # installed google api key
# readRenviron("~/.Renviron")
# Sys.getenv("GOOGLEGEOCODE_API_KEY")
#
# reading_lonlat <- reading_score %>%
#   geocode(address,
#           method = 'google',
#           lat = latitude ,
#           long = longitude,
#           full_results = FALSE)
#
# # save
# # write_csv(reading_lonlat, "reading_score_lonlat.csv")

# # load in data
# read_score <- read_csv("reading_score_lonlat.csv")
# read_score <- read_score[!is.na(read_score$latitude),]
#
# # school district shapes
# sd_shapes <- st_as_sf(st_read("~/../../../../project/biocomplexity/sdad/projects_data/mc/data_commons/dc_education_training/elementary_school_shapes/","SABS_1516_Primary"), crs = 4269)
#
# # lon and lat to geo-points
# geopts <- read_score %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4269)
# sd_shapes <- st_transform(sd_shapes, st_crs(geopts))
# sd_shapes <- sd_shapes %>% filter(stAbbrev == "VA")
# sd_shapes$leaid <- as.character(sd_shapes$leaid)
# sf_use_s2(FALSE)
# # indeces of counties which contain a geopoint
# inds <- st_within(geopts$geometry, sd_shapes$geometry, sparse=T)
#
# # init list to store tract ids
# sd_list <- c()
#
# for (i in inds){
#   print(i[1])
#
#   if (identical(sd_shapes$schnam[i[1]],character(0))){
#     sd_list <- append(sd_list, NA)}
#   else{
#     sd_list <- append(sd_list, sd_shapes$leaid[i[1]])}
# }
#
# read_score['geoid_sd'] <- sd_list
#
# write_csv(read_score, "reading_score_sd.csv")

#############################################

# load in data
read_score <- read_csv("reading_score_lonlat.csv")
read_score$`2018-2019 Pass Rate` <- as.numeric(read_score$`2018-2019 Pass Rate`)
read_score$`2019-2020 Pass Rate` <- as.numeric(read_score$`2019-2020 Pass Rate`)
read_score$`2020-2021 Pass Rate` <- as.numeric(read_score$`2020-2021 Pass Rate`)

# load county and health distrct files
# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

counties <- dbGetQuery(con, "SELECT * FROM dc_common.va_ct_sdad_2021_virginia_county_geoids")
health_dist <- dbGetQuery(con, "SELECT * FROM dc_common.va_hdct_sdad_2021_health_district_counties")
school_dist <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_sd_nces_2021_school_district_names")
dbDisconnect(con)


# match counties to reading score
read_score_ct <- counties %>%
  stringdist_inner_join(read_score, by = c('region_name' = 'Div Name'), max_dist = 10)

df1 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(mean_read_pass_rate_2019 =
            mean(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df2 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(mean_read_pass_rate_2021 =
              mean(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df12 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(median_read_pass_rate_2019 =
              median(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df22 <- read_score_ct %>%  group_by(geoid) %>%
  summarize(median_read_pass_rate_2021 =
              median(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

read_score_ct <- read_score_ct %>%
  select(geoid, region_name)

read_score_ct <- left_join(read_score_ct, df1, by=("geoid"))
read_score_ct <- left_join(read_score_ct, df2, by=("geoid"))
read_score_ct <- left_join(read_score_ct, df12, by=("geoid"))
read_score_ct <- left_join(read_score_ct, df22, by=("geoid"))

read_score_ct <- read_score_ct %>% distinct()
read_score_ct["region_type"] <- "county"


read_score_ct_long <- melt(read_score_ct,
                  id.vars=c("geoid", "region_type", 'region_name'),
                  variable.name="measure",
                  value.name="value"
)
read_score_ct_long['year'] =  str_sub(read_score_ct_long$measure,-4,-1)
read_score_ct_long$measure = str_sub(read_score_ct_long$measure,1,-6)
read_score_ct_long['measure_type'] = "percent"
# indx1 <- grepl('mean', read_score_ct_long$measure)
# indx2 <- grepl('median', read_score_ct_long$measure)
# read_score_ct_long$measure_type[indx1] <- 'mean'
# read_score_ct_long$measure_type[indx2] <- 'median'

#re-oder columns
read_score_ct_long <- read_score_ct_long[, c(1, 2, 3, 6, 4, 5, 7)]

#write_csv(read_score_ct, "va_ct_vdoe_2019_21_3rdGrade_MeanMedReadingScore.csv")

##################### HEALTH DISCTRICT ##############
read_score_hd <- left_join(read_score_ct, health_dist, by= c("geoid" = "geoid_county"))

df3 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(mean_read_pass_rate_2019 =
              mean(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df4 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(mean_read_pass_rate_2021 =
              mean(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df32 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(median_read_pass_rate_2019 =
              median(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df42 <- read_score_hd %>%  group_by(geoid.y) %>%
  summarize(median_read_pass_rate_2021 =
              median(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

read_score_hd <- read_score_hd %>% select(geoid.y, region_name.y)
names(read_score_hd)[1] <- 'geoid'
names(read_score_hd)[2] <- 'region_name'

read_score_hd <- left_join(read_score_hd, df3, by=c("geoid"="geoid.y"))
read_score_hd <- left_join(read_score_hd, df4, by=c("geoid"="geoid.y"))
read_score_hd <- left_join(read_score_hd, df32, by=c("geoid"="geoid.y"))
read_score_hd <- left_join(read_score_hd, df42, by=c("geoid"="geoid.y"))

read_score_hd <- read_score_hd %>% distinct(geoid, .keep_all=TRUE)

read_score_hd["region_type"] <- "health district"

read_score_hd_long <- melt(read_score_hd,
                           id.vars=c("geoid", "region_type", 'region_name'),
                           variable.name="measure",
                           value.name="value"
)

read_score_hd_long['year'] =  str_sub(read_score_hd_long$measure,-4,-1)

read_score_hd_long$measure = str_sub(read_score_hd_long$measure,1,-6)

read_score_hd_long['measure_type'] = "percent"
# indx1 <- grepl('mean', read_score_hd_long$measure)
# indx2 <- grepl('median', read_score_hd_long$measure)
# read_score_hd_long$measure_type[indx1] <- 'mean'
# read_score_hd_long$measure_type[indx2] <- 'median'
# read_score_hd_long['measure_units'] <- 'percent'

#re-oder columns
read_score_hd_long <- read_score_hd_long[, c(1, 2, 3, 6, 4, 5, 7)]

#write_csv(read_score_hd, "va_hd_vdoe_2019_21_3rdGrade_MeanMedReadingScore.csv")

################################ SCHOOL DISTRICT
# upload school geoids
read_score_sd <- read_csv("reading_score_sd.csv")
read_score_sd$`2018-2019 Pass Rate` <- as.numeric(read_score_sd$`2018-2019 Pass Rate`)
read_score_sd$`2019-2020 Pass Rate` <- as.numeric(read_score_sd$`2019-2020 Pass Rate`)
read_score_sd$`2020-2021 Pass Rate` <- as.numeric(read_score_sd$`2020-2021 Pass Rate`)


df5 <- read_score_sd %>%  group_by(geoid_sd) %>%
  summarize(mean_read_pass_rate_2019 =
              mean(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df6 <- read_score_sd %>%  group_by(geoid_sd) %>%
  summarize(mean_read_pass_rate_2021 =
              mean(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df52 <- read_score_sd %>%  group_by(geoid_sd) %>%
  summarize(median_read_pass_rate_2019 =
              median(`2018-2019 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

df62 <- read_score_sd %>%  group_by(geoid_sd) %>%
  summarize(median_read_pass_rate_2021 =
              median(`2020-2021 Pass Rate`, na.rm = TRUE)) %>%
  as.data.frame()

read_score_sd <- read_score_sd %>% select(geoid_sd)
read_score_sd <- left_join(read_score_sd, df5, by=c("geoid_sd"))
read_score_sd <- left_join(read_score_sd, df6, by=c("geoid_sd"))
read_score_sd <- left_join(read_score_sd, df52, by=c("geoid_sd"))
read_score_sd <- left_join(read_score_sd, df62, by=c("geoid_sd"))

read_score_sd <- read_score_sd %>% distinct(geoid_sd, .keep_all=TRUE)
read_score_sd$geoid_sd <- as.character(read_score_sd$geoid_sd)
read_score_sd <- left_join(read_score_sd, school_dist[substr(school_dist$geoid, 1,2)=="51",], by=c("geoid_sd"="geoid"))
read_score_sd <- read_score_sd[!is.na(read_score_sd$geoid_sd),]

read_score_sd_long <- melt(read_score_sd,
                           id.vars=c("geoid_sd", "region_type", "region_name"),
                           variable.name="measure",
                           value.name="value"
)

read_score_sd_long['year'] =  str_sub(read_score_sd_long$measure,-4,-1)

read_score_sd_long$measure = str_sub(read_score_sd_long$measure,1,-6)

read_score_sd_long['measure_type'] = "percent"
# indx1 <- grepl('mean', read_score_sd_long$measure)
# indx2 <- grepl('median', read_score_sd_long$measure)
# read_score_sd_long$measure_type[indx1] <- 'mean'
# read_score_sd_long$measure_type[indx2] <- 'median'
# read_score_sd_long['measure_units'] <- 'percent'

#re-oder columns
read_score_sd_long <- read_score_sd_long[, c(1, 2, 3, 6, 4, 5, 7)]

#write_csv(read_score_hd, "va_sd_vdoe_2019_21_3rdGrade_MeanMedReadingScore.csv")

##################### ADD TO DB
# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USER_NAME",
                 password = "YOUR_PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_ct_long,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_hd_long, row.names = F)
dbWriteTable(con, c("dc_education_training", "va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_sd_long, row.names = F)

# get tables
read_score_ct_long <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score")
read_score_hd_long <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score")
read_score_sd_long <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score")
# change ownership to SDAD
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score
                OWNER TO data_commons")

dbRemoveTable(con, c("dc_education_training", "va_ct_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_ct_long)
dbRemoveTable(con, c("dc_education_training", "va_hd_vdoe_2019_2021_3rd_grade_mean_median_read_score"),
             read_score_hd_long)
dbRemoveTable(con, c("dc_education_training", "va_sd_vdoe_2019_2021_3rd_grade_mean_median_read_score"))

#dbRemoveTable(con, c("dc_education_training", "va_ct_vdoe_2019_2021_3rdGrade_AvReadingScore"))
#dbRemoveTable(con, c("dc_education_training","va_hd_vdoe_2019_2021_3rdGrade_AvReadingScore"))

# geographies names
geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")

# health districts names
health_dist <- dbGetQuery(con, "SELECT * FROM dc_geographies.va_hd_vdh_2021_health_district_geo_names")


dbDisconnect(con)

counties <- geo_names %>% filter(region_type=="county")

# remove old names and geoids
read_score_ct_long <- read_score_ct_long %>% select(-region_name, -region_type)
read_score_hd_long <- read_score_hd_long %>% select(-geoid, -region_type)

# add new names and geoids
read_score_ct_long <- left_join(read_score_ct_long, counties, by="geoid")
read_score_ct_long <- left_join(read_score_hd_long, health_dist, by="region_name")

# re-arrange columns
read_score_ct_long <- read_score_ct_long[, c(1, 7,6, 2, 3, 4, 5)]
read_score_hd_long <- read_score_hd_long[, c(6, 7, 1, 2, 3, 4, 5)]

# re-name geoid col in sd dataset
names(read_score_sd_long)[1] <- "geoid"

