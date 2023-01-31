# Ingest employment rates using ACS
# packages
library(tidycensus)
library(tigris)
library(RPostgreSQL)
library(plyr)
library(tidyverse)

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

acs_vars <- c(# tot civilian labour force
                "B23025_003",
              # employed
                "B23025_004")

acs_2019 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2019,
                    cache_table=TRUE,
                    output="wide")

acs_2018 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2018,
                    cache_table=TRUE,
                    output="wide")

acs_2017 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2017,
                    cache_table=TRUE,
                    output="wide")

acs_2016 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2016,
                    cache_table=TRUE,
                    output="wide")

acs_2015 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2015,
                    cache_table=TRUE,
                    output="wide")

acs_est19 <- acs_2019 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2019 = B23025_004E,
  #tot labor force
  denominator_2019 = B23025_003E,
  # employment rate
  emp_rate_2019 = B23025_004E/B23025_003E * 100)

acs_est18 <- acs_2018 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2018 = B23025_004E,
  #tot labor force
  denominator_2018 = B23025_003E,
  # employment rate
  emp_rate_2018 = B23025_004E/B23025_003E * 100)

acs_est17 <- acs_2017 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2017 = B23025_004E,
  #tot labor force
  denominator_2017 = B23025_003E,
  # employment rate
  emp_rate_2017 = B23025_004E/B23025_003E * 100)

acs_est16 <- acs_2016 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2016 = B23025_004E,
  #tot labor force
  denominator_2016 = B23025_003E,
  # employment rate
  emp_rate_2016 = B23025_004E/B23025_003E * 100)

acs_est15 <- acs_2015 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2015 = B23025_004E,
  #tot labor force
  denominator_2015 = B23025_003E,
  # employment rate
  emp_rate_2015 = B23025_004E/B23025_003E *100)

# merge together

acs_est_all <-
  join_all(list(acs_est15, acs_est16, acs_est17, acs_est18, acs_est19),
           by='GEOID', type='left')
acs_est_all <- acs_est_all %>% select(-NAME)

# connect to database
get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }
con <- get_db_conn()

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
dbDisconnect(con)

tracts <- geo_names %>% filter(region_type == "tract")
acs_est_all <- left_join(acs_est_all, tracts, by=c("GEOID" = "geoid"))

emp_rate <- reshape2::melt(acs_est_all,
                         id.vars=c("GEOID", "region_name", "region_type"),
                         variable.name="measure",
                         value.name="value"
)

emp_rate['year'] =  str_sub(emp_rate$measure,-4,-1)
emp_rate$measure = str_sub(emp_rate$measure,1,-6)
indx1 <- grepl('numerator', emp_rate$measure)
indx2 <- grepl('denominator', emp_rate$measure)
indx3 <- grepl('emp_rate', emp_rate$measure)
emp_rate$measure_type[indx1] <- 'count'
emp_rate$measure_type[indx2] <- 'count'
emp_rate$measure_type[indx3] <- 'percent'

names(emp_rate)[1] <- 'geoid'

# re-arrange columns
emp_rate <- emp_rate[, c(1, 3, 2, 6, 4, 5, 7)]

readr::write_csv(emp_rate,
                 xzfile("./data/employment/original/va_tr_acs_2015_2019_emp_rate.csv.xz", compression = 9))
