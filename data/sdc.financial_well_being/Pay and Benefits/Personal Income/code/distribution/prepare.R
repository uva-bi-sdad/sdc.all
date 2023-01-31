# Prepare earnings

# load csv from BEA
# total employment selected
tot_emp <- read_csv("./data/earnings/original/tot_emp.csv.xz")
wage_sup <- read_csv("./data/earnings/original/wage_sup.csv.xz")
wage_sal <- read_csv("./data/earnings/original/wage_sal.csv.xz")
prop_inc <- read_csv("./data/earnings/original/prop_inc.csv.xz")

# init empty df
earn_job <- tot_emp %>% select(GeoFips, GeoName)

# compute emp rate
earn_job['tot_compensation_2015'] <- (wage_sup$`2015`+wage_sal$`2015`+prop_inc$`2015`) * 1000
earn_job['tot_compensation_2016'] <- (wage_sup$`2016`+wage_sal$`2016`+prop_inc$`2016`) * 1000
earn_job['tot_compensation_2017'] <- (wage_sup$`2017`+wage_sal$`2017`+prop_inc$`2017`) * 1000
earn_job['tot_compensation_2018'] <- (wage_sup$`2018`+wage_sal$`2018`+prop_inc$`2018`) * 1000
earn_job['tot_compensation_2019'] <- (wage_sup$`2019`+wage_sal$`2019`+prop_inc$`2019`) * 1000
earn_job['tot_compensation_2020'] <- (wage_sup$`2020`+wage_sal$`2020`+prop_inc$`2020`) * 1000

earn_job['tot_employment_2015'] <- tot_emp$`2015`
earn_job['tot_employment_2016'] <- tot_emp$`2016`
earn_job['tot_employment_2017'] <- tot_emp$`2017`
earn_job['tot_employment_2018'] <- tot_emp$`2018`
earn_job['tot_employment_2019'] <- tot_emp$`2019`
earn_job['tot_employment_2020'] <- tot_emp$`2020`

earn_job['earnings_per_job_2015'] <- (wage_sup$`2015`+wage_sal$`2015`+prop_inc$`2015`)/tot_emp$`2015` * 1000
earn_job['earnings_per_job_2016'] <- (wage_sup$`2016`+wage_sal$`2016`+prop_inc$`2016`)/tot_emp$`2016` * 1000
earn_job['earnings_per_job_2017'] <- (wage_sup$`2017`+wage_sal$`2017`+prop_inc$`2017`)/tot_emp$`2017` * 1000
earn_job['earnings_per_job_2018'] <- (wage_sup$`2018`+wage_sal$`2018`+prop_inc$`2018`)/tot_emp$`2018` * 1000
earn_job['earnings_per_job_2019'] <- (wage_sup$`2019`+wage_sal$`2019`+prop_inc$`2019`)/tot_emp$`2019` * 1000
earn_job['earnings_per_job_2020'] <- (wage_sup$`2020`+wage_sal$`2020`+prop_inc$`2020`)/tot_emp$`2020` * 1000

# format
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

counties <- geo_names %>% filter(region_type=="county")
earn_job$GeoFips <- as.character(earn_job$GeoFips)
earn_job_ct <- left_join(earn_job, counties, by=c("GeoFips" = "geoid"))
earn_job_ct <- earn_job_ct %>% select(-GeoName)

earn_job_ct_long <- reshape2::melt(earn_job_ct,
                                   id.vars=c("GeoFips", "region_type", "region_name"),
                                   variable.name="measure",
                                   value.name="value"
)

earn_job_ct_long['year'] =  str_sub(earn_job_ct_long$measure,-4,-1)
earn_job_ct_long$measure = str_sub(earn_job_ct_long$measure,1,-6)

earn_job_ct_long['measure_type'] = 'count'
earn_job_ct_long['measure_type'][earn_job_ct_long["measure"] == "earnings_per_job"] <- 'dollars'
earn_job_ct_long['measure_type'][earn_job_ct_long["measure"] == "tot_compensation"] <- 'dollars'

names(earn_job_ct_long)[1] <- 'geoid'

# re-arrange columns
earn_job_ct_long <- earn_job_ct_long[, c(1, 2, 3, 6, 4, 5, 7)]
earnings_per_job <- earn_job_ct_long

# upload to database
#con <- dbConnect(PostgreSQL(),
#                 dbname = "sdad",
#                 host = "postgis1",
#                 port = 5432,
#                 user = "YOUR_USERNAME",
#                 password = "YOUR_PASSWORD")

#dbWriteTable(con, c("dc_education_training", "va_ct_bea_2015_2020_earnings_per_job"),
#             earn_job_ct_long,  row.names = F)
#dbWriteTable(con, c("dc_education_training", "va_tr_acs_2015_2019_emp_rate"),
#             emp_rate,  row.names = F)

#dbRemoveTable(con, c("dc_education_training", "va_ct_bea_2015_2020_earnings_per_job"))
#dbRemoveTable(con, c("dc_education_training", "va_tr_acs_2015_2019_emp_rate"))

#dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_bea_2015_2020_earnings_per_job
#                    OWNER TO data_commons")
#dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_acs_2015_2019_emp_rate
#                    OWNER TO data_commons")

#dbDisconnect(con)


#source("./code/earnings/ingest_earnings.R")
readr::write_csv(earnings_per_job,
                 xzfile("./data/earnings/original/va_ct_bea_2015_2020_earnings_per_job.csv.xz", compression = 9))
