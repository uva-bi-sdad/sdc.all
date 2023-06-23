library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(naniar)
library(sf)


# Prepare earnings

# load csv from BEA
# total employment selected
tot_emp <- read_csv("./data/earnings/original/tot_emp.csv.xz")
wage_sup <- read_csv("./data/earnings/original/wage_sup.csv.xz")
wage_sal <- read_csv("./data/earnings/original/wage_sal.csv.xz")
prop_inc <- read_csv("./data/earnings/original/prop_inc.csv.xz")

#load data
tot_emp <- read_csv("~/git/sdc.financial_well_being_dev/Pay and Benefits/Personal Income/data/original/tot_emp.csv.xz")
wage_sup <- read_csv("~/git/sdc.financial_well_being_dev/Pay and Benefits/Personal Income/data/original/wage_sup.csv.xz")
wage_sal <- read_csv("~/git/sdc.financial_well_being_dev/Pay and Benefits/Personal Income/data/original/wage_sal.csv.xz")
prop_inc <- read_csv("~/git/sdc.financial_well_being_dev/Pay and Benefits/Personal Income/data/original/prop_inc.csv.xz")

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


# splitting combined rows to individual rows
#First removing the VA in the rows since few rows have and few rows doesnt (just for our flexibility in spliting)
earn_job$GeoName <- gsub("\\*|VA", "", earn_job$GeoName)

#only from row 83, there are multiple entries so we are splitting after 83 and also few rows have
#(, and +)  so we are first splitting for + and then splitting for , 
#few rows have + and few have , so focusing on + first and , next for splitting

earn_job <- earn_job %>%
  mutate(GeoName = ifelse(row_number() >= 83, strsplit(GeoName, "\\+"), GeoName)) %>%
  unnest(GeoName) %>%
  mutate(GeoName = ifelse(row_number() %in% c(87,93, 95, 111, 115), strsplit(GeoName, ","), GeoName)) %>%
  unnest(GeoName)

#Assigning the remaining GeoFips values for the remaining areas
GeoFips_values <- c(51003, 51540, 51005, 51580, 51015, 51790, 51820, 51031, 51680, 51035,
                    51640, 51053, 51570, 51730, 51059, 51600, 51610, 51069, 51840, 51081,
                    51595, 51089, 51690, 51095, 51830, 51121, 51750, 51143, 51590, 51149,
                    51670, 51153, 51683, 51685, 51161, 51775, 51163, 51530, 51678, 51165,
                    51660, 51175, 51620, 51177, 51630, 51191, 51520, 51195, 51720, 51199,
                    51735)

#assigning values from row 83
earn_job$GeoFips[83:nrow(earn_job)] <- GeoFips_values

#pasting ,VA at the end of each area
earn_job$GeoName <- paste0(gsub(",", "", earn_job$GeoName), ",VA")




# format
# connect to database
# get_db_conn <-
#   function(db_name = "sdad",
#            db_host = "postgis1",
#            db_port = "5432",
#            db_user = Sys.getenv("db_usr"),
#            db_pass = Sys.getenv("db_pwd")) {
#     RPostgreSQL::dbConnect(
#       drv = RPostgreSQL::PostgreSQL(),
#       dbname = db_name,
#       host = db_host,
#       port = db_port,
#       user = db_user,
#       password = db_pass
#     )
#   }
# con <- get_db_conn()
# 
# geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
# dbDisconnect(con)



#get geonames from sdc.geographies
geo_names <- st_read("~/git/sdc.geographies/VA/Census Geographies/County/2020/data/distribution/va_geo_census_cb_2020_counties.geojson")


counties <- geo_names %>% filter(region_type=="county")

earn_job$GeoFips <- as.character(earn_job$GeoFips)
earn_job_ct <- left_join(earn_job, counties, by=c("GeoFips" = "geoid"))
earn_job_ct <- earn_job_ct %>% select(-GeoName)



####

# Define the desired order of measure categories
measure_order <- c("tot_compensation", "tot_employment", "earnings_per_job")

# Extract year from column names
years <- str_extract(colnames(earn_job_ct)[2:19], "\\d{4}")

# Reshape the data frame to long format
earn_job_ct_long <- earn_job_ct %>%
  select(GeoFips, region_type, region_name, starts_with("tot_") | starts_with("earnings_per_")) %>%
  pivot_longer(cols = starts_with("tot_") | starts_with("earnings_per_"),
               names_to = "measure",
               values_to = "value") %>%
  mutate(year = str_extract(measure, "\\d{4}"),
         measure = str_remove(measure, "_\\d{4}"),
         value_type = ifelse(measure == "tot_employment", "count", "dollars")) %>%
  select(GeoFips, region_type, region_name, year, measure, value, value_type) %>%
  arrange(factor(measure, levels = measure_order), year)


#renaming the column names 
earn_job_ct_long <- earn_job_ct_long %>%
  rename(geoid = GeoFips,
         measure_type = value_type)

earnings_per_job <- earn_job_ct_long


#######
# earn_job_ct_long <- reshape2::melt(earn_job_ct,
#                                    id.vars=c("GeoFips", "region_type", "region_name"),
#                                    variable.name="measure",
#                                    value.name="value"
# )
# 
# earn_job_ct_long['year'] =  str_sub(earn_job_ct_long$measure,-4,-1)
# earn_job_ct_long$measure = str_sub(earn_job_ct_long$measure,1,-6)
# 
# earn_job_ct_long['measure_type'] = 'count'
# earn_job_ct_long['measure_type'][earn_job_ct_long["measure"] == "earnings_per_job"] <- 'dollars'
# earn_job_ct_long['measure_type'][earn_job_ct_long["measure"] == "tot_compensation"] <- 'dollars'
# 
# names(earn_job_ct_long)[1] <- 'geoid'
# 
# # re-arrange columns
# earn_job_ct_long <- earn_job_ct_long[, c(1, 2, 3, 6, 4, 5, 7)]
# earnings_per_job <- earn_job_ct_long

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

#write.csv(earnings_per_job, "~/git/sdc.financial_well_being_dev/Pay and Benefits/Personal Income/data/distribution/va_ct_bea_2015_2020_earnings_per_job.csv.xz")







#Aggregation with healthcare districts



va_ct_data <- earnings_per_job


va_ct_to_hd_crosswalk <- read_csv("~/git/sdc.geographies/VA/State Geographies/Health Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv")



#merging them but making sure the order is preserved while merging so adding index column and sorting based on it and removing the index column
va_ct_data$index <- seq_len(nrow(va_ct_data))
va_ct_data <- merge(va_ct_data, va_ct_to_hd_crosswalk[, c(1, 3, 4)], by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)
va_ct_data <- va_ct_data[order(va_ct_data$index), ]
va_ct_data$index <- NULL


#Performing aggregation
earnings_per_job_hd_year <- va_ct_data %>%
  group_by(year, hd_geoid, hd_name) %>%
  summarize(total_compensation = sum(value[measure == "tot_compensation"]),
            total_employment = sum(value[measure == "tot_employment"])) %>%
  mutate(earnings_per_job = total_compensation / total_employment)


#Changing column names of the df

current_names_year <- colnames(earnings_per_job_hd_year)

new_names_year <- current_names_year
new_names_year[new_names_year == "hd_geoid"] <- "geoid"
new_names_year[new_names_year == "hd_name"] <- "region_name"

colnames(earnings_per_job_hd_year) <- new_names_year



#Performing long format conversion
earnings_per_job_hd_year_long <- earnings_per_job_hd_year %>%
  gather(measure, value, total_compensation:earnings_per_job) %>%
  mutate(
    region_type = "health district",
    year = as.character(year),
    measure_type = case_when(
      measure %in% c("total_compensation", "earnings_per_job") ~ "dollars",
      measure == "total_employment" ~ "count"
    )
  ) %>%
  select(geoid, region_type, region_name, year, measure, value, measure_type)


#making sure they are of df class 
earnings_per_job_df <- as.data.frame(earnings_per_job)
earnings_per_job_hd_year_long_df <- as.data.frame(earnings_per_job_hd_year_long)

# Performing row bind
row_binded_data <- rbind(earnings_per_job_df, earnings_per_job_hd_year_long_df)

write.csv(row_binded_data, "~/git/sdc.financial_well_being_dev/Pay and Benefits/Personal Income/data/distribution/va_ct_bea_2015_2020_earnings_per_job.csv.xz")
