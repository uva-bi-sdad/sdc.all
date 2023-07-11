for (f in list.files("R/", full.names = T)) source(f)

# Load the County Health Rankings Column Definitions
load("data/county.hlth.rnks.columns.RData")

# List the Variable Categories
county_hlth_rnks_columns[, .(Category), c("DataType")]

# List all Variables in a Variable Category
county_hlth_rnks_columns[Category=="Preventable hospital stays",]

# Download Data By Year, Data Type, and Column Numbers
my_data_2015 <- get_data(year = "2015", data_type = "Ranked Measure Data", columns = c(88, 91))
my_data_2016 <- get_data(year = "2016", data_type = "Ranked Measure Data", columns = c(84, 87))
my_data_2017 <- get_data(year = "2017", data_type = "Ranked Measure Data", columns = c(81, 84))
my_data_2018 <- get_data(year = "2018", data_type = "Ranked Measure Data", columns = c(85, 88))
my_data_2019 <- get_data(year = "2019", data_type = "Ranked Measure Data", columns = c(84, 85))
my_data_2020 <- get_data(year = "2020", data_type = "Ranked Measure Data", columns = c(121, 122))
my_data_2021 <- get_data(year = "2021", data_type = "Ranked Measure Data", columns = c(127, 128))


my_data <- data.table::rbindlist(list(
  my_data_2015[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hosp. Rate` * 100,
    measure_type = 'rate per 100K'
  )],

  my_data_2016[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hosp. Rate` * 100,
    measure_type = 'rate per 100K'
  )],

  my_data_2017[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hosp. Rate` * 100,
    measure_type = 'rate per 100K'
  )],

  my_data_2018[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hosp. Rate` * 100,
    measure_type = 'rate per 100K'
  )],

  my_data_2019[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hosp. Rate`,
    measure_type = 'rate per 100K'
  )],

  my_data_2020[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hospitalization Rate`,
    measure_type = 'rate per 100K'
  )],

  my_data_2021[, .(
    geoid = `Geographic identifiers - FIPS`,
    region_type = 'county',
    year,
    measure = "prevent_hosp_rate",
    value = `Preventable hospital stays - Preventable Hospitalization Rate`,
    measure_type = 'rate per 100K'
  )]

))

my_data <- my_data[!is.na(geoid) & geoid != "51000",]

con <- get_db_conn()
counties <- data.table::setDT(DBI::dbGetQuery(con, "select * from dc_common.va_ct_sdad_2021_virginia_county_geoids"))
DBI::dbDisconnect(con)


mrg <- merge(my_data, counties, by = "geoid", all.x = T)[, .(geoid, region_type, region_name, year, measure, value, measure_type)]

con <- get_db_conn()
DBI::dbSendQuery(con, "drop table if exists dc_health_behavior_diet.va_ct_chr_2015_2021_preventable_hospitalizations")
dc_dbWriteTable(con, schema_name = "dc_health_behavior_diet", table_name = "va_ct_chr_2015_2021_preventable_hospitalizations", mrg)
DBI::dbDisconnect(con)

write.csv(mrg, "data/va_ct_chr_2015_2021_preventable_hospitalizations.csv", row.names = FALSE)
