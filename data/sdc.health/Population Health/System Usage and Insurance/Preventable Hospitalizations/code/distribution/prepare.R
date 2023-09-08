library(readr)
library(data.table)
library(xlsx)
library(dplyr)
library(naniar)




county_hlth_rnks_2021<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2021.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2021_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2021.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

county_hlth_rnks_2020<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2020.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2020_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2020.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

county_hlth_rnks_2019<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2019.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2019_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2019.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

county_hlth_rnks_2018<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2018.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2018_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2018.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

county_hlth_rnks_2017<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2017.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2017_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2017.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

county_hlth_rnks_2016<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2016.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2016_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2016.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

county_hlth_rnks_2015<- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2015.xlsx", sheetName = "Ranked Measure Data", header = TRUE, startRow = 2))
county_hlth_rnks_2015_additional <- setDT(xlsx::read.xlsx("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2015.xlsx", sheetName = "Additional Measure Data", header = TRUE, startRow = 2))

dt2021 <- county_hlth_rnks_2021[!is.na(county_hlth_rnks_2021$County), .(geoid = FIPS, region_type = "county", year = "2021", measure = "prevent_hosp_rate", value = Preventable.Hospitalization.Rate, measure_type = "rate per 100k")]
dt2020 <- county_hlth_rnks_2020[!is.na(county_hlth_rnks_2020$County), .(geoid = FIPS, region_type = "county", year = "2020", measure = "prevent_hosp_rate", value = Preventable.Hospitalization.Rate, measure_type = "rate per 100k")]
dt2019 <- county_hlth_rnks_2019[!is.na(county_hlth_rnks_2019$County), .(geoid = FIPS, region_type = "county", year = "2019", measure = "prevent_hosp_rate", value = Preventable.Hosp..Rate, measure_type = "rate per 100k")]
dt2018 <- county_hlth_rnks_2018[!is.na(county_hlth_rnks_2018$County), .(geoid = FIPS, region_type = "county", year = "2018", measure = "prevent_hosp_rate", value = Preventable.Hosp..Rate, measure_type = "rate per 100k")]
dt2017 <- county_hlth_rnks_2017[!is.na(county_hlth_rnks_2017$County), .(geoid = FIPS, region_type = "county", year = "2017", measure = "prevent_hosp_rate", value = Preventable.Hosp..Rate, measure_type = "rate per 100k")]
dt2016 <- county_hlth_rnks_2016[!is.na(county_hlth_rnks_2016$County), .(geoid = FIPS, region_type = "county", year = "2016", measure = "prevent_hosp_rate", value = Preventable.Hosp..Rate, measure_type = "rate per 100k")]
dt2015 <- county_hlth_rnks_2015[!is.na(county_hlth_rnks_2015$County), .(geoid = FIPS, region_type = "county", year = "2015", measure = "prevent_hosp_rate", value = Preventable.Hosp..Rate, measure_type = "rate per 100k")]

dt <- rbindlist(list(dt2021, dt2020, dt2019, dt2018, dt2017, dt2016, dt2015))

# 2019-2021: measure_type = rate per 100k
# 2015-2018: measure_type = rate per 1k

# Transform rate per 1k to rate per 100k

# Define vector of years to update
years_to_update <- c(2015, 2016, 2017, 2018)

# Update observations for the specified years
dt <- dt %>%
  mutate(value = ifelse(year %in% years_to_update, 100*value, value))



# Aggregate VA county data to VA Health District data

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get VA county data

va_ct_data <- dt %>%
  filter(substr(geoid,1,2) == "51" & region_type == "county")


# merge with health district info

va_ct_data <- merge(va_ct_data, ct_hd_crosswalk[ , c(1,3,4)], by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

miss_var_summary(va_ct_data) # there is 30 missing observations for "value" variable
# Missing values in the downloaded data itself 
# since 30 missing date observations for "value" variable in dataset "dt" 
# checked using: miss_var_summary(dt)

# aggregate county data to health district data

va_hd_data <- va_ct_data %>%
  group_by(hd_geoid, hd_name, year) %>%
  #summarise_at(vars(value), list(value = mean))
  summarise(Mean = mean(value, na.rm = TRUE))

colnames(va_hd_data) <- c("geoid", "hd_name", "year", "value")
va_hd_data$measure <- "prevent_hosp_rate"
va_hd_data$measure_type <- "rate per 100k"
va_hd_data$region_type <- "health district"
va_hd_data$hd_geoid <- va_hd_data$geoid

va_hd_data <- va_hd_data[ , c(1,7,3,5,4,6,8,2)]

# add health district data to va_ct_data

va_hd_ct_2015_2021 <- rbind(va_ct_data, va_hd_data)


# arrange by group and year (ascending)
va_hd_ct_2015_2021 <- va_hd_ct_2015_2021 %>%
  arrange(geoid, year)

# Final dataset
va_hd_ct_2015_2021_preventable_hospitalizations <-  va_hd_ct_2015_2021 %>%
  select(geoid, year, measure, value, measure_type)

write.csv(data, file=xzfile("/sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/distribution/va_hdct_2015_2021_preventable_hospitalizations.csv.xz"), row.names = FALSE)


