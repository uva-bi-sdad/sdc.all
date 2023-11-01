library(readxl)
library(dplyr)
library(readr)
library(tidycensus)

#reading the HOI raw scores exel file
df <- read_excel("~/segregation_raw/HOI V3_14 Variables_Raw Scores.xlsx")


#selecting the required columns, here focusing on census tract and segregation index
df <-  df %>% select(CT2, "**Spatial Segregation")

#changing the column names of the df according to SDAD format
colnames(df)[1] <- "geoid"
colnames(df)[2] <- "value"

#creating year column and assigning value
df$year <- '2020'

#creating measure column
df$measure <- 'segregation_indicator'

#tract level segregation index data frame
df <- df[, c("geoid", "measure", "year", "value")]



###########

#converting from tract to county
#tract to county

#initialize
df_county <- df

#performing aggregation, just taking first five digits of fips since it is county fips
# and grouping by those 5 digit geoid and perform aggregation
df_county_aggregated <- df_county %>%
  mutate(geoid = substr(geoid, 1, 5)) %>%
  group_by(geoid) %>%
  summarize(value = sum(value)) %>%
  select(geoid, value) %>%
  mutate(measure = "segregation_indicator", year = 2020) %>%
  select(geoid, measure, year, value)


#county to heath district

health_district_data <- read_csv("~/git/sdc.geographies_dev/VA/State Geographies/Health Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv")

#merging the data so easy for grouping
merged_data <- merge(df_county_aggregated, health_district_data, by.x = "geoid", by.y = "ct_geoid")

#grouping by hd_geoid and performing aggregation and finally having health district data frame
df_hd_aggregated <- merged_data %>%
  group_by(hd_geoid) %>%
  summarize(value = sum(value)) %>%
  select(geoid = hd_geoid, value) %>%
  mutate(measure = "segregation_indicator", year = 2020) %>%
  select(geoid, measure, year, value)

############

#combining tract, county, health district data frames
df_final <- rbind(df,df_county_aggregated,df_hd_aggregated)

#df_final$value <- round(df_final$value, 2)

#exporting the final data frame 
readr::write_csv(df_final,  xzfile("~/segregation_raw/va_tr_ct_hd_vdh_2020_segregation_index.csv.xz", compression = 9))

