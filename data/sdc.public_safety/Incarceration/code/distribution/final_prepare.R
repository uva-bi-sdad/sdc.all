#code for tract level data

#libraries
library(readr)
library(tigris)
library(tidycensus)
library(dplyr)
library(readxl)

#reading the original data downloaded from prison policy
data_2020_tract <- read_csv("~/git/sdc.public_safety_dev/Incarceration/data/original/incarceration_tract_data_2020.csv")



#converting to dataframe
data_2020_tract <- data.frame(data_2020_tract)

#renaming the column names for our convenience

colnames(data_2020_tract) <- c(
  "FIPS",
  "Census_tracts",
  "People_prison",
  "Census_population",
  "Total_population",
  "Incarceration_rate_per_100000"
)

#Selecting required columns and changing it to our format
data_2020_df_tract <- data_2020_tract %>%
  select(FIPS, Incarceration_rate_per_100000) %>%
  mutate(
    geoid = FIPS,
    measure = "incarceration_rate_per_100000",
    year = 2020,
    value = Incarceration_rate_per_100000
  ) %>%
  select(geoid, measure, year, value)

#exporting the file
#write.csv(data_2020_df_tract, "~/git/sdc.public_safety_dev/Incarceration/data/distribution/va_tr_2020_incarceration_rate.csv", row.names = FALSE)


#code for county level data

library(readr)
library(tigris)
library(tidycensus)
library(dplyr)

data_2020_county <- read_csv("~/git/sdc.public_safety_dev/Incarceration/data/original/incarceration_county_data_2020.csv")


data_2020_county <- data.frame(data_2020_county)

#renaming the column names for our convenience

colnames(data_2020_county) <- c(
  "FIPS",
  "Census_tracts",
  "People_prison",
  "Census_population",
  "Total_population",
  "Incarceration_rate_per_100000"
)

data_2020_df_county <- data_2020_county %>%
  select(FIPS, Incarceration_rate_per_100000) %>%
  mutate(
    geoid = FIPS,
    measure = "incarceration_rate_per_100000",
    year = 2020,
    value = Incarceration_rate_per_100000
  ) %>%
  select(geoid, measure, year, value)

#write.csv(data_2020_df_county, "~/git/sdc.public_safety_dev/Incarceration/data/distribution/va_ct_2020_incarceration_rate.csv", row.names = FALSE)




#####health district level

#getting the county level   data to perform hd aggregation


county_df <- read_csv("~/git/sdc.public_safety_dev/Incarceration/data/original/incarceration_county_data_2020.csv")

colnames(county_df) <- c("fips", "name", "number_of_people", "census_pop", "total_pop", "value")


#########

#for health district, we need to convert the normalized per 100,000 to absolute incarcerations
#aggregating  the sum of the county population to get the health district population
# reading health district data


health_district_data <- read_csv("~/git/sdc.geographies_dev/VA/State Geographies/Health Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv")

#county population from acs
#Since county df actually do have county population, we can skip using ACS to get county population and we can directly use prison policy's county population
# va_acs <- get_acs(geography = "county",
#                   variables = "B01003_001",
#                   year = 2020,
#                   survey = "acs5",
#                   state = "VA")

#merging them
#merged_data <- merge(va_acs, health_district_data, by.x = "GEOID", by.y = "ct_geoid")

merged_data_1 <- merge(county_df, health_district_data, by.x = "fips", by.y = "ct_geoid")

#aggregating the dataset by health district geoid to get health district population
#aggregating county population to hd
# aggregated_data <- merged_data %>%
#   group_by(hd_geoid) %>%
#   summarize(estimate = sum(estimate))

#census pop is total population in county reported by census in prison policy dataset
aggregated_data_1 <- merged_data_1 %>%
     group_by(hd_geoid) %>%
     summarize(estimate = sum(census_pop))


#####
#aggregating number of people in jail county level to health district

#merging health district data and county level prison data
health_district_inc <- merge(x = health_district_data, y = county_df, by.x = "ct_geoid", by.y = "fips")

#removing ',' and converting to  numeric since it is character
health_district_inc$number_of_people <- as.numeric(gsub(",", "", health_district_inc$number_of_people))

health_district_inc_sum <- health_district_inc %>%
  group_by(hd_geoid) %>%
  summarise(
    total_number = sum(number_of_people, na.rm = TRUE)
  )


#####

#health_district_inc_sum

#aggregated_data

#combining the health district total population dataset with total incarcerated people data
#total number is number of people in jail and estimate is total population

#combined_data <-  left_join(health_district_inc_sum ,aggregated_data , by = "hd_geoid")
combined_data_1 <-  left_join(health_district_inc_sum ,aggregated_data_1 , by = "hd_geoid")


# final_hd <- combined_data %>%
#   mutate(
#     value = (value / estimate) * 100000,
#     year = 2020,
#     measure = "incarceration_rate_per_100000",
#     geoid = hd_geoid
#   ) %>%
#   select(geoid, measure, year, value)

#converting the normalized per 100,000 to absolute incarcerations

final_hd_1 <- combined_data_1 %>%
  mutate(
    value = (total_number / estimate) * 100000,
    year = 2020,
    measure = "incarceration_rate_per_100000",
    geoid = hd_geoid
  ) %>%
  select(geoid, measure, year, value)

#final_hd$value <- round(final_hd$value, 2)

final_hd_1$value <- round(final_hd_1$value, 0)


#health_district_inc_sum <- final_hd

health_district_inc_sum <- final_hd_1

#making sure all are df type
data_2020_df_tract <- data.frame(data_2020_df_tract)
data_2020_df_county <- data.frame(data_2020_df_county)
health_district_inc_sum <- data.frame(health_district_inc_sum)

#combining tract and county data
combined_df <- rbind(data_2020_df_tract, data_2020_df_county,health_district_inc_sum)

write.csv(combined_df, xzfile("Incarceration/data/distribution/va_hdcttr_2020_incarceration_rate.csv.xz"), row.names = FALSE)

# #Rex is fine with using 2020 data for previous years so we are using the same dataset for  multiple years 
# years_repeat <- c(2015, 2016, 2017, 2018, 2019, 2020)
# 
# replicated_df <- data.frame()
# 
# for (year in years_repeat) {
# 
#     temp_df <- combined_df
# 
#   temp_df$year <- year
# 
#   replicated_df <- rbind(replicated_df, temp_df)
# }
# 
# #readr::write_csv(replicated_df,xzfile("~/incarceration/va_hdcttr_2015_2020_incarceration_rate.csv.xz", compression = 9))
# 
# #readr::write_csv(replicated_df,xzfile("~/git/sdc.public_safety_dev/Incarceration/data/distribution/va_hdcttr_2020_incarceration_rate.csv.xz",compression = 9))
# 
# 
# write.csv(replicated_df, "~/incarceration/va_incarceration_2015_2020.csv", row.names = FALSE)