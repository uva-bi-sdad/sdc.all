# prepare Feeding America sub-county food insecurity data

library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

#
# ingest original data -----------------
#

data_path = "Food\ Security/Sub-County\ Estimates/Census\ Tracts/data/original/"

df <- read_excel(paste0(data_path, "US_tract_2020.xlsx"), sheet = 1)


#
# prepare data for VA -----------------
#

va_df <- df %>%
  filter(state == "VA") %>%
  select(TractID, geography, total_population_2020, 
         percent_food_insecure_2020, number_food_insecure_2020) %>%
  rename(geoid = TractID, 
         region_name = geography,
         total_population = total_population_2020,
         percent_food_insecure = percent_food_insecure_2020,
         number_food_insecure = number_food_insecure_2020) %>%
  mutate(region_type = "tract", 
         year = "2020",
         moe = "",
         measure_type = "count",
         percent_food_insecure = percent_food_insecure*100) %>%
  pivot_longer(cols = total_population:number_food_insecure,
               names_to = "measure",
               values_to = "value")

va_df[va_df$measure == "percent_food_insecure", "measure_type"] <- "percent"


# order columns

va_df <- va_df[ , c(1,3,2,4,7,8,6,5)]


#
# prepare data for NCR ---------------------
#

ncr_df <- df %>%
  filter(state == "VA" | state == "MD" | state == "DC") %>%
  select(TractID, geography, total_population_2020, 
         percent_food_insecure_2020, number_food_insecure_2020) %>%
  rename(geoid = TractID, 
         region_name = geography,
         total_population = total_population_2020,
         percent_food_insecure = percent_food_insecure_2020,
         number_food_insecure = number_food_insecure_2020) %>%
  mutate(region_type = "tract", 
         year = "2020",
         moe = "",
         measure_type = "count",
         percent_food_insecure = percent_food_insecure*100) %>%
  pivot_longer(cols = total_population:number_food_insecure,
               names_to = "measure",
               values_to = "value")

ncr_df[ncr_df$measure == "percent_food_insecure", "measure_type"] <- "percent"


# order columns

ncr_df <- ncr_df[ , c(1,3,2,4,7,8,6,5)]

# filter to NCR only

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")
ncr_df <- ncr_df %>% 
  filter(str_detect(geoid, ncr_counties))


#
# write data ---------------------------------
#

write_csv(va_df, xzfile("Food\ Security/Sub-County\ Estimates/Census\ Tracts/data/distribution/va_tr_fa_2020_food_insecurity.csv.xz", compression = 9))
write_csv(ncr_df, xzfile("Food\ Security/Sub-County\ Estimates/Census\ Tracts/data/distribution/ncr_tr_fa_2020_food_insecurity.csv.xz", compression = 9))

