# change measure name for NCR data 
library(dplyr)
library(sf)
# library(httr)
library(rjson)
library(tidyr)
library(readr)
library(tidycensus)
library(geojsonio)


# load ncr
df <- read_csv('Gender/data/distribution/ncr_cttrbg_acs_2009_2021_gender_demographics.csv.xz')

df_renamed <- df %>%
  mutate(measure=case_when(
    measure=="total_pop" ~ "gender_total_count",
    measure=="pop_male" ~ "gender_male_count",
    measure=="pop_female" ~ "gender_female_count",
    measure=="perc_male" ~ "gender_male_percent",
    measure=="perc_female" ~ "gender_female_percent")) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))

# save ncr
readr::write_csv(df_renamed, xzfile('Gender/data/distribution/ncr_cttrbg_acs_2009_2021_gender_demographics.csv.xz', compression = 9))


