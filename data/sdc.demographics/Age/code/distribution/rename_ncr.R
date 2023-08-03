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
df <- read_csv('Age/data/distribution/ncr_cttrbg_acs_2009_2021_age_demographics.csv.xz')

df_renamed <- df %>%
  mutate(measure=case_when(
    measure=="total_pop" ~ "age_total_count",
    measure=="pop_under_20" ~ "age_under_20_count",
    measure=="pop_20_64" ~ "age_20_64_count",
    measure=="pop_65_plus" ~ "age_65_plus_count",
    measure=="perc_pop_under_20" ~ "age_under_20_percent",
    measure=="perc_pop_20_64" ~ "age_20_64_percent",
    measure=="perc_pop_65_plus" ~ "age_65_plus_percent")) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))

# save ncr
readr::write_csv(df_renamed, xzfile('Age/data/distribution/ncr_cttrbg_acs_2009_2021_age_demographics.csv.xz', compression = 9))


