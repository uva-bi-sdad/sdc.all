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
df <- read_csv('Veteran/data/distribution/ncr_cttrbg_acs_2009_2021_veteran_demographics.csv.xz')

df_renamed <- df %>%
  mutate(measure=case_when(
    measure=="pop_veteran" ~ "veteran_count_direct",
    measure=="perc_veteran" ~ "veteran_percent_direct")) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))

# save ncr
readr::write_csv(df_renamed, xzfile('Veteran/data/distribution/ncr_cttrbg_acs_2009_2021_veteran_demographics.csv.xz', compression = 9))


