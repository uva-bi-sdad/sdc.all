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
df <- read_csv('Language/data/distribution/ncr_cttrbg_acs_2016_2021_language_demographics.csv.xz')

df_renamed <- df %>%
  mutate(measure=case_when(
    measure=="hh_limited_english" ~ "language_hh_limited_english_count_direct",
    measure=="perc_hh_limited_english" ~ "language_hh_limited_english_percent_direct")) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))

# save ncr
readr::write_csv(df_renamed, xzfile('Language/data/distribution/ncr_cttrbg_acs_2016_2021_language_demographics.csv.xz', compression = 9))


