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
df <- read_csv('Race/data/distribution/ncr_cttrbg_acs_2009_2021_race_demographics.csv.xz')

df_renamed <- df %>%
  mutate(measure=case_when(
    measure=="total_race" ~ "race_total_count_direct",
    measure=="pop_wht_alone" ~ "race_wht_alone_count_direct",
    measure=="pop_afr_amer_alone" ~ "race_afr_amer_alone_count_direct",
    measure=="pop_native_alone" ~ "race_native_alone_count_direct",
    measure=="pop_AAPI" ~ "race_AAPI_count_direct",
    measure=="pop_other" ~ "race_other_count_direct", 
    measure=="pop_two_or_more" ~ "race_two_or_more_count_direct",
    measure=="pop_hispanic_or_latino" ~ "race_hispanic_or_latino_count_direct",
    measure=="perc_wht_alone" ~ "race_wht_alone_percent_direct",
    measure=="perc_afr_amer_alone" ~ "race_afr_amer_alone_percent_direct",
    measure=="perc_native_alone" ~ "race_native_alone_percent_direct",
    measure=="perc_AAPI" ~ "race_AAPI_percent_direct",
    measure=="perc_two_or_more" ~ "race_two_or_more_percent_direct",
    measure=="perc_other" ~ "race_other_percent_direct",
    measure=="perc_hispanic_or_latino" ~ "race_hispanic_or_latino_percent_direct",
    measure=="pop_eth_tot" ~ "race_eth_tot_count_direct")) %>%
  filter(!is.na(value)) %>%
  mutate(geoid=as.character(geoid))

# save ncr
readr::write_csv(df_renamed, xzfile('Race/data/distribution/ncr_cttrbg_acs_2009_2021_race_demographics.csv.xz', compression = 9))


