# download all lodes data for NCR

# library
library(lehdr)
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(reshape2)
library(crosstable)
library(tidyr)
library(scales)


# Extract LODES WAC

# block group level ----------------------------------------------------------------------------
selected_year <- 2010:2019
ncr <- c('va','dc','md')
lodes_ncr_bg <- grab_lodes(state = ncr, 
                          year = selected_year, 
                          lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                          job_type = "JT00",        # all jobs 
                          segment = "S000", 
                          state_part = "main",
                          agg_geo = "bg" ) %>% select(year, state, geoid=w_bg, starts_with('CNS'),starts_with('CR'),starts_with('CS'))

# reshape, create geometry type, create naics names
lodes_ncr_bg_lg <- lodes_ncr_bg %>% 
  pivot_longer(cols = starts_with("C"), names_to = "var_id", values_to = "jobs") %>%
  mutate(region_type='block group',
         var_name=case_when(
           var_id=='CNS01' ~ "Agriculture, Forestry, Fishing and Hunting",
           var_id=='CNS02' ~ "Mining, Quarrying, and Oil and Gas Extraction",
           var_id=='CNS03' ~ "Utilities",
           var_id=='CNS04' ~ "Construction",
           var_id=='CNS05' ~ "Manufacturing",
           var_id=='CNS06' ~ "Wholesale Trade",
           var_id=='CNS07' ~ "Retail Trade",
           var_id=='CNS08' ~ "Transportation and Warehousing",
           var_id=='CNS09' ~ "Information",
           var_id=='CNS10' ~ "Finance and Insurance",
           var_id=='CNS11' ~ "Real Estate and Rental and Leasing",
           var_id=='CNS12' ~ "Professional, Scientific, and Technical Services",
           var_id=='CNS13' ~ "Management of Companies and Enterprises",
           var_id=='CNS14' ~ "Administrative and Support and Waste Management and Remediation Services",
           var_id=='CNS15' ~ "Educational Services",
           var_id=='CNS16' ~ "Health Care and Social Assistance",
           var_id=='CNS17' ~ "Arts, Entertainment, and Recreation",
           var_id=='CNS18' ~ "Accommodation and Food Services",
           var_id=='CNS19' ~ "Other Services (except Public Administration)",
           var_id=='CNS20' ~ "Public Administration",
           var_id=='CR01' ~ "White",
           var_id=='CR02' ~ "Black or African american",
           var_id=='CR03' ~ "American indian and Alaska",
           var_id=='CR04' ~ "Asian",
           var_id=='CR05' ~ "Hawaiian and other islander",
           var_id=='CR07' ~ "Two or more races",
           var_id=='CS01' ~ "Male",
           var_id=='CS02' ~ "Female"),
         measure = str_replace_all(paste0(var_name,'_number_jobs'),' ','_'),
         measure_type='count',
         MOE='') %>%
  select(geoid,region_type,region_name=state,year,measure,value=jobs,measure_type,MOE) 
  


# tract level -----------------------------------------------------------------------------------------
lodes_ncr_tract <- grab_lodes(state = ncr, 
                             year = selected_year, 
                             lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                             job_type = "JT00",        # all jobs 
                             segment = "S000", 
                             state_part = "main",
                             agg_geo = "tract" ) %>% select(year, state, geoid=w_tract, starts_with('CNS'),starts_with('CR'),starts_with('CS'))

# reshape, create geometry type, create naics names
lodes_ncr_tract_lg <- lodes_ncr_tract %>% 
  pivot_longer(cols = starts_with("C"), names_to = "var_id", values_to = "jobs") %>%
  mutate(region_type='tract',
         var_name=case_when(
           var_id=='CNS01' ~ "Agriculture, Forestry, Fishing and Hunting",
           var_id=='CNS02' ~ "Mining, Quarrying, and Oil and Gas Extraction",
           var_id=='CNS03' ~ "Utilities",
           var_id=='CNS04' ~ "Construction",
           var_id=='CNS05' ~ "Manufacturing",
           var_id=='CNS06' ~ "Wholesale Trade",
           var_id=='CNS07' ~ "Retail Trade",
           var_id=='CNS08' ~ "Transportation and Warehousing",
           var_id=='CNS09' ~ "Information",
           var_id=='CNS10' ~ "Finance and Insurance",
           var_id=='CNS11' ~ "Real Estate and Rental and Leasing",
           var_id=='CNS12' ~ "Professional, Scientific, and Technical Services",
           var_id=='CNS13' ~ "Management of Companies and Enterprises",
           var_id=='CNS14' ~ "Administrative and Support and Waste Management and Remediation Services",
           var_id=='CNS15' ~ "Educational Services",
           var_id=='CNS16' ~ "Health Care and Social Assistance",
           var_id=='CNS17' ~ "Arts, Entertainment, and Recreation",
           var_id=='CNS18' ~ "Accommodation and Food Services",
           var_id=='CNS19' ~ "Other Services (except Public Administration)",
           var_id=='CNS20' ~ "Public Administration",
           var_id=='CR01' ~ "White",
           var_id=='CR02' ~ "Black or African american",
           var_id=='CR03' ~ "American indian and Alaska",
           var_id=='CR04' ~ "Asian",
           var_id=='CR05' ~ "Hawaiian and other islander",
           var_id=='CR07' ~ "Two or more races",
           var_id=='CS01' ~ "Male",
           var_id=='CS02' ~ "Female"),
         measure = str_replace_all(paste0(var_name,'_number_jobs'),' ','_'),
         measure_type='count',
         MOE='') %>%
  select(geoid,region_type,region_name=state,year,measure,value=jobs,measure_type,MOE)


# county level -----------------------------------------------------------------------------------------
lodes_ncr_county <- grab_lodes(state = ncr, 
                              year = selected_year, 
                              lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                              job_type = "JT00",        # all jobs 
                              segment = "S000", 
                              state_part = "main",
                              agg_geo = "county" ) %>% select(year, state, geoid=w_county, starts_with('CNS'),starts_with('CR'),starts_with('CS'))

# reshape, create geometry type, create naics names
lodes_ncr_county_lg <- lodes_ncr_county %>% 
  pivot_longer(cols = starts_with("C"), names_to = "var_id", values_to = "jobs") %>%
  mutate(region_type='county',
         var_name=case_when(
           var_id=='CNS01' ~ "Agriculture, Forestry, Fishing and Hunting",
           var_id=='CNS02' ~ "Mining, Quarrying, and Oil and Gas Extraction",
           var_id=='CNS03' ~ "Utilities",
           var_id=='CNS04' ~ "Construction",
           var_id=='CNS05' ~ "Manufacturing",
           var_id=='CNS06' ~ "Wholesale Trade",
           var_id=='CNS07' ~ "Retail Trade",
           var_id=='CNS08' ~ "Transportation and Warehousing",
           var_id=='CNS09' ~ "Information",
           var_id=='CNS10' ~ "Finance and Insurance",
           var_id=='CNS11' ~ "Real Estate and Rental and Leasing",
           var_id=='CNS12' ~ "Professional, Scientific, and Technical Services",
           var_id=='CNS13' ~ "Management of Companies and Enterprises",
           var_id=='CNS14' ~ "Administrative and Support and Waste Management and Remediation Services",
           var_id=='CNS15' ~ "Educational Services",
           var_id=='CNS16' ~ "Health Care and Social Assistance",
           var_id=='CNS17' ~ "Arts, Entertainment, and Recreation",
           var_id=='CNS18' ~ "Accommodation and Food Services",
           var_id=='CNS19' ~ "Other Services (except Public Administration)",
           var_id=='CNS20' ~ "Public Administration",
           var_id=='CR01' ~ "White",
           var_id=='CR02' ~ "Black or African american",
           var_id=='CR03' ~ "American indian and Alaska",
           var_id=='CR04' ~ "Asian",
           var_id=='CR05' ~ "Hawaiian and other islander",
           var_id=='CR07' ~ "Two or more races",
           var_id=='CS01' ~ "Male",
           var_id=='CS02' ~ "Female"),
         measure = str_replace_all(paste0(var_name,'_number_jobs'),' ','_'),
         measure_type='count',
         MOE='') %>%
  select(geoid,region_type,region_name=state,year,measure,value=jobs,measure_type,MOE)

# combine all the geography level and save the data
ncr_lodes_bg_tr_co_20102019 <- rbind(lodes_ncr_bg_lg, lodes_ncr_tract_lg, lodes_ncr_county_lg)

# save the data --------------------------------------------------------------------------------------------------------
savepath = "employment_diversity/VA/fairfax/overall/data/distribution/"
readr::write_csv(ncr_lodes_bg_tr_co_20102019, xzfile(paste0(savepath,"vadcmd_bgtrct_lodes_20102019_number_of_jobs.csv.xz"), compression = 9))


