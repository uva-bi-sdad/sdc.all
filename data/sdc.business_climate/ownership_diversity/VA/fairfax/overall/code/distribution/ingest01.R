# download all lodes data for virginia

# library
library(lehdr)
library(tidyr)


# Extract LODES WAC

# block group level ----------------------------------------------------------------------------
selected_year <- 2010:2019
lodes_va_bg <- grab_lodes(state = 'va', 
                    year = selected_year, 
                    lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                    job_type = "JT00",        # all jobs 
                    segment = "S000", 
                    state_part = "main",
                    agg_geo = "bg" ) %>% select(year, state, geoid=w_bg, starts_with('CNS'))

# reshape, create geometry type, create naics names
lodes_va_bg_lg <- lodes_va_bg %>% 
  pivot_longer(cols = starts_with("CNS"), names_to = "naics_id", values_to = "jobs") %>%
  mutate(geotype='block group',
         naics_name=case_when(
           naics_id=='CNS01' ~ "Agriculture, Forestry, Fishing and Hunting",
           naics_id=='CNS02' ~ "Mining, Quarrying, and Oil and Gas Extraction",
           naics_id=='CNS03' ~ "Utilities",
           naics_id=='CNS04' ~ "Construction",
           naics_id=='CNS05' ~ "Manufacturing",
           naics_id=='CNS06' ~ "Wholesale Trade",
           naics_id=='CNS07' ~ "Retail Trade",
           naics_id=='CNS08' ~ "Transportation and Warehousing",
           naics_id=='CNS09' ~ "Information",
           naics_id=='CNS10' ~ "Finance and Insurance",
           naics_id=='CNS11' ~ "Real Estate and Rental and Leasing",
           naics_id=='CNS12' ~ "Professional, Scientific, and Technical Services",
           naics_id=='CNS13' ~ "Management of Companies and Enterprises",
           naics_id=='CNS14' ~ "Administrative and Support and Waste Management and Remediation Services",
           naics_id=='CNS15' ~ "Educational Services",
           naics_id=='CNS16' ~ "Health Care and Social Assistance",
           naics_id=='CNS17' ~ "Arts, Entertainment, and Recreation",
           naics_id=='CNS18' ~ "Accommodation and Food Services",
           naics_id=='CNS19' ~ "Other Services (except Public Administration)",
           naics_id=='CNS20' ~ "Public Administration")) %>%
  select(state,year,geoid,geotype,naics_name,jobs)


# tract level -----------------------------------------------------------------------------------------
lodes_va_tract <- grab_lodes(state = 'va', 
                          year = selected_year, 
                          lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                          job_type = "JT00",        # all jobs 
                          segment = "S000", 
                          state_part = "main",
                          agg_geo = "tract" ) %>% select(year, state, geoid=w_tract, starts_with('CNS'))

# reshape, create geometry type, create naics names
lodes_va_tract_lg <- lodes_va_tract %>% 
  pivot_longer(cols = starts_with("CNS"), names_to = "naics_id", values_to = "jobs") %>%
  mutate(geotype='tract',
         naics_name=case_when(
           naics_id=='CNS01' ~ "Agriculture, Forestry, Fishing and Hunting",
           naics_id=='CNS02' ~ "Mining, Quarrying, and Oil and Gas Extraction",
           naics_id=='CNS03' ~ "Utilities",
           naics_id=='CNS04' ~ "Construction",
           naics_id=='CNS05' ~ "Manufacturing",
           naics_id=='CNS06' ~ "Wholesale Trade",
           naics_id=='CNS07' ~ "Retail Trade",
           naics_id=='CNS08' ~ "Transportation and Warehousing",
           naics_id=='CNS09' ~ "Information",
           naics_id=='CNS10' ~ "Finance and Insurance",
           naics_id=='CNS11' ~ "Real Estate and Rental and Leasing",
           naics_id=='CNS12' ~ "Professional, Scientific, and Technical Services",
           naics_id=='CNS13' ~ "Management of Companies and Enterprises",
           naics_id=='CNS14' ~ "Administrative and Support and Waste Management and Remediation Services",
           naics_id=='CNS15' ~ "Educational Services",
           naics_id=='CNS16' ~ "Health Care and Social Assistance",
           naics_id=='CNS17' ~ "Arts, Entertainment, and Recreation",
           naics_id=='CNS18' ~ "Accommodation and Food Services",
           naics_id=='CNS19' ~ "Other Services (except Public Administration)",
           naics_id=='CNS20' ~ "Public Administration")) %>%
  select(state,year,geoid,geotype,naics_name,jobs)

# county level -----------------------------------------------------------------------------------------
lodes_va_county <- grab_lodes(state = 'va', 
                             year = selected_year, 
                             lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                             job_type = "JT00",        # all jobs 
                             segment = "S000", 
                             state_part = "main",
                             agg_geo = "county" ) %>% select(year, state, geoid=w_county, starts_with('CNS'))

# reshape, create geometry type, create naics names
lodes_va_county_lg <- lodes_va_county %>% 
  pivot_longer(cols = starts_with("CNS"), names_to = "naics_id", values_to = "jobs") %>%
  mutate(geotype='county',
         naics_name=case_when(
           naics_id=='CNS01' ~ "Agriculture, Forestry, Fishing and Hunting",
           naics_id=='CNS02' ~ "Mining, Quarrying, and Oil and Gas Extraction",
           naics_id=='CNS03' ~ "Utilities",
           naics_id=='CNS04' ~ "Construction",
           naics_id=='CNS05' ~ "Manufacturing",
           naics_id=='CNS06' ~ "Wholesale Trade",
           naics_id=='CNS07' ~ "Retail Trade",
           naics_id=='CNS08' ~ "Transportation and Warehousing",
           naics_id=='CNS09' ~ "Information",
           naics_id=='CNS10' ~ "Finance and Insurance",
           naics_id=='CNS11' ~ "Real Estate and Rental and Leasing",
           naics_id=='CNS12' ~ "Professional, Scientific, and Technical Services",
           naics_id=='CNS13' ~ "Management of Companies and Enterprises",
           naics_id=='CNS14' ~ "Administrative and Support and Waste Management and Remediation Services",
           naics_id=='CNS15' ~ "Educational Services",
           naics_id=='CNS16' ~ "Health Care and Social Assistance",
           naics_id=='CNS17' ~ "Arts, Entertainment, and Recreation",
           naics_id=='CNS18' ~ "Accommodation and Food Services",
           naics_id=='CNS19' ~ "Other Services (except Public Administration)",
           naics_id=='CNS20' ~ "Public Administration")) %>%
  select(state,year,geoid,geotype,naics_name,jobs)

# combine all the geography level and save the data
va_lodes_bg_tr_co_20102019 <- rbind(lodes_va_bg_lg,lodes_va_tract_lg,lodes_va_county_lg)

# save the data --------------------------------------------------------------------------------------------------------
savepath = "ownership_diversity/VA/fairfax/overall/data/working/"
readr::write_csv(va_lodes_bg_tr_co_20102019, xzfile(paste0(savepath,"va_lodes_bg_tr_co_20102019.csv.xz"), compression = 9))


