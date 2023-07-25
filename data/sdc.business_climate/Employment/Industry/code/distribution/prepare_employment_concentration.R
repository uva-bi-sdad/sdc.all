# Measure the employment concentration using the Herfindalh Hirschman index

# library -----------------------------------------------------------------------------
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
library(plm)


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/Mergent_intellect/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# measure the employment concentration
industry_employment <- mi_fairfax_features %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment), .groups='drop') %>%
  group_by(year,naics_name) %>%
  summarise(emp_industry = sum(employment),
            measure = 'Herfindalh_Hirschman_index',
            value = round(sum( (100*employment/emp_industry)^2, na.rm=T),2) , .groups='keep') %>%
  select(year,naics_name,measure,value) %>%
  ungroup() %>%
  mutate(measure = paste0(naics_name,'_',measure),
         geoid = 51059,
         region_type = 'county',
         region_name = 'Fairfax county',
         measure_type = case_when(
           grepl('emp_industry',measure)==T ~ "count",
           grepl('Herfindalh_Hirschman_index',measure)==T ~ "index"),
         MOE='') %>%
  select(geoid,region_name,region_type,year,measure,value,measure_type,MOE)


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Industry/data/distribution/"
readr::write_csv(industry_employment, xzfile(paste0(savepath,"va059_ct_mi_",min(industry_employment$year),'_',max(industry_employment$year),"_Herfindalh_Hirschman_index_by_industry.csv.xz"), compression = 9))



