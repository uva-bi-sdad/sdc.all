# Mergent intellect: compute business characteristics such as 
#       - companies size (small=employee<15 - break small business into 1 employees and more than one)
#       - company type (public and private)
#       - company age
#       - Owner characteristics
#       - minority owned
#       - industry (naics 2 digits)


# library
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(reshape2)
library(tidyr)



# upload data on features and operations ----------------------------------------------------------------------

# upload companies located in fairfax 
uploadpath = "Microdata/data/working/"
mi_fairfax_geolocated <-  read_csv(paste0(uploadpath,"mi_fairfax_geolocated.csv.xz"))

# upload companies characteristics located in fairfax
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_companies_details.csv.xz")) %>%
  select(company_name=`Company Name`,
         duns = `D-U-N-S@ Number`,
         company_type = `Company Type`,
         founding_year = `Year of Founding`,
         trade_status = `Import/Export`,
         minority = `Minority Owned Indicator`,
         primary_naics = `Primary NAICS Code` ) %>%
  filter(duns %in% unique(mi_fairfax_geolocated$duns))

# upload fairfax companies operation (sales, employment and tax rate) (comments: just select operations you are interested and add)
mi_fairfax_operation <-  read_csv(paste0(uploadpath,"mi_financial_info.csv.xz")) %>%
  select(company_name=`Company Name`,
         duns = `D-U-N-S@ Number`,
         measure,
         value,
         year) %>%
  filter(measure %in% c( 'Sales Volume', 'Employee This Site' , 'Calculated Tax Rate %')) %>%
  filter(duns %in% unique(mi_fairfax_geolocated$duns)) %>%
  filter(year %in% c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")) %>%
  mutate(year=as.numeric(year),
         value=as.numeric(gsub('[$,]','',value))) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  select(company_name,
         duns,
         year,
         sales= `Sales Volume`,
         employment = `Employee This Site`)

  
# Compute the key statistics at the companies level ---------------------------------------------------------------------

# Compute the key statistics
temp1 <- mi_fairfax_features %>%
  mutate(minority = if_else(minority=='Yes',1,0),
         naics2=as.numeric(substr(primary_naics, 1, 2)),
         naics_name=case_when(
           naics2==11 ~ "Agriculture, Forestry, Fishing and Hunting",
           naics2==21 ~ "Mining, Quarrying, and Oil and Gas Extraction",
           naics2==22 ~ "Utilities",
           naics2==23 ~ "Construction",
           naics2==31 | naics2==32 | naics2==33 ~ "Manufacturing",
           naics2==42 ~ "Wholesale Trade",
           naics2==44 | naics2==45 ~ "Retail Trade",
           naics2==48 | naics2==49 ~ "Transportation and Warehousing",
           naics2==51 ~ "Information",
           naics2==52 ~ "Finance and Insurance",
           naics2==53 ~ "Real Estate and Rental and Leasing",
           naics2==54 ~ "Professional, Scientific, and Technical Services",
           naics2==55 ~ "Management of Companies and Enterprises",
           naics2==56 ~ "Administrative and Support and Waste Management and Remediation Services",
           naics2==61 ~ "Educational Services",
           naics2==62 ~ "Health Care and Social Assistance",
           naics2==71 ~ "Arts, Entertainment, and Recreation",
           naics2==72 ~ "Accommodation and Food Services",
           naics2==81 ~ "Other Services (except Public Administration)",
           naics2==92 ~ "Public Administration",
           naics2==99 ~ "Nonclassifiable Establishments")) 

# companies operation stats (solo_proprietor companies refers to companies with only )
temp2 <- mi_fairfax_operation %>%
  mutate(small = if_else(employment<50,1,0),
         sole_proprietor =if_else(employment==1,1,0))

# identify entry and exit
min_year <- min(temp2$year)
max_year <- max(temp2$year)
duns_after_time <- unique(temp2$duns[temp2$year>min_year])
temp3 <- temp2 %>% filter(year==min_year) %>% mutate(entry=NA, exit=if_else(duns %in% duns_after_time,0,1))

for (time in min(temp2$year)+1:max(temp2$year)){
  duns_prior_time <- unique(temp2$duns[temp2$year<time])
  duns_after_time <- unique(temp2$duns[temp2$year>time])
  temp4 <- temp2 %>% filter(year==time) %>% mutate(entry=if_else(duns %in% duns_prior_time,0,1), exit=if_else(duns %in% duns_after_time,0,1))
  temp3 <- rbind(temp3,temp4)
}
temp3 <- temp3 %>% mutate(exit=if_else(year==max_year,0,exit))


# merge features and operations
fairfax_features_operation <- merge(temp1, temp3, by.x=c('duns','company_name'), by.y=c('duns','company_name')) 

# add geo-location. Combine geoid based on the year variable
fairfax_features_operation_geo <- merge(fairfax_features_operation, mi_fairfax_geolocated, by.x=c('duns','company_name'), by.y=c('duns','company_name')) 
fairfax_features_operation_geo <- fairfax_features_operation_geo %>%
  mutate(geoid=if_else(year>=2020, geoid_2020, geoid_2010)) %>%
  select(-geoid_2020, -geoid_2010)


# save the data --------------------------------------------------------------------------------------------------------
savepath = "Microdata/data/working/"
readr::write_csv(fairfax_features_operation_geo, xzfile(paste0(savepath,"mi_fairfax_features_updated.csv.xz"), compression = 9))


