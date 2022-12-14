# Food Security -- Data from Feeding America, Map the Meal Gap
# Creates data for overall food insecurity, children's food insecurity, average meal cost, 
#   and food budget shortfall.
#
# ACS data used to get standard region names and aggregate VA county level data to VA health districts
  
# load packages
  
library(readr)
library(tidycensus)
library(dplyr)
library(readxl)
library(tidyr)


# Data Ingestion ------------------------------- 

data_path = "Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Overall\ Food\ Insecurity/data/original/"

mmg2019 <- read_excel(paste0(data_path, "MMG2021_2019Data_ToShare.xlsx"), sheet = 2)
mmg2019$Food_Insecurity_Rate <- mmg2019$`2019 Food Insecurity Rate`
mmg2019$Child_Food_Insecurity_Rate <- mmg2019$`2019 Child food insecurity rate`
mmg2019$Cost_Per_Meal <- mmg2019$`2019 Cost Per Meal`
mmg2019$Num_Food_Insecure <- mmg2019$`# of Food Insecure Persons in 2019`
mmg2019$Num_Child_Food_Insecure <- mmg2019$`# of Food Insecure Children in 2019`
mmg2019$weighted_budget_shortfall <- mmg2019$`2019 Weighted Annual Food Budget Shortfall`

# there was a methodological change in 2018, but I keep the data (not sure how they changed it and if they also changed 2019)
mmg2018 <- read_excel(paste0(data_path, "MMG2020_2018Data_ToShare.xlsx"), sheet = 1)
mmg2018 <- mmg2018[2:nrow(mmg2018),]
mmg2018$Food_Insecurity_Rate <- mmg2018$...4
mmg2018$Child_Food_Insecurity_Rate <- mmg2018$...13
mmg2018$FIPS <- mmg2018$`Data from MMG 2020 are NOT directly comparable to data from any prior MMG study due to methodological changes made in 2020.`
mmg2018$Cost_Per_Meal <- mmg2018$...17
mmg2018$Num_Food_Insecure <- mmg2018$...5
mmg2018$Num_Child_Food_Insecure <- mmg2018$...14
mmg2018$weighted_budget_shortfall <- mmg2018$...18

mmg2017 <- read_excel(paste0(data_path, "MMG2019_2017Data_ToShare.xlsx"), sheet = 1)
mmg2017$Food_Insecurity_Rate <- mmg2017$`2017 Food Insecurity Rate`
mmg2017$Child_Food_Insecurity_Rate <- mmg2017$`2017 Child food insecurity rate`
mmg2017$Cost_Per_Meal <- mmg2017$`2017 Cost Per Meal`
mmg2017$Num_Food_Insecure <- mmg2017$`# of Food Insecure Persons in 2017`
mmg2017$Num_Child_Food_Insecure <- mmg2017$`# of Food Insecure Children in 2017`
mmg2017$weighted_budget_shortfall <- mmg2017$`2017 Weighted Annual Food Budget Shortfall`

mmg2016 <- read_excel(paste0(data_path, "MMG2018_2016Data_ToShare.xlsx"), sheet = 1)
mmg2016$Food_Insecurity_Rate <- mmg2016$`2016 Food Insecurity Rate`
mmg2016$Child_Food_Insecurity_Rate <- mmg2016$`2016 Child food insecurity rate`
mmg2016$Cost_Per_Meal <- mmg2016$`2016 Cost Per Meal`
mmg2016$Num_Food_Insecure <- mmg2016$`# of Food Insecure Persons in 2016`
mmg2016$Num_Child_Food_Insecure <- mmg2016$`# of Food Insecure Children in 2016`
mmg2016$weighted_budget_shortfall <- mmg2016$`2016 Weighted Annual Food Budget Shortfall`

mmg2015 <- read_excel(paste0(data_path, "MMG2017_2015Data_ToShare.xlsx"), sheet = 1)
mmg2015$Food_Insecurity_Rate <- mmg2015$`2015 Food Insecurity Rate`
mmg2015$Child_Food_Insecurity_Rate <- mmg2015$`2015 Child food insecurity rate`
mmg2015$Cost_Per_Meal <- mmg2015$`2015 Cost Per Meal`
mmg2015$Num_Food_Insecure <- mmg2015$`# of Food Insecure Persons in 2015`
mmg2015$Num_Child_Food_Insecure <- mmg2015$`# of Food Insecure Children in 2015`
mmg2015$weighted_budget_shortfall <- mmg2015$`2015 Weighted Annual Food Budget Shortfall`

mmg2014 <- read_excel(paste0(data_path, "MMG2016_2014Data_ToShare.xlsx"), sheet = 1)
mmg2014$Food_Insecurity_Rate <- mmg2014$`2014 Food Insecurity Rate`
mmg2014$Child_Food_Insecurity_Rate <- mmg2014$`2014 Child food insecurity rate`
mmg2014$Cost_Per_Meal <- mmg2014$`2014 Cost Per Meal`
mmg2014$Num_Food_Insecure <- mmg2014$`# of Food Insecure Persons in 2014`
mmg2014$Num_Child_Food_Insecure <- mmg2014$`# of Food Insecure Children in 2014`
mmg2014$weighted_budget_shortfall <- mmg2014$`2014 Weighted Annual Food Budget Shortfall`

mmg2013 <- read_excel(paste0(data_path, "MMG2015_2013Data_ToShare.xlsx"), sheet = 2)
mmg2013$Food_Insecurity_Rate <- mmg2013$`2013 Food Insecurity Rate`
mmg2013$Child_Food_Insecurity_Rate <- mmg2013$`2013 Child food insecurity rate` # 2 "-*" entries where there are 0 food insecure children. not MD, DC, or VA data.
mmg2013$Cost_Per_Meal <- mmg2013$`2013 Cost Per Meal`
mmg2013$Num_Food_Insecure <- mmg2013$`# of Food Insecure Persons in 2013`
mmg2013$Num_Child_Food_Insecure <- mmg2013$`# of Food Insecure Children in 2013`
mmg2013$weighted_budget_shortfall <- mmg2013$`2013 Weighted Annual Food Budget Shortfall`

mmg2012 <- read_excel(paste0(data_path, "MMG2014_2012Data_ToShare.xlsx"), sheet = 2)
mmg2012$Food_Insecurity_Rate <- mmg2012$`2012 Food Insecurity Rate`
mmg2012$Child_Food_Insecurity_Rate <- mmg2012$`2012 Child food insecurity rate`
mmg2012$Cost_Per_Meal <- mmg2012$`2012 Cost Per Meal`
mmg2012$Num_Food_Insecure <- mmg2012$`# of Food Insecure Persons in 2012`
mmg2012$Num_Child_Food_Insecure <- mmg2012$`# of Food Insecure Children in 2012`
mmg2012$weighted_budget_shortfall <- mmg2012$`2012 Weighted Annual Food Budget Shortfall`

# we dont have ACS variables needed for these years - not 100%
mmg2011 <- read_excel(paste0(data_path, "MMG2013_2011Data_ToShare.xlsx"), sheet = 2)
mmg2011$Food_Insecurity_Rate <- mmg2011$`2011 Food Insecurity Rate`
mmg2011$Child_Food_Insecurity_Rate <- mmg2011$`2011 Child Food Insecurity Rate`
mmg2011$Cost_Per_Meal <- mmg2011$`Cost Per Meal`
mmg2011$Num_Food_Insecure <- mmg2011$`Number of Food Insecure Persons in 2011`
mmg2011$Num_Child_Food_Insecure <- mmg2011$`Number of Food Insecure Children in 2011`
mmg2011$weighted_budget_shortfall <- mmg2011$`Weighted Annual Food Budget Shortfall`

mmg2010 <- read_excel(paste0(data_path, "MMG2012_2010Data_ToShare.xlsx"), sheet = 1)
mmg2010$Food_Insecurity_Rate <- mmg2010$`2010 Food Insecurity Rate`
mmg2010$Child_Food_Insecurity_Rate <- mmg2010$`2010 Child food insecurity rate`  # 5 NAs - not MD, DC, or VA data
mmg2010$Cost_Per_Meal <- mmg2010$`2010 Cost Per Meal`  # all missing
mmg2010$Num_Food_Insecure <- mmg2010$`Number of Food Insecure Persons in 2010`
mmg2010$Num_Child_Food_Insecure <- mmg2010$`Number of Food Insecure Children in 2010` #5 NAs - not MD, DC, or VA data
mmg2010$weighted_budget_shortfall <- mmg2010$`2010 Weighted Annual Food Budget Shortfall`

# we will only use data from 2014-2019
files <- list(mmg2019, mmg2018, mmg2017, mmg2016, mmg2015, mmg2014)


# COUNTY LEVEL FOOD INSECURITY ------------------------------------

# prepare feeding america data and standardize 

# start with 2019 and go backward
year <- 2019
all_county_fa_data <- matrix(, nrow = 0, ncol = 5)
for (i in 1:length(files))
{
  my_data_2 <- files[[i]]
  
  # change fips to characters and where codes only have 4 characters, add 0 to beginning
  my_data_2$FIPS <- as.character(my_data_2$FIPS)
  idxs <- which(nchar(my_data_2$FIPS) == 4)
  my_data_2$FIPS[idxs] <- paste0(0, my_data_2$FIPS[idxs])
  
  # merge acs data wth food insecurity data from feeding america
  county_fa_data <- my_data_2[, c("Food_Insecurity_Rate", "Child_Food_Insecurity_Rate", "Cost_Per_Meal", "Num_Food_Insecure", "Num_Child_Food_Insecure", "FIPS", "weighted_budget_shortfall")] %>% 
    mutate(yr = as.character(year))
  all_county_fa_data <- rbind(all_county_fa_data, county_fa_data)
  
  year <- year - 1
}


# format county level food insecurity measurements 

# pull ACS to get standard region names
us.co <- get_acs(geography = "county",
                 year = 2019,
                 variables = c(tpop = "B01003_001"),
                 survey = "acs5",
                 output = "wide",
                 geometry = FALSE)

all_county_fa_data_2 <- all_county_fa_data %>%
  mutate(Food_Insecurity_Rate = as.numeric(Food_Insecurity_Rate) * 100,
         Child_Food_Insecurity_Rate = as.numeric(Child_Food_Insecurity_Rate) * 100, # 2 -* entries --> NA, 5 NAs --> NA; not in MD, DC, or VA data
         Cost_Per_Meal = as.numeric(Cost_Per_Meal),
         weighted_budget_shortfall = as.numeric(weighted_budget_shortfall)
         ) %>%
  gather(measure, value, c(Food_Insecurity_Rate, Child_Food_Insecurity_Rate, Cost_Per_Meal, Num_Food_Insecure, Num_Child_Food_Insecure, weighted_budget_shortfall)) %>%
  merge(us.co[, c("GEOID", "NAME")], by.x = "FIPS", by.y = "GEOID", all.x = TRUE) %>% # 2 2014 fips with no match in 2019 census data, not MD, DC, or VA data
  rename(geoid = FIPS,
         year = yr,
         region_name = NAME) %>%
  mutate(measure_type = ifelse(measure %in% c("Food_Insecurity_Rate", "Child_Food_Insecurity_Rate"), "percent", 
                               ifelse(measure %in% c("Cost_Per_Meal", "weighted_budget_shortfall"), "cost", 
                                      "count")),
         region_type = "county") %>%
  relocate("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type") %>%
  mutate(value = as.numeric(value))


va_ct_fa_data <- all_county_fa_data_2[substr(all_county_fa_data_2$geoid, 1, 2) == "51",]

# filter and save NCR - county level data
ncr_fa_data <- all_county_fa_data_2 %>% 
  filter(geoid %in% c("51013", "51059", "51107", "51510", "51600", "51153", "51683", "51685", "51610", "11001", "24031", "24033", "24017", "24021"))

write_csv(ncr_fa_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Overall\ Food\ Insecurity/data/distribution/ncr_ct_fa_2014_2019_food_security.csv.xz", compression = 9))



# HEALTH DISTRICT LEVEL FOOD INSECURITY --------------------------------

# get necessary county level ACS variables 

# start with 2019 and go backward
year <- 2019
all_county_fa_data <- matrix(, nrow = 0, ncol = 7)
for (i in 1:length(files))
{
  us.co <- get_acs(geography = "county",
                   year = year,
                   variables = c(tpop = "B01003_001",
                                 male_under_5 = "B01001_003",
                                 male_5_9 = "B01001_004",
                                 male_10_14 = "B01001_005",
                                 male_15_17 = "B01001_006",
                                 female_under_5 = "B01001_027",
                                 female_5_9 = "B01001_028",
                                 female_10_14 = "B01001_029",
                                 female_15_17 = "B01001_030"),
                   survey = "acs5",
                   output = "wide",
                   geometry = FALSE)
  
  us.co2 <- us.co %>%
    mutate(child_pop = male_under_5E + male_5_9E + male_10_14E + male_15_17E + female_under_5E + female_5_9E + female_10_14E + female_15_17E)
  
  my_data_2 <- files[[i]]
  
  # change fips to characters and where codes only have 4 characters, add 0 to beginning
  my_data_2$FIPS <- as.character(my_data_2$FIPS)
  idxs <- which(nchar(my_data_2$FIPS) == 4)
  my_data_2$FIPS[idxs] <- paste0(0, my_data_2$FIPS[idxs])
  
  # merge acs data wth food insecurity data from feeding america
  county_fa_data <- merge(my_data_2[, c("Food_Insecurity_Rate", "Child_Food_Insecurity_Rate", "Cost_Per_Meal", "Num_Food_Insecure", "Num_Child_Food_Insecure", "FIPS", "weighted_budget_shortfall")], us.co2[, c("tpopE", "child_pop", "GEOID")], by.x = "FIPS", by.y = "GEOID") %>% mutate(yr = year)
  all_county_fa_data <- rbind(all_county_fa_data, county_fa_data)
  
  year <- year - 1
}

# formatting health district data

health_district <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

va_county_fa_data_db <- all_county_fa_data[substr(all_county_fa_data$FIPS, 1, 2) == "51", ]  
va_county_fa_data_db_2 <- va_county_fa_data_db %>%
  merge(health_district, by.x = "FIPS", by.y = "ct_geoid", all.x = TRUE) %>%
  mutate(yr = as.character(yr),
         Cost_Per_Meal = as.numeric(Cost_Per_Meal),
         Num_Food_Insecure = as.numeric(Num_Food_Insecure),
         Num_Child_Food_Insecure = as.numeric(Num_Child_Food_Insecure),
         weighted_budget_shortfall = as.numeric(weighted_budget_shortfall)) %>%
  group_by(hd_geoid, hd_name, yr) %>%
  summarise(tpop = sum(tpopE),
            Tot_Food_Insecure = sum(Num_Food_Insecure),
            child_pop = sum(child_pop),
            Tot_Child_Food_Insecure = sum(Num_Child_Food_Insecure),
            cost_x_people = sum(Cost_Per_Meal * tpopE),
            Tot_weighted_budget_shortfall = sum(weighted_budget_shortfall))

va_county_fa_data_db_3 <- va_county_fa_data_db_2 %>%
  mutate(Food_Insecurity_Rate = (Tot_Food_Insecure / tpop) * 100,
         Child_Food_Insecurity_Rate = (Tot_Child_Food_Insecure / child_pop) * 100,
         Cost_Per_Meal = cost_x_people / tpop) %>%
  select(-c(tpop, child_pop, cost_x_people)) %>%
  rename(Num_Child_Food_Insecure = Tot_Child_Food_Insecure,
         Num_Food_Insecure = Tot_Food_Insecure,
         weighted_budget_shortfall = Tot_weighted_budget_shortfall) %>%
  gather(measure, value, c(Num_Food_Insecure, Num_Child_Food_Insecure, Food_Insecurity_Rate, Child_Food_Insecurity_Rate, Cost_Per_Meal, weighted_budget_shortfall)) %>%
  mutate(measure_type = ifelse(measure %in% c("Food_Insecurity_Rate", "Child_Food_Insecurity_Rate"), "percent", 
                               ifelse(measure %in% c("Cost_Per_Meal", "weighted_budget_shortfall"), "cost", 
                                                     "count")),
         region_type = "health district") %>%
  rename(region_name = hd_name,
         year = yr,
         geoid = hd_geoid) %>%
  relocate("geoid", "region_type", "region_name", "year", "measure", "value", "measure_type")


# bind with VA county data
va_hdct_data <- rbind(va_ct_fa_data, va_county_fa_data_db_3)

# WRITE
write_csv(va_hdct_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Overall\ Food\ Insecurity/data/distribution/va_hdct_fa_2014_2019_food_security.csv.xz", compression = 9))



