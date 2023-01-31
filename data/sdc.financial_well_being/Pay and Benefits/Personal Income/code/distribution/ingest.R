# Ingests earnings per job from BEA
# packages
library(tidyverse)
library(jsonlite)

# ANNUAL PERSONAL INCOME AND EMPLOYMENT BY COUNTY ->
# PERSONAL INCOME AND EMPLOYMENT BY MAJOR COMPONENT

################################## earnings per job ########################################


bea_key <- Sys.getenv("BEA_API_KEY")

# Total employment is 7010

tot_emp <- fromJSON(paste0("https://apps.bea.gov/api/data/?UserID=", bea_key, "&method=GetData&datasetname=Regional&TableName=CAINC4&LineCode=7010&Year=2015,2016,2017,2018,2019,2020&GeoFips=VA&ResultFormat=json"))[["BEAAPI"]][["Results"]][["Data"]]
tot_emp <- tot_emp %>% filter(GeoFips!="51000") %>% mutate(DataValue = str_remove_all(DataValue, ",")) %>% select(GeoFips, GeoName, TimePeriod, DataValue) %>% pivot_wider(id_cols = c("GeoFips", "GeoName"), names_from = TimePeriod, values_from = DataValue) %>% mutate_at(c("2015", "2016", "2017", "2018", "2019", "2020"), as.numeric)

# Supplements to wages and salaries is 60

wage_sup <- fromJSON(paste0("https://apps.bea.gov/api/data/?UserID=", bea_key, "&method=GetData&datasetname=Regional&TableName=CAINC4&LineCode=60&Year=2015,2016,2017,2018,2019,2020&GeoFips=VA&ResultFormat=json"))[["BEAAPI"]][["Results"]][["Data"]]
wage_sup <- wage_sup %>% filter(GeoFips!="51000") %>% mutate(DataValue = str_remove_all(DataValue, ",")) %>% select(GeoFips, GeoName, TimePeriod, DataValue) %>% pivot_wider(id_cols = c("GeoFips", "GeoName"), names_from = TimePeriod, values_from = DataValue) %>% mutate_at(c("2015", "2016", "2017", "2018", "2019", "2020"), as.numeric)

# Wages and salaries is 50

wage_sal <- fromJSON(paste0("https://apps.bea.gov/api/data/?UserID=", bea_key, "&method=GetData&datasetname=Regional&TableName=CAINC4&LineCode=50&Year=2015,2016,2017,2018,2019,2020&GeoFips=VA&ResultFormat=json"))[["BEAAPI"]][["Results"]][["Data"]]
wage_sal <- wage_sal %>% filter(GeoFips!="51000") %>% mutate(DataValue = str_remove_all(DataValue, ",")) %>% select(GeoFips, GeoName, TimePeriod, DataValue) %>% pivot_wider(id_cols = c("GeoFips", "GeoName"), names_from = TimePeriod, values_from = DataValue) %>% mutate_at(c("2015", "2016", "2017", "2018", "2019", "2020"), as.numeric)

# Proprietors income is 70

prop_inc <- fromJSON(paste0("https://apps.bea.gov/api/data/?UserID=", bea_key, "&method=GetData&datasetname=Regional&TableName=CAINC4&LineCode=70&Year=2015,2016,2017,2018,2019,2020&GeoFips=VA&ResultFormat=json"))[["BEAAPI"]][["Results"]][["Data"]]
prop_inc <- prop_inc %>% filter(GeoFips!="51000") %>% mutate(DataValue = str_remove_all(DataValue, ",")) %>% select(GeoFips, GeoName, TimePeriod, DataValue) %>% pivot_wider(id_cols = c("GeoFips", "GeoName"), names_from = TimePeriod, values_from = DataValue) %>% mutate_at(c("2015", "2016", "2017", "2018", "2019", "2020"), as.numeric)

readr::write_csv(tot_emp, xzfile("./data/earnings/original/tot_emp.csv.xz", compression = 9))
readr::write_csv(wage_sup, xzfile("./data/earnings/original/wage_sup.csv.xz", compression = 9))
readr::write_csv(wage_sal, xzfile("./data/earnings/original/wage_sal.csv.xz", compression = 9))
readr::write_csv(prop_inc, xzfile("./data/earnings/original/prop_inc.csv.xz", compression = 9))
