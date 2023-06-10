# Collection of Industry Specific Agriculture

library('quickerstats')
library(tidyverse)

key <- Sys.getenv('NASS_KEY')

vignette('quickerstats')

# <!------------------------------------------------------->
# <!------------ Searching for Desired Items -------------->
# <!------------------------------------------------------->

items <- search_data_items(key = key, search_terms = "")
items <- data.frame(items)
View(items)

search_terms <- read.csv("Industry specific/Agriculture/code/distribution/measures.csv")

total <- NULL
year <- 2017

for(i in 1:nrow(search_terms)){
  Sys.sleep(3)
  curr.df <- get_county_data(key=key, year = year, data_item=search_terms[i, 1], fips='51')
  
  if(is.null(curr.df)){
    Sys.sleep(3)
    curr.df <- get_county_data(key=key, year = 2017, data_item=search_terms[i, 1], fips='51', domain = "all")
  }
  
  addTo <- curr.df %>% select(state_fips_code, county_code, county_name, year, value = Value) %>% mutate(geoid = paste0(state_fips_code, county_code), county_name = tolower(county_name), measure = search_terms[i, 2])
  
  total <- rbind(total, addTo)
}

total <- total %>% select(geoid, year, measure, value) %>% mutate(measure_type = "count")
total$value <- as.numeric(gsub(",", "", total$value))


write_csv(total, xzfile("./Industry specific/Agriculture/data/distribution/va_ct_2017_industry_agriculture.csv.xz", compression = 9))


