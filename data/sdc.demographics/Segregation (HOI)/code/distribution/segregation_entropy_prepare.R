library(dplyr)
library(tidycensus)
library(readxl)
library(readr)


years <- 2017:2020

df_2017_2020 <- data.frame()

for (year in years) {
  segregation_data <- get_acs(
    geography = 'tract',
    year = year,
    survey = "acs5",
    state = 'VA',
    variables = c(
      total_pop = 'DP05_0001E',
      hisp_latin = 'DP05_0071E',
      white = 'DP05_0077E',
      black = 'DP05_0078E',
      american_indian = 'DP05_0079E',
      asian = 'DP05_0080E',
      nhopi = 'DP05_0081E',
      sor = 'DP05_0082E',
      two = 'DP05_0083E'
    ),
    geometry = FALSE,
    output = 'wide'
  )
  
  segregation_data <- segregation_data %>% 
    select(-ends_with("M")) %>%
    mutate(year = year)
  
  df_2017_2020 <- bind_rows(df_2017_2020, segregation_data)
}

##########

#########



two_year <- c(2015, 2016)

df_2015_2016 <- data.frame()

for (year in two_year) {
  data <- get_acs(
    geography = 'tract',
    year = year,
    survey = "acs5",
    state = 'VA',
    variables = c(
      total_pop = 'DP05_0001E',
      hisp_latin = 'DP05_0066E',
      white = 'DP05_0072E',
      black = 'DP05_0073E',
      american_indian = 'DP05_0074E',
      asian = 'DP05_0075E',
      nhopi = 'DP05_0076E',
      sor = 'DP05_0077E',
      two = 'DP05_0078E'
    ),
    geometry = FALSE,
    output = 'wide'
  )
  
  data <- data %>% 
    select(-ends_with("M")) %>%
    mutate(year = year)
  
  df_2015_2016 <- bind_rows(df_2015_2016, data)
}

combined_df <- rbind(df_2015_2016, df_2017_2020)

df <- combined_df


#df <- read_csv("~/segregation_rex/segregation_2015_2020.csv")


##subset_segregation_df <- df[df$total_pop == 0 & df$year == 2020, ]

df_0 <-  df[df$total_pop == 0, ] 

df <- df[df$total_pop != 0, ]

df <- df %>%
  mutate(
    hisp_latin_prop = hisp_latin / total_pop,
    white_prop = white / total_pop,
    black_prop = black / total_pop,
    american_indian_prop = american_indian / total_pop,
    asian_prop = asian / total_pop,
    nhopi_prop = nhopi / total_pop,
    sor_prop = sor / total_pop,
    two_prop = two / total_pop
  )


p_c <- c("hisp_latin_prop","white_prop", "black_prop" ,  "american_indian_prop" ,"asian_prop",  "nhopi_prop", "sor_prop" ,"two_prop")




#initializing a entropy_index column with 0 initially for entropy 
df$entropy_index <- 0

#loop through each row of df
# get the proportion value of each tract and store them in vector
# keep values where p is greater than 0 since log 0 is undefined
#calculate entropy  using formula 
#store the value in h column
for (i in 1:nrow(df)) {
  p <- df[i, p_c]
  p <- p[p>0]
  entropy_index <- -sum(p * log(p))
  df$entropy_index[i] <- entropy_index
}

df$entropy_index <- round(df$entropy_index, 2)

#######

df_entropy_zero <-  df[df$entropy_index == 0, ] 


#converting to SDAD format 
df$measure <- "segregation_indicator"


segregation_df <- df %>%
  select(GEOID, measure, year, entropy_index) %>%
  rename(geoid = GEOID, value = entropy_index)



#df_max <- segregation_df[segregation_df$value == 1.68,]


#c_r <- df$hisp_latin + df$white + df$black + df$american_indian + df$asian + df$nhopi + df$sor + df$two == df$total_pop

#df$c_r <- c_r


#df_2020 <- df[df$year == 2020, ]





#census tract to county aggregation


segregation_data_tract_2020 <- segregation_df[segregation_df$year == 2020, ]
segregation_data_tract_2019 <- segregation_df[segregation_df$year == 2019, ]
segregation_data_tract_2018 <- segregation_df[segregation_df$year == 2018, ]
segregation_data_tract_2017 <- segregation_df[segregation_df$year == 2017, ]
segregation_data_tract_2016 <- segregation_df[segregation_df$year == 2016, ]
segregation_data_tract_2015 <- segregation_df[segregation_df$year == 2015, ]

# segregation_df_county_2020_aggregated <- segregation_data_tract_2020 %>%
#   mutate(geoid = substr(geoid, 1, 5)) %>%
#   group_by(geoid) %>%
#   summarize(value = sum(value)) %>%
#   select(geoid, value) %>%
#   mutate(measure = "segregation_indicator", year = 2020) %>%
#   select(geoid, measure, year, value)

library(dplyr)

aggregated_dfs <- list()

for (year in 2015:2020) {
  
  current_df <- get(paste0("segregation_data_tract_", year))
  
  aggregated_df <- current_df %>%
    mutate(geoid = substr(geoid, 1, 5)) %>%
    group_by(geoid) %>%
    summarize(value = sum(value)) %>%
    select(geoid, value) %>%
    mutate(measure = "segregation_indicator", year = year) %>%
    select(geoid, measure, year, value)
  
  aggregated_dfs[[paste0("aggregated_df_", year)]] <- aggregated_df
}

segregation_df_county_2015_2020 <- bind_rows(aggregated_dfs)



#county to health district aggregation

segregation_data_county_2020 <- segregation_df_county_2015_2020[segregation_df_county_2015_2020$year == 2020, ]
segregation_data_county_2019 <- segregation_df_county_2015_2020[segregation_df_county_2015_2020$year == 2019, ]
segregation_data_county_2018 <- segregation_df_county_2015_2020[segregation_df_county_2015_2020$year == 2018, ]
segregation_data_county_2017 <- segregation_df_county_2015_2020[segregation_df_county_2015_2020$year == 2017, ]
segregation_data_county_2016 <- segregation_df_county_2015_2020[segregation_df_county_2015_2020$year == 2016, ]
segregation_data_county_2015 <- segregation_df_county_2015_2020[segregation_df_county_2015_2020$year == 2015, ]


health_district_data_2020 <- read_csv("~/git/sdc.geographies_dev/VA/State Geographies/Health Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv")

# merged_data_2020 <- merge(segregation_data_county_2020,health_district_data_2020 , by.x = "geoid", by.y = "ct_geoid")
# 
# 
# segregation_df_hd_aggregated_2020 <- merged_data_2020 %>%
#   group_by(hd_geoid) %>%
#   summarize(value = sum(value)) %>%
#   select(geoid = hd_geoid, value) %>%
#   mutate(measure = "segregation_indicator", year = 2020) %>%
#   select(geoid, measure, year, value)



library(dplyr)

aggregated_hd_dfs <- list()

for (year in 2015:2020) {
  
  current_segregation_data <- get(paste0("segregation_data_county_", year))
  
  merged_data <- merge(current_segregation_data, health_district_data_2020, by.x = "geoid", by.y = "ct_geoid")
  
  aggregated_hd_df <- merged_data %>%
    group_by(hd_geoid) %>%
    summarize(value = sum(value)) %>%
    select(geoid = hd_geoid, value) %>%
    mutate(measure = "segregation_indicator", year = year) %>%
    select(geoid, measure, year, value)
  
  aggregated_hd_dfs[[paste0("segregation_df_hd_aggregated_", year)]] <- aggregated_hd_df
}

segregation_df_hd_aggregated_2015_2020 <- bind_rows(aggregated_hd_dfs)


segregation_df <- segregation_df %>% mutate(geoid = as.character(geoid))
segregation_df_county_2015_2020 <- segregation_df_county_2015_2020 %>% mutate(geoid = as.character(geoid))


va_tr_ct_hd_vdh_2015_2020_segregation_index <- bind_rows(
  segregation_df,
  segregation_df_county_2015_2020,
  segregation_df_hd_aggregated_2015_2020
)

readr::write_csv(va_tr_ct_hd_vdh_2015_2020_segregation_index,xzfile("~/segregation_rex/va_tr_ct_hd_vdh_2015_2020_segregation_index.csv.xz", compression = 9))


write.csv(va_tr_ct_hd_vdh_2015_2020_segregation_index,xzfile("~/segregation_rex/va_tr_ct_hd_vdh_2015_2020_segregation_index.csv.xz", compression = 9))
write.csv(va_tr_ct_hd_vdh_2015_2020_segregation_index, "~/segregation_rex/va_tr_ct_hd_vdh_2015_2020_segregation_index.csv", row.names = FALSE)