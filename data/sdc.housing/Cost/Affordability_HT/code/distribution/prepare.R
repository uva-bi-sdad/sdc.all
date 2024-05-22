#### Load the required libraries ####
library(readr)
library(readxl)
library(readr)
library(tigris)
library(tidycensus)
library(dplyr)
library(tidyr)
library(zoo)

#### Prepare tract level data for Affordability Index ####
data_2015_tr <- read_csv("Cost/Affordability_HT/data/original/htaindex2015_data_tracts_51.csv")
data_2019_tr <- read_csv("Cost/Affordability_HT/data/original/htaindex2019_data_tracts_51.csv")
data_2020_tr <- read_csv("Cost/Affordability_HT/data/original/htaindex2020_data_tracts_51.csv")


data_2015_tr <- data_2015_tr[, c("tract", "cbsa", "ht_ami")]
data_2019_tr <- data_2019_tr[, c("tract", "cbsa", "ht_ami")]
data_2020_tr <- data_2020_tr[, c("tract", "cbsa", "ht_ami")]


remove_double_quotes <- function(x) {
  gsub('"', '', x)
}

data_2015_tr[] <- lapply(data_2015_tr, remove_double_quotes)
data_2019_tr[] <- lapply(data_2019_tr, remove_double_quotes)
data_2020_tr[] <- lapply(data_2020_tr, remove_double_quotes)


data_2015_tr <- data.frame(data_2015_tr[!is.na(data_2015_tr$ht_ami),])
data_2019_tr <- data.frame(data_2019_tr[!is.na(data_2019_tr$ht_ami),])
data_2020_tr <- data.frame(data_2020_tr[!is.na(data_2020_tr$ht_ami),])



get_acs_subset_tr <- function(year, state) {
  acs_data_tr <- get_acs(geography = "tract",
                         variables = "B01003_001",  # Just a random variable, as we don't need it
                         year = year,
                         survey = "acs5",
                         state = state,
                         tidy = FALSE)
  subset_data <- acs_data_tr[, c("GEOID", "NAME")]
  return(subset_data)
}


va_acs_2015_tr <- get_acs_subset_tr(year = 2015, state = "VA")
va_acs_2019_tr <- get_acs_subset_tr(year = 2019, state = "VA")
va_acs_2020_tr <- get_acs_subset_tr(year = 2020, state = "VA")

va_acs_2015_tr <- data.frame(va_acs_2015_tr)
va_acs_2019_tr <- data_frame(va_acs_2019_tr)
va_acs_2020_tr <- data_frame(va_acs_2020_tr)




merge_and_rename_tr <- function(data, acs_data_tr, year) {
  merged_data <- merge(data, acs_data_tr, by.x = "tract", by.y = "GEOID", all.x = TRUE)
  merged_data$cbsa <- NULL
  colnames(merged_data)[colnames(merged_data) == "tract"] <- "geoid"
  colnames(merged_data)[colnames(merged_data) == "year"] <- paste0("year_", year)
  
  colnames(merged_data)[colnames(merged_data) == "ht_ami"] <- "value"
  merged_data$measure <- "affordability_index"
  merged_data$year <- year
  
  merged_data <- merged_data[, c("geoid", "measure", "value", "year")]
  
  return(merged_data)
}

merged_data_2015_tr <- merge_and_rename_tr(data_2015_tr, va_acs_2015_tr, 2015)
merged_data_2019_tr <- merge_and_rename_tr(data_2019_tr, va_acs_2019_tr, 2019)
merged_data_2020_tr <- merge_and_rename_tr(data_2020_tr, va_acs_2020_tr, 2020)


combined_data_tr <- rbind(merged_data_2015_tr, merged_data_2019_tr, merged_data_2020_tr)

#write.csv(combined_data, file = "/home/gcm8gw/Git/sdc.housing_dev/Cost/Affordability_HT/data/distribution/va_tr_2015_2019_2020_affordability_index.csv.xz", row.names = FALSE)


####interpolation

data_2015_tr <- combined_data_tr %>% filter(year == 2015)
data_2019_tr <- combined_data_tr %>% filter(year == 2019)
data_2020_tr <- combined_data_tr %>% filter(year == 2020)


combined_data_1519_tr <- bind_rows(data_2015_tr %>% mutate(year = 2015),
                                   data_2019_tr %>% mutate(year = 2019))


combined_data_1519_tr$value <- as.numeric(combined_data_1519_tr$value)

# copy 2019 values to 2015 where 2015 doesn't exist (need for interpolation)
df0 <- combined_data_1519_tr %>% group_by(geoid) %>%tally()
df1 <- df0[df0$n==1,]
df2 <- combined_data_1519_tr[combined_data_1519_tr$geoid %in% df1$geoid,]
df3 <- data.frame(geoid=df1$geoid, measure="affordability_index", value=df2$value, year=2015)
combined_data_1519_tr <- rbind(combined_data_1519_tr, df3)

grouped_data_1519_tr <- combined_data_1519_tr %>%
  group_by(geoid,measure)

data_2016_to_2018_tr <- grouped_data_1519_tr %>%
  complete(year = seq(2016, 2018)) %>%
  arrange(year)


data_2016_to_2018_tr <- data_2016_to_2018_tr %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup()

combined_data_2015_2020_tr <- rbind( data_2016_to_2018_tr,  data_2020_tr)

### 2021

# Define the year for which you want to estimate values (2021)
year_2021 <- 2021

#combined_data_1920_tr <- bind_rows(data_2019_tr %>% mutate(year = 2019),
#                                   data_2020_tr %>% mutate(year = 2020))

combined_data_1920_tr <- bind_rows(combined_data_2015_2020_tr[combined_data_2015_2020_tr$year==2019,],
                                   combined_data_2015_2020_tr[combined_data_2015_2020_tr$year==2020,])


combined_data_1920_tr$value <- as.numeric(combined_data_1920_tr$value)


# Calculate the rate of change for each tract, handling NA values
rate_of_change_tr <- combined_data_1920_tr %>%
  group_by(geoid) %>%
  summarise(rate_of_change_tr = (last(value, order_by = year) - first(value, order_by = year)) / if_else(max(year) != min(year), (max(year) - min(year, na.rm = TRUE)), max(year)))
  #summarise(rate_of_change_tr = (last(value, order_by = year) - first(value, order_by = year)) / (max(year) - min(year, na.rm = TRUE)))


sum(is.na(rate_of_change_tr))
# now 0
# 853/2455 = 0.3474542

# data_2020_tr <- data %>% filter(Year == 2020)
# Merge the rate of change back into the 2020 data
data_2020_tr <- data_2020_tr %>%
  left_join(rate_of_change_tr, by = "geoid")

data_2020_tr_comb <- combined_data_2015_2020_tr[combined_data_2015_2020_tr$year==2020,]

data_2020_tr_comb$value <- as.numeric(data_2020_tr$value)
data_2020_tr_comb <- merge(data_2020_tr_comb, rate_of_change_tr, by = "geoid", all.x = T)

# Calculate the estimated values for 2021 for each tract, handling NA values
data_2021_tr <- data_2020_tr_comb %>%
  #mutate(year = year_2021, value = ifelse(is.na(value), NA, value + (rate_of_change_tr$rate_of_change_tr * (year_2021 - max(year, na.rm = TRUE))))) %>%
  mutate(year = year_2021, value = value + rate_of_change_tr) %>%
  select(year,measure, geoid, value)


combined_data_2015_2021_tr <- rbind( combined_data_2015_2020_tr, data_2021_tr)


#write.csv(combined_data_2015_2021_tr, file = xzfile("~/Git/sdc.housing_dev/Cost/Affordability_HT/data/distribution/va_tr_2015_2021_affordability_index.csv.xz"), row.names = FALSE)

#### Prepare county level data for Affordability Index ####


data_2015_ct <- read_csv("~/Git/sdc.housing_dev/Cost/Affordability_HT/data/original/htaindex2015_data_counties_51.csv")
data_2019_ct <- read_csv("~/Git/sdc.housing_dev/Cost/Affordability_HT/data/original/htaindex2019_data_counties_51.csv")
data_2020_ct <- read_csv("~/Git/sdc.housing_dev/Cost/Affordability_HT/data/original/htaindex2020_data_counties_51.csv")

data_2015_ct <- data_2015_ct[, c("county", "cbsa", "ht_ami")]
data_2019_ct <- data_2019_ct[, c("county", "cbsa", "ht_ami")]
data_2020_ct <- data_2020_ct[, c("county", "cbsa", "ht_ami")]


remove_double_quotes <- function(x) {
  gsub('"', '', x)
}

data_2015_ct[] <- lapply(data_2015_ct, remove_double_quotes)
data_2019_ct[] <- lapply(data_2019_ct, remove_double_quotes)
data_2020_ct[] <- lapply(data_2020_ct, remove_double_quotes)

data_2015_ct <- data.frame(data_2015_ct)
data_2019_ct <- data.frame(data_2019_ct)
data_2020_ct <- data.frame(data_2020_ct)

get_acs_subset_ct <- function(year, state) {
  acs_data_ct <- get_acs(geography = "county",
                         variables = "B01003_001",  # Just a random variable, as we don't need it
                         year = year,
                         survey = "acs5",
                         state = state,
                         tidy = FALSE)
  subset_data <- acs_data_ct[, c("GEOID", "NAME")]
  return(subset_data)
}


va_acs_2015_ct <- get_acs_subset_ct(year = 2015, state = "VA")
va_acs_2019_ct <- get_acs_subset_ct(year = 2019, state = "VA")
va_acs_2020_ct <- get_acs_subset_ct(year = 2020, state = "VA")


va_acs_2015_ct <- data.frame(va_acs_2015_ct)
va_acs_2019_ct <- data_frame(va_acs_2019_ct)
va_acs_2020_ct <- data_frame(va_acs_2020_ct)

merge_and_rename_ct <- function(data, acs_data, year) {
  merged_data <- merge(data, acs_data, by.x = "county", by.y = "GEOID", all.x = TRUE)
  merged_data$cbsa <- NULL
  colnames(merged_data)[colnames(merged_data) == "county"] <- "geoid"
  colnames(merged_data)[colnames(merged_data) == "year"] <- paste0("year_", year)
  
  colnames(merged_data)[colnames(merged_data) == "ht_ami"] <- "value"
  merged_data$measure <- "affordability_index"
  merged_data$year <- year
  
  merged_data <- merged_data[, c("geoid", "measure", "value", "year")]
  
  return(merged_data)
}

merged_data_2015_ct <- merge_and_rename_ct(data_2015_ct, va_acs_2015_ct, 2015)
merged_data_2019_ct <- merge_and_rename_ct(data_2019_ct, va_acs_2019_ct, 2019)
merged_data_2020_ct <- merge_and_rename_ct(data_2020_ct, va_acs_2020_ct, 2020)

combined_data_ct <- rbind(merged_data_2015_ct, merged_data_2019_ct, merged_data_2020_ct)
#write.csv(combined_data_ct, file = "/home/gcm8gw/Git/sdc.housing_dev/Cost/Affordability_HT/data/distribution/va_ct_2015_2019_2020_affordability_index.csv.xz", row.names = FALSE)


####interpolation

data_2015_ct <- combined_data_ct %>% filter(year == 2015)
data_2019_ct <- combined_data_ct %>% filter(year == 2019)
data_2020_ct <- combined_data_ct %>% filter(year == 2020)

data_2019_ct <- combined_data_ct %>% filter(year == 2019)

combined_data_1519_ct <- bind_rows(data_2015_ct %>% mutate(year = 2015),
                                   data_2019_ct %>% mutate(year = 2019))

combined_data_1519_ct$value <- as.numeric(combined_data_1519_ct$value)


grouped_data_1519_ct <- combined_data_1519_ct %>%
  group_by(geoid, measure)

data_2016_to_2018_ct <- grouped_data_1519_ct %>%
  complete(year = seq(2016, 2018)) %>%
  arrange(year)


data_2016_to_2018_ct <- data_2016_to_2018_ct %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup()


combined_data_2015_2020_ct <- rbind( data_2016_to_2018_ct,  data_2020_ct)

### 2021


# Define the year for which you want to estimate values (2021)
year_2021 <- 2021

combined_data_1920_ct <- bind_rows(data_2019_ct %>% mutate(year = 2019),
                                   data_2020_ct %>% mutate(year = 2020))


combined_data_1920_ct$value <- as.numeric(combined_data_1920_ct$value)


# Calculate the rate of change for each tract, handling NA values
rate_of_change_ct <- combined_data_1920_ct %>%
  group_by(geoid) %>%
  summarise(rate_of_change_ct = (last(value, order_by = year) - first(value, order_by = year)) / (max(year) - min(year, na.rm = TRUE)))

sum(is.na(rate_of_change_ct))


# data_2020_tr <- data %>% filter(Year == 2020)
# Merge the rate of change back into the 2020 data
data_2020_ct <- data_2020_ct %>%
  left_join(rate_of_change_ct, by = "geoid")

data_2020_ct$value <- as.numeric(data_2020_ct$value)

# Calculate the estimated values for 2021 for each tract, handling NA values
data_2021_ct <- data_2020_ct %>%
  mutate(year = year_2021, value = ifelse(is.na(value), NA, value + (rate_of_change_ct * (year_2021 - max(year, na.rm = TRUE))))) %>%
  select(year, measure, geoid, value)

combined_data_2015_2021_ct <- rbind( combined_data_2015_2020_ct, data_2021_ct)


#write.csv(combined_data_2015_2021_ct, file = xzfile("~/Git/sdc.housing_dev/Cost/Affordability_HT/data/distribution/va_ct_2015_2021_affordability_index.csv.xz"), row.names = FALSE)




#### Aggregate to Health Districts for Affordability Index and combine datasets to get hdcttr levels in a single dataset ####

source("~/Git/sdc.housing_dev/utils/distribution/aggregate.R")

# Adding column in tract and county data so as to run the aggregate function (found in utils folder)
combined_data_2015_2021_tr$moe <- ""
combined_data_2015_2021_ct$moe <- ""

# Using aggregate function (found in utils folder) to aggregate from county to health district
# Aggregate function output includes hd and ct levels
combined_data_2015_2021_hdct <- aggregate(combined_data_2015_2021_ct, "county", weight_col = "B01003_001E")


# Combine hdct with tr level
combined_data_2015_2021_hdcttr <- rbind(combined_data_2015_2021_hdct, combined_data_2015_2021_tr)

write.csv(combined_data_2015_2021_hdcttr, file = xzfile("~/Git/sdc.housing_dev/Cost/Affordability_HT/data/distribution/va_hdcttr_2015_2021_affordability_index.csv.xz"), row.names = FALSE)


# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## get aggrgegate function for health districts
#source("https://raw.githubusercontent.com/uva-bi-sdad/sdc.education/main/utils/distribution/aggregate.R")

## convert
stnd <- standardize_all(combined_data_2015_2021_hdcttr)

# aggregate counties to health districts
#hds <- aggregate(stnd[nchar(stnd$geoid)==5,], "county")
#stnd <- rbindlist(list(stnd, hds[!nchar(hds$geoid)==5,]), use.names = T)

# save standardized file
write.csv(stnd, file = xzfile("Cost/Affordability_HT/data/distribution/va_hdcttr_2015_2021_affordability_index_std.csv.xz"), row.names = FALSE)
