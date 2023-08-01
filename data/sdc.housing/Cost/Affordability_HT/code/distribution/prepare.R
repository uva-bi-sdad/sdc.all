library(readr)
library(tigris)
library(tidycensus)
library(dplyr)


data_2015 <- read_csv("~/git/vdh_hoi_indexes/data/h+t/htaindex2015_data_tracts_51.csv")

data_2019 <- read_csv("~/git/vdh_hoi_indexes/data/h+t/htaindex2019_data_tracts_51.csv")

data_2020 <- read_csv("~/git/vdh_hoi_indexes/data/h+t/htaindex2020_data_tracts_51.csv")


data_2015 <- data_2015[, c("tract", "cbsa", "ht_ami")]

data_2019 <- data_2019[, c("tract", "cbsa", "ht_ami")]

data_2020 <- data_2020[, c("tract", "cbsa", "ht_ami")]


remove_double_quotes <- function(x) {
  gsub('"', '', x)
}

data_2015[] <- lapply(data_2015, remove_double_quotes)

data_2019[] <- lapply(data_2019, remove_double_quotes)

data_2020[] <- lapply(data_2020, remove_double_quotes)



data_2015 <- data.frame(data_2015)
data_2019 <- data.frame(data_2019)
data_2020 <- data.frame(data_2020)

get_acs_subset <- function(year, state) {
  acs_data <- get_acs(geography = "tract",
                      variables = "B01003_001",  # Just a random variable, as we don't need it
                      year = year,
                      survey = "acs5",
                      state = state,
                      tidy = FALSE)
  subset_data <- acs_data[, c("GEOID", "NAME")]
  return(subset_data)
}


va_acs_2015 <- get_acs_subset(year = 2015, state = "VA")
va_acs_2019 <- get_acs_subset(year = 2019, state = "VA")
va_acs_2020 <- get_acs_subset(year = 2020, state = "VA")



va_acs_2015 <- data.frame(va_acs_2015)
va_acs_2019 <- data_frame(va_acs_2019)
va_acs_2020 <- data_frame(va_acs_2020)


# merge_and_rename <- function(data, acs_data, year) {
#   merged_data <- merge(data, acs_data, by.x = "tract", by.y = "GEOID", all.x = TRUE)
#   merged_data$cbsa <- NULL
#   colnames(merged_data)[colnames(merged_data) == "tract"] <- "geoid"
#   colnames(merged_data)[colnames(merged_data) == "year"] <- paste0("year_", year)
#   
#   merged_data$measure <- "affordability index"
#   merged_data$measure_type <- "index"
#   merged_data$year <- year
#   colnames(merged_data)[colnames(merged_data) == "NAME"] <- "region_name"
#   colnames(merged_data)[colnames(merged_data) == "ht_ami"] <- "value"
#   merged_data$region_type <- "tract"
#   
#   merged_data <- merged_data[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year")]
#   
#   return(merged_data)
# }


merge_and_rename <- function(data, acs_data, year) {
  merged_data <- merge(data, acs_data, by.x = "tract", by.y = "GEOID", all.x = TRUE)
  merged_data$cbsa <- NULL
  colnames(merged_data)[colnames(merged_data) == "tract"] <- "geoid"
  colnames(merged_data)[colnames(merged_data) == "year"] <- paste0("year_", year)
  
  colnames(merged_data)[colnames(merged_data) == "ht_ami"] <- "value"
  merged_data$measure <- "affordability_index"
  merged_data$year <- year
  
  merged_data <- merged_data[, c("geoid", "measure", "value", "year")]
  
  return(merged_data)
}

merged_data_2015 <- merge_and_rename(data_2015, va_acs_2015, 2015)


merged_data_2019 <- merge_and_rename(data_2019, va_acs_2019, 2019)

merged_data_2020 <- merge_and_rename(data_2020, va_acs_2020, 2020)


combined_data <- rbind(merged_data_2015, merged_data_2019, merged_data_2020)


#write.csv(combined_data, file = "~/git/vdh_hoi_indexes/va_cttr_2015_2019_2020_affordability_index.csv", row.names = FALSE)


####interpolation

data_2015 <- combined_data %>% filter(year == 2015)
data_2019 <- combined_data %>% filter(year == 2019)



combined_data_1519 <- bind_rows(data_2015 %>% mutate(year = 2015),
                                data_2019 %>% mutate(year = 2019))


combined_data_1519$value <- as.numeric(combined_data_1519$value)


grouped_data_1519 <- combined_data_1519 %>%
  group_by(geoid, measure)

data_2016_to_2018 <- grouped_data_1519 %>%
  complete(year = seq(2016, 2018)) %>%
  arrange(year)


data_2016_to_2018 <- data_2016_to_2018 %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup()

combined_data_2015_2020 <- rbind( data_2016_to_2018,  merged_data_2020)


write.csv(combined_data_2015_2020, file = "~/git/vdh_hoi_indexes/data/va_cttr_2015_2020_affordability_index.csv", row.names = FALSE)







