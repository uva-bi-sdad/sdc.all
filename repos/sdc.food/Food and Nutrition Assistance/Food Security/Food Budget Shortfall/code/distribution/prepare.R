# Food Insecurity -- Data from Feeding America, Map the Meal Gap
# Gets data for overall food insecurity, children's food insecurity, average meal cost, 
#   and food budget shortfall.
# Filtered to only include food budget shortfall data.


# load packages

library(readr)
library(dplyr)


# Data Ingestion ------------------------------- 

data_path = "Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Overall\ Food\ Insecurity/data/distribution/"

va_hdct_data <- read_csv(xzfile(paste0(data_path, "va_hdct_fa_2014_2019_food_security.csv.xz")))
ncr_ct_data <- read_csv(xzfile(paste0(data_path, "ncr_ct_fa_2014_2019_food_security.csv.xz")))


# Filter to include only budget shortfall measure --------------------------------

va_shortfall_data <- va_hdct_data %>%
  filter(measure %in% c("weighted_budget_shortfall"))

ncr_shortfall_data <- ncr_ct_data %>%
  filter(measure %in% c("weighted_budget_shortfall"))


# Write ------------------------------

write_csv(va_shortfall_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Food\ Budget\ Shortfall/data/distribution/va_hdct_fa_2014_2019_food_budget_shortfall.csv.xz", compression = 9))

write_csv(ncr_shortfall_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Food\ Budget\ Shortfall/data/distribution/ncr_ct_fa_2014_2019_food_budget_shortfall.csv.xz", compression = 9))




