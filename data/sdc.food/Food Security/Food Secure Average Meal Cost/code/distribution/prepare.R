# Food Insecurity -- Data from Feeding America, Map the Meal Gap
# Gets data for overall food insecurity, children's food insecurity, average meal cost, 
#   and food budget shortfall.
# Filtered to only include food secure average meal cost data.

 
# load packages
  
library(readr)
library(dplyr)


# Data Ingestion ------------------------------- 

data_path = "Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Overall\ Food\ Insecurity/data/distribution/"

va_hdct_data <- read_csv(xzfile(paste0(data_path, "va_hdct_fa_2014_2019_food_security.csv.xz")))
ncr_ct_data <- read_csv(xzfile(paste0(data_path, "ncr_ct_fa_2014_2019_food_security.csv.xz")))


# Filter to include only average meal cost measure --------------------------------

va_cost_data <- va_hdct_data %>%
  filter(measure %in% c("Cost_Per_Meal"))

ncr_cost_data <- ncr_ct_data %>%
  filter(measure %in% c("Cost_Per_Meal"))


# Write ------------------------------

write_csv(va_cost_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Food\ Secure\ Average\ Meal\ Cost/data/distribution/va_hdct_fa_2014_2019_food_secure_avg_meal_cost.csv.xz", compression = 9))

write_csv(ncr_cost_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Food\ Secure\ Average\ Meal\ Cost/data/distribution/ncr_ct_fa_2014_2019_food_secure_avg_meal_cost.csv.xz", compression = 9))





