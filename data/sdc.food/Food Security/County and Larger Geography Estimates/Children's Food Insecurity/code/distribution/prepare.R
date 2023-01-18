# Food Insecurity -- Data from Feeding America, Map the Meal Gap
# Gets data for overall food insecurity, children's food insecurity, average meal cost, 
#   and food budget shortfall.
# Filtered to only include children's food insecurity data.

 
# load packages
  
library(readr)
library(dplyr)


# Data Ingestion ------------------------------- 

data_path = "Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Overall\ Food\ Insecurity/data/distribution/"

va_hdct_data <- read_csv(xzfile(paste0(data_path, "va_hdct_fa_2014_2019_food_security.csv.xz")))
ncr_ct_data <- read_csv(xzfile(paste0(data_path, "ncr_ct_fa_2014_2019_food_security.csv.xz")))


# Filter to include only children's measures --------------------------------

va_child_data <- va_hdct_data %>%
  filter(measure %in% c("Child_Food_Insecurity_Rate", "Num_Child_Food_Insecure"))

ncr_child_data <- ncr_ct_data %>%
  filter(measure %in% c("Child_Food_Insecurity_Rate", "Num_Child_Food_Insecure"))


# Write ------------------------------

write_csv(va_child_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Children\'s\ Food\ Insecurity/data/distribution/va_hdct_fa_2014_2019_childrens_food_insecurity.csv.xz", compression = 9))

write_csv(ncr_child_data, xzfile("Food\ and\ Nutrition\ Assistance/Food\ Security/Population\ Measures/Children\'s\ Food\ Insecurity/data/distribution/ncr_ct_fa_2014_2019_childrens_food_insecurity.csv.xz", compression = 9))


