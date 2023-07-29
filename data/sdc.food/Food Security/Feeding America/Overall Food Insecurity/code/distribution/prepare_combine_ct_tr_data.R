# Combine county and tract level overall food security estimates
# Data source - Feeding America, Map the Meal Gap study

library(readr)

#
# NCR data -----------------------
#

data_path <- "Food\ Security/Overall\ Food\ Insecurity/data/working/"

df_ncr_tr <- read_csv(paste0(data_path, "ncr_tr_fa_2020_food_insecurity.csv.xz"))
df_ncr_ct <- read_csv(paste0(data_path, "ncr_ct_fa_2014_2019_overall_food_insecurity.csv.xz"))

# combine into one dataset

df_ncr <- rbind(df_ncr_ct, df_ncr_tr)

# write

write_csv(df_ncr, xzfile("Food\ Security/Overall\ Food\ Insecurity/data/distribution/ncr_cttr_fa_2014_2020_overall_food_insecurity.csv.xz", compression = 9))



#
# VA data ------------------------
#

df_va_tr <- read_csv(paste0(data_path, "va_tr_fa_2020_food_insecurity.csv.xz"))
df_va_ct <- read_csv(paste0(data_path, "va_hdct_fa_2014_2019_overall_food_insecurity.csv.xz"))

# combine into one dataset

df_va <- rbind(df_va_ct, df_va_tr)

# write

write_csv(df_va, xzfile("Food\ Security/Overall\ Food\ Insecurity/data/distribution/va_hdcttr_fa_2014_2020_overall_food_insecurity.csv.xz", compression = 9))


