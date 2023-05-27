# combine renters block group data with hd, ct, tr, cost burdened household data

library(readr)


# NCR --------------

ncr_cttr <- read_csv("Cost-burdened HHs/data/working/ncr_cttr_acs_2014_2021_cost_burdened_hhs.csv.xz")
ncr_bg <- read_csv("Cost-burdened HHs/data/working/ncr_bg_acs_2014_2021_cost_burdened_renters.csv.xz")

ncr <- rbind(ncr_cttr, ncr_bg)

write_csv(ncr, xzfile("Cost-burdened HHs/data/distribution/ncr_cttrbg_acs_2014_2021_cost_burdened_hhs.csv.xz", compression = 9))


# VA ----------------

va_hdcttr <- read_csv("Cost-burdened HHs/data/working/va_hdcttr_acs_2014_2021_cost_burdened_hhs.csv.xz")
va_bg <- read_csv("Cost-burdened HHs/data/working/va_bg_acs_2014_2021_cost_burdened_renters.csv.xz")

va <- rbind(va_hdcttr, va_bg)

write_csv(va, xzfile("Cost-burdened HHs/data/distribution/va_hdcttrbg_acs_2014_2021_cost_burdened_hhs.csv.xz", compression = 9))

