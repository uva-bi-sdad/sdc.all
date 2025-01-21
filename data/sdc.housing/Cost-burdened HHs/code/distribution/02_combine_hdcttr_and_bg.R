# combine renters block group data with hd, ct, tr, cost burdened household data

library(readr)

years <- c(2014:2023)

# NCR --------------

ncr_cttr <- read_csv(paste0("Cost-burdened HHs/data/working/ncr_cttr_acs_", min(years), "_", max(years), "_cost_burdened_hhs.csv.xz"))
ncr_bg <- read_csv(paste0("Cost-burdened HHs/data/working/ncr_bg_acs_", min(years), "_", max(years), "_cost_burdened_renters.csv.xz"))

ncr <- rbind(ncr_cttr, ncr_bg)

write_csv(ncr, xzfile(paste0("Cost-burdened HHs/data/distribution/ncr_cttrbg_acs_", min(years), "_", max(years), "_cost_burdened_hhs.csv.xz"), compression = 9))


# VA ----------------

va_hdcttr <- read_csv(paste0("Cost-burdened HHs/data/working/va_hdcttr_acs_", min(years), "_", max(years), "_cost_burdened_hhs.csv.xz"))
va_bg <- read_csv(paste0("Cost-burdened HHs/data/working/va_bg_acs_", min(years), "_", max(years), "_cost_burdened_renters.csv.xz"))

va <- rbind(va_hdcttr, va_bg)

write_csv(va, xzfile(paste0("Cost-burdened HHs/data/distribution/va_hdcttrbg_acs_", min(years), "_", max(years), "_cost_burdened_hhs.csv.xz"), compression = 9))

