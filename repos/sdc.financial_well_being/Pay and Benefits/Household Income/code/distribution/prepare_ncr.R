library(data.table)

dt <- fread("Pay and Benefits/Household Income/data/original/dmv_cttrbg_2015_2019_median_household_income.csv", colClasses = "character")

dt_ncr <- dt[substr(GEOID, 1, 5) %in% c("51013", "51059", "51107", "51510", "51600", "51153", "51683", "51685", "51610", "11001", "24031", "24033", "24017", "24021")]

setnames(dt_ncr, "GEOID", "geoid")
setnames(dt_ncr, "NAME", "region_name")
setnames(dt_ncr, "estimate", "value")
setnames(dt_ncr, "variable", "measure")
setnames(dt_ncr, "year", "year")

dt_ncr_final <- dt_ncr[,.(geoid, region_type, region_name, measure, value, moe, year)]

readr::write_csv(dt_ncr_final,
                 xzfile("Pay and Benefits/Household Income/data/distribution/ncr_cttrbg_2015_2019_median_household_income.csv.xz"),
                 append = FALSE)
