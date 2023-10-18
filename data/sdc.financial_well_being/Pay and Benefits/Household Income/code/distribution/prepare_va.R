library(data.table)
library(dplyr)

dt <- read.csv("Pay and Benefits/Household Income/data/original/dmv_cttrbg_2015_2021_median_household_income.csv.xz", colClasses = "character")
# dt_va <- dt[GEOID %like% "^51"]
dt_va <- filter(dt, GEOID %like% "^51")

setnames(dt_va, "GEOID", "geoid")
setnames(dt_va, "NAME", "region_name")
setnames(dt_va, "estimate", "value")
setnames(dt_va, "variable", "measure")
setnames(dt_va, "year", "year")

# dt_va_final <- dt_va[,.(geoid, region_type, region_name, measure, value, moe, year, measure_type)]
dt_va_final <- dt_va[, c('geoid', 'region_type', 'region_name', 'measure', 'value', 'moe', 'year', 'measure_type')]

readr::write_csv(dt_va_final,
                 xzfile("Pay and Benefits/Household Income/data/distribution/va_cttrbg_2015_2019_median_household_income.csv.xz"),
                 append = FALSE)
