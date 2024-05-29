library(tidycensus)
library(data.table)

vars <- tidycensus::load_variables(2017, "acs5")

if (exists("va_tr_17_21")) rm(va_tr_17_21)
for (y in 2017:2021) {
  va_tr <- setDT(get_acs(geography = "tract",
                   state = "VA",
                   variables = c("B01001_001", "B27010_033", "B27010_050"),
                   year = y))
  
  va_tr$year <- y
  if(!exists("va_tr_17_21")) va_tr_17_21 <- va_tr else va_tr_17_21 <- rbindlist(list(va_tr_17_21, va_tr))
}

va_tr_17_21 <- va_tr_17_21[, .(geoid = GEOID, year, measure = variable, value = estimate, moe)]

fwrite(va_tr_17_21, "Access to Care (HOI)/data/working/va_tr_2017_2021_tot_pop_tot_insur.csv")

