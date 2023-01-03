library(tidycensus)

# get individual files
for (g in c("county", "tract", "block group")) {
  for (y in c(2015, 2016, 2017, 2018, 2019)) {
    dt <- get_acs(geography = g,
                  year = y,
                  variables = c(median_household_income = "B19013_001"),
                  state = c("VA", "DC", "MD"),
                  survey = "acs5",
                  output = "tidy",
                  geometry = FALSE)

    dt$year <- toString(y)
    dt$region_type <- g

    data.table::fwrite(dt,
                       paste0("Pay and Benefits/Household Income/data/original/dmv_", g, "_", y,"_median_household_income.csv"),
                       append = FALSE)
  }
}

# combine
file_paths <-
  list.files(
    "Pay and Benefits/Household Income/data/original/",
    pattern = "dmv_*",
    full.names = TRUE
  )

if (exists("dta")) rm("dta")
for (f in file_paths) {
  dt <- data.table::fread(f)
  if (exists("dta")) {
    dta <- data.table::rbindlist(list(dta, dt))
  } else {
    dta <- dt
  }
}

# write
data.table::fwrite(dta, "Pay and Benefits/Household Income/data/original/dmv_cttrbg_2015_2019_median_household_income.csv")
