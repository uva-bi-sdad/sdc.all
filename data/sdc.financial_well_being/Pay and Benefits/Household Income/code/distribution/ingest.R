library(tidycensus)

# get individual files
for (g in c("county", "tract", "block group")) {
  for (y in 2015:2021) {
    dt <- get_acs(geography = g,
                  year = y,
                  variables = c(median_household_income = "B19013_001"),
                  state = c("VA", "DC", "MD"),
                  survey = "acs5",
                  output = "tidy",
                  geometry = FALSE)

    dt$year <- toString(y)
    dt$region_type <- g
    dt$measure_type <- "count"

    readr::write_csv(dt, xzfile(paste0("Pay and Benefits/Household Income/data/original/dmv_", g, "_", y,"_median_household_income.csv.xz"), compression = 9),
                     append=FALSE)
  }
}

# combine
file_paths <-
  list.files(
    "Pay and Benefits/Household Income/data/original/individual",
    pattern = "dmv_*",
    full.names = TRUE
  )

if (exists("dta")) rm("dta")
for (f in file_paths) {
  dt <- read.csv(f)

  if (exists("dta")) {
    dta <- data.table::rbindlist(list(dta, dt))
  } else {
    dta <- dt
  }
}

# write
readr::write_csv(dta, xzfile("Pay and Benefits/Household Income/data/original/dmv_cttrbg_2015_2021_median_household_income.csv.xz", compression = 9))
