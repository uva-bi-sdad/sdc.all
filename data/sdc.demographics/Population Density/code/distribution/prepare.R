library(parallel)

# get health district associations
districts <- read.csv(paste0(
  "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/",
  "VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv"
))
county_districts <- structure(districts$hd_geoid, names = districts$ct_geoid)

# repeat the same process for each year
cl <- makeCluster(detectCores() - 2)
clusterExport(cl, "county_districts")
data <- do.call(rbind, parLapply(cl, 2015:2021, function(year) {
  retrieve <- function(geography, map) {
    # get region information
    map <- map(state = "VA", cb = FALSE, year = year)
    map$ALAND[map$ALAND == 0] <- 1

    # get population
    pop <- tidycensus::get_acs(
      geography = geography,
      variables = "B01003_001",
      year = year,
      survey = "acs5",
      state = "VA"
    )
    pop$moe[is.na(pop$moe)] <- 0
    pop$sqmiles <- structure(map$ALAND / 1609.344^2, names = map$GEOID)[pop$GEOID]

    # calculating population density
    computed <- with(pop, data.frame(
      geoid = GEOID,
      year = year,
      measure = "population_density",
      value = estimate / sqmiles
    ))
    computed$moe <- with(pop, (estimate + moe) / sqmiles) - computed$value
    list(pop = pop, final = computed)
  }
  tracts <- retrieve("tract", tigris::tracts)

  # retrieve / aggregate to other layers
  do.call(rbind, list(
    tracts$final,
    retrieve("county", tigris::counties)$final,
    {
      districts <- tracts$pop
      districts$geoid <- substring(districts$GEOID, 1, 5)
      districts <- districts[districts$geoid %in% names(county_districts), ]
      districts$geoid <- county_districts[districts$geoid]
      do.call(rbind, lapply(unname(split(districts, districts$geoid)), function(e) {
        id <- e$geoid[[1]]
        value <- sum(e$estimate) / sum(e$sqmiles)
        data.frame(
          geoid = id,
          year = year,
          measure = "population_density",
          value = value,
          moe = sum(e$estimate + e$moe) / sum(e$sqmiles) - value
        )
      }))
    }
  ))
}))
stopCluster(cl)

vroom::vroom_write(
  data, "Population Density/data/distribution/va_hdcttr_acs_2015_2021.csv.xz", ","
)
