library(parallel)
library(redistribute) # remotes::install_github("uva-bi-sdad/redistribute")

measures <- paste0("population", c("", "_density"))

# get health district associations
districts <- read.csv(paste0(
  "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/",
  "VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv"
))
county_districts <- structure(districts$hd_geoid, names = districts$ct_geoid)

# get other local geographies
parcels_fairfax <- readRDS("Synthetic_population/Housing_units_distribution/Fairfax/data/working/parcels_2023.rds")
parcels_arlington <- readRDS("Synthetic_population/Housing_units_distribution/Arlington/data/working/parcels_2023.rds")
parcels <- data.frame(
  GEOID = paste0(
    rep(c("f_", "a_"), c(nrow(parcels_fairfax), nrow(parcels_arlington))),
    c(parcels_fairfax$OBJECTID, parcels_arlington$OBJECTID)
  ),
  year = c(parcels_fairfax$YEAR_BUILT, parcels_arlington$Year_Built),
  units = c(parcels_fairfax$CURRE_UNIT, parcels_arlington$Total_Units)
)
sf::st_geometry(parcels) <- c(parcels_fairfax$geometry, parcels_arlington$geometry)

targets <- lapply(paste0(
  "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Local%20Geographies/",
  c(
    "Fairfax%20County/Human%20Services%20Regions/2022/data/distribution/va059_geo_ffxct_gis_2022_human_services_regions.geojson",
    "Fairfax%20County/Planning%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_planning_districts.geojson",
    "Fairfax%20County/Supervisor%20Districts/2022/data/distribution/va059_geo_ffxct_gis_2022_supervisor_districts.geojson",
    "Fairfax%20County/Zip%20Codes/2022/data/distribution/va059_geo_ffxct_gis_2022_zip_codes.geojson",
    "Arlington%20County/Civic%20Associations/2021/data/distribution/va013_geo_arl_2021_civic_associations.geojson"
  )
), function(url) sf::st_make_valid(sf::read_sf(url)[, "geoid"]))

# repeat the same process for each year
cl <- makeCluster(detectCores() - 2)
clusterExport(cl, c("county_districts", "measures", "parcels", "targets"))
data <- do.call(rbind, parLapply(cl, 2015:2021, function(year) {
  library(redistribute)
  options(tigris_use_cache = TRUE)
  retrieve <- function(geography, get_map) {
    pop <- do.call(rbind, lapply(c("DC", "MD", "VA"), function(state) {
      # get region information
      map <- get_map(state = state, cb = FALSE, year = year)
      map$ALAND[map$ALAND == 0] <- 1

      # get population
      pop <- tidycensus::get_acs(
        geography = geography,
        variables = "B01003_001",
        year = year,
        survey = "acs5",
        state = state,
        geometry = TRUE,
        cache_table = TRUE
      )
      pop$moe[is.na(pop$moe)] <- 0
      pop$sqmiles <- structure(map$ALAND / 1609.344^2, names = map$GEOID)[pop$GEOID]
      pop
    }))

    # calculating population density
    n <- nrow(pop)
    computed <- with(pop, data.frame(
      geoid = rep(GEOID, length(measures)),
      year = rep(year, length(measures)),
      measure = rep(measures, each = n),
      value = c(estimate, estimate / sqmiles),
      moe = c(moe, (estimate + moe) / sqmiles - estimate / sqmiles)
    ))
    list(pop = pop, final = computed)
  }
  block_group <- retrieve("block group", tigris::block_groups)

  # retrieve / aggregate to other layers
  base <- do.call(rbind, list(
    block_group$final,
    retrieve("tract", tigris::tracts)$final,
    retrieve("county", tigris::counties)$final,
    {
      districts <- block_group$pop
      districts$geoid <- substring(districts$GEOID, 1, 5)
      districts <- districts[districts$geoid %in% names(county_districts), ]
      districts$geoid <- county_districts[districts$geoid]
      do.call(rbind, lapply(unname(split(districts, districts$geoid)), function(e) {
        id <- e$geoid[[1]]
        total <- sum(e$estimate)
        density <- total / sum(e$sqmiles)
        n_measures <- length(measures)
        computed <- data.frame(
          geoid = rep(id, n_measures),
          year = rep(year, n_measures),
          measure = measures,
          value = c(total, density),
          moe = c(
            tidycensus::moe_sum(e$moe, e$estimate),
            sum(e$estimate + e$moe) / sum(e$sqmiles) - density
          )
        )
      }))
    }
  ))

  # make sub-county estimates
  vars <- c("GEOID", "estimate", "moe", "sqmiles")
  geo_counties <- list(c("51600", "51059"), "51013")
  parcel_estimates <- lapply(geo_counties, function(cc) {
    redistribute(
      block_group$pop[substring(block_group$pop$GEOID, 1, 5) %in% cc, vars],
      parcels[parcels$year <= year, ],
      weight = "units", default_value = 0, fill_targets = TRUE, verbose = TRUE
    )
  })
  subcounty <- do.call(rbind, lapply(targets, function(target) {
    county_su <- "51013" %in% substring(target$geoid, 1, 5) + 1
    cbind(rbind(
      redistribute(
        parcel_estimates[[county_su]], target,
        source_id = "id", target_id = "geoid",
        default_value = 0, return_geometry = FALSE, verbose = TRUE
      ),
      redistribute(
        block_group$pop[
          substring(block_group$pop$GEOID, 1, 5) %in% geo_counties[[county_su]], vars
        ], target,
        target_id = "geoid", default_value = 0, return_geometry = FALSE, verbose = TRUE
      )
    ), method = rep(c("_parcels", "_direct"), each = nrow(target)))
  }))
  subcounty$sqmiles[subcounty$sqmiles == 0] <- 1
  rbind(
    within(base, measure <- paste0(measure, "_direct")),
    within(base, measure <- paste0(measure, "_parcels")),
    with(subcounty, data.frame(
      geoid = id, year = year,
      measure = paste0("population", rep(c("", "_density"), each = nrow(subcounty)), rep(method, 2)),
      value = c(estimate, estimate / sqmiles),
      moe = c(moe, abs((estimate + moe) / sqmiles - estimate / sqmiles))
    ))
  )
}))
stopCluster(cl)

vroom::vroom_write(data, "Overall/data/distribution/census.csv.xz", ",")
