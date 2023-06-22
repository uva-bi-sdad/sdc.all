library(parallel)
library(jsonlite)
library(sf)

base_dir <- "Postsecondary/data"
dir.create("docs", FALSE)

# define health districts and focal counties
districts <- read.csv(paste0(
  "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/",
  "VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv"
))
county_districts <- structure(districts$hd_geoid, names = districts$ct_geoid)
include_counties <- c(names(county_districts), "11001", "24017", "24021", "24031", "24033")

# make a function to process each year
process_year <- function(year, dir = base_dir, districts = county_districts,
                         states = c("DC", "DE", "KY", "MD", "NC", "NJ", "PA", "TN", "VA", "WV"),
                         programs = c(
                           biomedical = "^26",
                           computer = "^11",
                           engineering = "^1[45]",
                           physical = "^40",
                           science = "^41"
                         )) {
  library(catchment)
  library(sf)
  library(osrm)
  message(year, " starting")
  results_file <- paste0(dir, "/working/nces_", year, ".csv.xz")
  block_groups <- if (file.exists(results_file)) {
    message(year, " loading existing file")
    read.csv(gzfile(results_file), check.names = FALSE)
  } else {
    #
    # get and prepare data
    #
    # get population data
    pop_file <- paste0(dir, "/original/population", year)
    dir.create(pop_file, FALSE)
    message(year, " preparing population data")
    population <- do.call(rbind, lapply(states, function(s) {
      pop <- download_census_population(pop_file, s, year, include_margins = TRUE)
      err <- pop$margins
      err$GEOID <- as.character(err$GEOID)
      rownames(err) <- err$GEOID
      pop <- pop$estimates
      pop$GEOID <- as.character(pop$GEOID)
      rownames(pop) <- pop$GEOID
      shapes <- download_census_shapes(paste0(dir, "/original/shapes"), s, "bg", year = if (year < 2014) 2014 else year)
      rownames(shapes) <- shapes$GEOID
      IDs <- union(pop$GEOID, shapes$GEOID)
      vars <- grep("SEX\\.BY\\.AGE_[FemM]+ale_(?:15|[2-8][05])", colnames(pop))
      total <- rowSums(pop[IDs, vars])
      res <- data.frame(
        GEOID = IDs,
        state = s,
        population_over_14 = total,
        population_over_14_error = total + sqrt(rowSums((err[IDs, vars] / 1.645)^2, na.rm = TRUE)) * 1.645,
        st_coordinates(st_centroid(shapes[IDs, ]))
      )
      rownames(res) <- IDs
      if (anyNA(res$X)) res <- res[!is.na(res$X), ]
      res
    }))
    # download and/or load data
    # https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx
    files <- c(
      institutions = paste0("https://nces.ed.gov/ipeds/datacenter/data/HD", year, ".zip"),
      completion = paste0("https://nces.ed.gov/ipeds/datacenter/data/C", year, "_A.zip")
    )
    message(year, " preparing school data")
    data <- lapply(files, function(f) {
      df <- paste0(dir, "/original/", basename(f))
      if (!file.exists(df)) download.file(f, df)
      read.csv(unz(df, sub("zip$", "csv", tolower(basename(f)))))
    })
    ## select schools
    data$completion$program <- do.call(paste0, as.data.frame(vapply(
      names(programs), function(code) {
        c("FALSE" = "", "TRUE" = code)[as.character(grepl(programs[[code]], data$completion$CIPCODE))]
      },
      character(nrow(data$completion))
    )))
    data$completion <- data$completion[data$completion$CTOTALT != 0, ]
    school_ids <- data$institutions[
      data$institutions$STABBR %in% states & # is in one of the selected states
        data$institutions$CYACTIVE == 1 & # is active
        data$institutions$ICLEVEL %in% 2:3 & # is a 2- or <2-year school
        data$institutions$UNITID %in% data$completion$UNITID, # has completion information
      "UNITID"
    ]
    data <- lapply(data, function(d) d[d$UNITID %in% school_ids, ])
    data$by_program <- lapply(
      programs, function(p) unique(data$completion[grepl(p, data$completion$CIPCODE), "UNITID"])
    )
    providers <- data$institutions[, c("UNITID", "LONGITUD", "LATITUDE", "ICLEVEL")]
    for (p in names(data$by_program)) {
      providers[, p] <- as.integer(providers$ICLEVEL == 2 & providers$UNITID %in% data$by_program[[p]])
    }
    providers$UNITID <- as.character(providers$UNITID)
    providers$LONGITUD <- as.numeric(providers$LONGITUD)
    providers$LATITUDE <- as.numeric(providers$LATITUDE)
    # write location files
    out <- paste0(dir, "/distribution/points_", year, ".geojson")
    unlink(out)
    write_sf(st_as_sf(data.frame(
      providers[, -(2:3)],
      X = round(providers$LONGITUD, 6),
      Y = round(providers$LATITUDE, 6)
    ), coords = c("X", "Y")), out)
    colnames(providers)[1:4] <- c("GEOID", "X", "Y", "type")
    rownames(providers) <- providers$GEOID
    # calculate travel times
    cost_file <- paste0(dir, "/working/traveltimes_", year, ".csv.xz")
    if (file.exists(cost_file)) {
      message(year, " loading existing travel times")
      traveltimes <- read.csv(gzfile(cost_file), row.names = 1, check.names = FALSE)
    } else {
      message(year, " requesting travel times")
      options(osrm.server = Sys.getenv("OSRM_SERVER"))
      traveltimes <- osrmTable(
        src = population[, c("X", "Y")],
        dst = providers[, c("X", "Y")]
      )$duration
      if (is.null(traveltimes)) stop("failed to calculate travel times")
      write.csv(
        cbind(GEOID = rownames(traveltimes), as.data.frame(as.matrix(traveltimes))),
        xzfile(cost_file),
        row.names = FALSE
      )
    }
    traveltimes <- traveltimes[population$GEOID, ]
    #
    # calculate outputs
    #
    message(year, " calculating measures")
    # get minimum travel times
    population$schools_2year_min_drivetime <- apply(
      traveltimes[, providers[providers$type == 2, "GEOID"]], 1, min,
      na.rm = TRUE
    )
    population$schools_2year_min_drivetime_error <- 0
    population$schools_under2year_min_drivetime <- apply(
      traveltimes[, providers[providers$type == 3, "GEOID"]], 1, min,
      na.rm = TRUE
    )
    population$schools_under2year_min_drivetime_error <- 0
    # calculate catchment ratios
    population$schools_2year_per_100k <- catchment_ratio(
      population, providers[providers$type == 2, ], traveltimes,
      weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
      consumers_value = "population_over_14"
    )
    population$schools_under2year_per_100k <- catchment_ratio(
      population, providers[providers$type == 3, ], traveltimes,
      weight = "gaussian", scale = 18,
      normalize_weight = TRUE, return_type = 1e5,
      consumers_value = "population_over_14"
    )
    for (p in names(data$by_program)) {
      population[[paste0("schools_2year_with_", p, "_program_per_100k")]] <- catchment_ratio(
        population, providers[providers[[p]] == 1, ],
        traveltimes,
        weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
        consumers_value = "population_over_14"
      )
    }
    population$schools_2year_per_100k_error <- abs(catchment_ratio(
      population, providers[providers$type == 2, ], traveltimes,
      weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
      consumers_value = "population_over_14_error"
    ) - population$schools_2year_per_100k)
    population$schools_under2year_per_100k_error <- abs(catchment_ratio(
      population, providers[providers$type == 3, ], traveltimes,
      weight = "gaussian", scale = 18,
      normalize_weight = TRUE, return_type = 1e5,
      consumers_value = "population_over_14_error"
    ) - population$schools_under2year_per_100k)
    for (p in names(data$by_program)) {
      population[[paste0("schools_2year_with_", p, "_program_per_100k_error")]] <- abs(catchment_ratio(
        population, providers[providers[[p]] == 1, ],
        traveltimes,
        weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
        consumers_value = "population_over_14_error"
      ) - population[[paste0("schools_2year_with_", p, "_program_per_100k")]])
    }
    population$year <- year
    ## save each year for reruns
    write.csv(population, xzfile(results_file), row.names = FALSE)
    population
  }
  block_groups <- block_groups[substring(block_groups$GEOID, 1, 5) %in% include_counties, ]
  # aggregate
  agger <- function(d, part = NULL) {
    drivetimes <- grep("drivetime", colnames(d), fixed = TRUE)
    ratios <- grep("_per_", colnames(d), fixed = TRUE, value = TRUE)
    ratios <- ratios[!grepl("_error", ratios, fixed = TRUE)]
    ratio_errors <- paste0(ratios, "_error")
    total <- sum(d$population_over_14, na.rm = TRUE)
    total[total == 0] <- 1
    totalM <- sum(d$population_over_14_error, na.rm = TRUE)
    totalM[totalM == 0] <- 1
    ragg <- colSums(d[, ratios] * d$population_over_14, na.rm = TRUE) / total
    res <- as.data.frame(c(
      GEOID = if (missing(part)) districts[[substring(d[1, "GEOID"], 1, 5)]] else substring(d[1, "GEOID"], 1, part),
      as.list(c(
        colMeans(d[, drivetimes], na.rm = TRUE), ragg,
        abs(colSums((d[, ratio_errors] + d[, ratios]) * d$population_over_14_error, na.rm = TRUE) / totalM - ragg)
      ))
    ))
    res
  }
  message(year, " creating aggregates")
  dists <- districts[substring(block_groups$GEOID, 1, 5)]
  su <- !is.na(dists)
  browser()
  list(
    block_groups = block_groups,
    tracts = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 11)), agger, 11)),
    counties = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 5)), agger, 5)),
    districts = do.call(rbind, lapply(split(block_groups[su, ], dists[su]), agger))
  )
}

# run for each year
## may need to run sequentially when first calculating travel times
cl <- makeCluster(min(2021 - 2013, detectCores() - 1))
on.exit(stopCluster(cl))
clusterExport(cl, c("base_dir", "county_districts", "include_counties"))
data <- parLapply(cl, 2013:2021, process_year)
stopCluster(cl)

# reformat and save
final <- do.call(rbind, lapply(data, function(d) {
  year <- d$block_groups[1, "year"]
  d$block_groups <- d$block_groups[, colnames(d$districts)]
  d <- do.call(rbind, d)
  varnames <- colnames(d)[-1]
  varnames <- varnames[!grepl("_error", varnames, fixed = TRUE)]
  varerrors <- paste0(varnames, "_error")
  d$year <- year
  do.call(rbind, lapply(split(d, seq_len(nrow(d))), function(r) {
    data.frame(
      geoid = r$GEOID,
      year = year,
      measure = varnames,
      value = as.numeric(r[varnames]),
      moe = as.numeric(r[varerrors])
    )
  }))
}))

write.csv(final, xzfile(paste0(base_dir, "/distribution/nces.csv.xz")), row.names = FALSE)
