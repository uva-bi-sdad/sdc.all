library(httr)
library(parallel)
library(catchment)
library(sf)
library(osrm)

dir <- "Daycare/data"
year <- 2021
states <- c("DC", "DE", "KY", "MD", "NC", "TN", "VA", "WV")

# get health district associations
va_id_map <- jsonlite::read_json("https://uva-bi-sdad.github.io/community/dist/shapes/VA/entity_info.json")
county_districts <- unlist(lapply(va_id_map$county, "[[", "district"))

# download data
locations_file <- paste0(dir, "/working/locations_", year, ".csv")
if (file.exists(locations_file)) {
  locations <- read.csv(locations_file)
} else {
  # retrieve locations
  req <- POST(
    "https://www.dss.virginia.gov/facility/search/cc.cgi",
    body = list(
      rm = "Search", search_keywords_name = "", search_exact_fips = "", search_contains_zip = ""
    )
  )

  locations <- do.call(rbind, lapply(
    grep(
      "/facility/search/cc2.cgi",
      strsplit(content(req, "text"), "<tr>", fixed = TRUE)[[1]][-1],
      fixed = TRUE, value = TRUE
    ),
    function(r) {
      data.frame(lapply(list(
        id = regmatches(r, regexec(";ID=([^;]+);", r))[[1]],
        name = regmatches(r, regexec(">([^<]+)</a>", r))[[1]],
        quality = regmatches(r, regexec("level_(\\d+)\\.svg", r))[[1]],
        address = gsub("^\\s+|[\n\t]+", "", gsub(
          "[\n\t]*<br>[\n\t]*", ", ",
          regmatches(r, regexec(">\n\t+([^<]+<br>[^<]+)</td>", r))[[1]]
        )),
        phone = regmatches(r, regexec("(\\(\\d+\\) [0-9-]+)", r))[[1]]
      ), function(e) if (length(e)) e[2] else NA))
    }
  ))
  locations$uid <- paste0(locations$id, "_", vapply(locations$address, digest::digest, ""))
  locations <- locations[!duplicated(locations$uid), ]

  # retrieve more info and geocode
  cl <- makeCluster(detectCores() - 2)
  cache <- paste0(dir, "/original/search_cache/")
  clusterExport(cl, "cache")
  dir.create(cache, FALSE)
  addresses_id <- paste0(locations$id, ":ID:", locations$address)
  data <- as.data.frame(do.call(rbind, parLapply(cl, addresses_id, function(a, geocode = F) {
    a <- strsplit(a, ":ID:", fixed = TRUE)[[1]]
    id <- a[1]
    address <- a[2]
    file <- paste0(cache, id, "_", digest::digest(address), ".rds")
    if (!file.exists(file)) {
      info <- httr::GET(paste0("https://www.dss.virginia.gov/facility/search/cc2.cgi?rm=Details;ID=", id))
      if (info$status_code == 200) saveRDS(info$content, file, compress = FALSE)
    }
    tables <- if (file.exists(file)) {
      strsplit(rawToChar(readRDS(file)), "<table")[[1]]
    } else {
      ""
    }
    data_table <- strsplit(tables[grep("Facility Type", tables, fixed = TRUE)[1]], "[\r\n\t]+")[[1]]
    data <- data.frame(list(
      type = data_table[grep("Facility Type", data_table, fixed = TRUE)[1] + 7],
      licence = strsplit(data_table[grep("License Type", data_table, fixed = TRUE)[1] + 5], "<u>|</u>")[[1]][2],
      expiration = data_table[grep("Expiration Date", data_table, fixed = TRUE)[1] + 3],
      administrator = data_table[grep("Administrator", data_table, fixed = TRUE)[1] + 3],
      capacity = as.numeric(data_table[grep("Capacity", data_table, fixed = TRUE)[1] + 3]),
      ages = sub("(?:NA ?|<.*)+", "", paste(
        data_table[grep("Ages:", data_table, fixed = TRUE)[1] + (3:6)],
        collapse = " "
      )),
      inspector = sub("(?:NA ?|<.*)+", "", paste(
        data_table[grep("Inspector:", data_table, fixed = TRUE)[1] + (3:4)],
        collapse = " "
      )),
      subsidiary = data_table[grep("Current Subsidy Provider", data_table, fixed = TRUE)[1] + 3],
      facility_id = data_table[grep("License/Facility ID#", data_table, fixed = TRUE)[1] + 3]
    ))
    inspection_table <- strsplit(strsplit(
      tables[grep("Inspection Date", tables, fixed = TRUE)[1]], "table>",
      fixed = TRUE
    )[[1]][1], "<tr")[[1]]
    inspection_table <- do.call(rbind, lapply(
      strsplit(gsub("[\r\n\t]+|</(?:a|td)", "", inspection_table[
        grep("/facility/search/cc2.cgi?rm=Inspection", inspection_table, fixed = TRUE)
      ]), ">", fixed = TRUE),
      function(r) {
        ids <- which(r %in% c("Yes", "No"))
        if (length(ids) == 4) ids <- ids[-2]
        data.frame(
          date = sub("(?:<|and).*$", "", r[grep(", 20", r, fixed = TRUE)[1]]),
          shsi = r[ids[1]],
          compliant_related = r[ids[2]],
          violations = r[ids[3]]
        )
      }
    ))
    inspections <- if (is.null(inspection_table)) {
      data.frame(
        inspections = NA,
        inspection_first = NA,
        inspection_last = NA,
        inspection_shsi = NA,
        inspection_complicant = NA,
        inspection_violation = NA
      )
    } else {
      data.frame(
        inspections = nrow(inspection_table),
        inspection_first = inspection_table[nrow(inspection_table), 1],
        inspection_last = inspection_table[1, 1],
        inspection_shsi = sum(!is.na(inspection_table$shsi) & inspection_table$shsi == "Yes"),
        inspection_complicant = sum(
          !is.na(inspection_table$compliant_related) & inspection_table$compliant_related == "Yes"
        ),
        inspection_violation = sum(
          !is.na(inspection_table$violations) & inspection_table$violations == "Yes"
        )
      )
    }

    if (geocode) {
      coords <- tidygeocoder::geo(a[2], progress_bar = FALSE, quiet = TRUE, method = "arcgis")
      if (is.na(coords$long)) coords <- tidygeocoder::geo(a[2], progress_bar = FALSE, quiet = TRUE)
    } else {
      coords <- data.frame(address = address, lat = NA, long = NA)
    }
    cbind(coords, data, inspections)
  })))
  stopCluster(cl)

  locations <- cbind(locations, data[, -1])

  ## parse ages
  locations$age_min <- as.integer(
    sub(" year.*$", "", sub("\\s*\\d+ months?|Birth", "", sub(" -.*$", "", locations$ages)))
  )
  locations$age_min[is.na(locations$age_min)] <- 0
  locations$age_max <- as.integer(
    sub(" year.*$", "", sub("\\s*\\d+ months?", "", sub("^[^-]+- ", "", locations$ages)))
  )
  locations$age_max[is.na(locations$age_max)] <- 12

  write.csv(locations, locations_file, row.names = FALSE)

  points_file <- paste0(dir, "/distribution/points_", year, ".geojson")
  unlink(points_file)
  write_sf(st_as_sf(locations[, c(
    "id", "name", "quality", "type", "age_min", "age_max", "licence", "expiration",
    "capacity", "ages", "facility_id", "inspections", "inspection_violation", "long", "lat"
  )], coords = c("long", "lat")), points_file)
}
rownames(locations) <- locations$uid

## fill in location assumptions
locations$capacity[is.na(locations$capacity)] <- 4
locations$ages[locations$ages == ""] <- "Birth - 12 years 11 months"


## drop unlocated locations
locations <- locations[!is.na(locations$lat), ]

## make location IDs
locations$lid <- vapply(paste0(round(locations$long, 6), round(locations$lat, 6)), digest::digest, "")

# get population data
pop_file <- paste0(dir, "/original/population", year)
dir.create(pop_file, FALSE)

vars <- paste0("SEX.BY.AGE_", rep(c("Female", "Male"), 3), rep(c("_Under.5", "_5.to.9", "_10.to.14"), each = 2))
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
  shapes <- shapes[IDs, ]
  total <- rowSums(pop[IDs, vars], na.rm = TRUE)
  over_4 <- rowSums(pop[IDs, vars[-(1:2)]], na.rm = TRUE)
  under_10 <- rowSums(pop[IDs, vars[1:4]], na.rm = TRUE)
  res <- data.frame(
    GEOID = IDs,
    state = s,
    population_under_15 = total,
    population_under_15_error = total + sqrt(rowSums((err[IDs, vars] / 1.645)^2, na.rm = TRUE)) * 1.645,
    population_over_4 = over_4,
    population_over_4_error = over_4 + sqrt(rowSums((err[IDs, vars[-(1:2)]] / 1.645)^2, na.rm = TRUE)) * 1.645,
    population_under_10 = under_10,
    population_under_10_error = under_10 + sqrt(rowSums((err[IDs, vars[1:4]] / 1.645)^2, na.rm = TRUE)) * 1.645,
    st_coordinates(st_centroid(shapes))
  )
  st_geometry(res) <- st_geometry(shapes)
  rownames(res) <- IDs
  if (anyNA(res$X)) res <- res[!is.na(res$X), ]
  res
}))

# calculate travel times
cost_files <- paste0(dir, "/working/traveltimes_", unique(population$state), "_", year, ".csv.xz")
options(osrm.server = Sys.getenv("OSRM_SERVER"))
unique_locations <- locations[!duplicated(locations$lid), c("lid", "long", "lat")]
rownames(unique_locations) <- unique_locations$lid
unique_locations <- unique_locations[, -1]
traveltimes <- do.call(rbind, lapply(cost_files, function(f) {
  if (file.exists(f)) {
    read.csv(gzfile(f), row.names = 1, check.names = FALSE)
  } else {
    s <- regmatches(f, regexec("_(\\w{2})_", f))[[1]][[2]]
    p <- osrmTable(
      src = population[population$state == s, c("X", "Y"), drop = TRUE],
      dst = unique_locations
    )$duration
    if (is.null(p)) stop("failed to calculate travel times")
    write.csv(
      cbind(GEOID = rownames(p), as.data.frame(as.matrix(p))),
      xzfile(f),
      row.names = FALSE
    )
    p
  }
}))
traveltimes <- traveltimes[population$GEOID, ]

#
# calculate outputs
#

# get minimum travel times
population$daycare_min_drivetime <- apply(traveltimes, 1, min, na.rm = TRUE)
population$daycare_min_drivetime_error <- 0

# calculate capacity per region
population$daycare_capacity <- vapply(st_intersects(
  population, st_as_sf(locations, coords = c("long", "lat"), crs = st_crs(population))
), function(ids) if (length(ids)) sum(locations$capacity[ids]) else 0, 0)
population$daycare_capacity_error <- 0

# calculate catchment ratios
population$daycare_per_1k <- catchment_ratio(
  population, locations[locations$age_min < 5 & locations$age_max > 9, ], traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e3,
  consumers_value = "population_under_15", providers_id = "lid",
  providers_value = "capacity", providers_location = c("long", "lat"), verbose = TRUE
)
population$daycare_per_1k_error <- abs(catchment_ratio(
  population, locations[locations$age_min < 5 & locations$age_max > 9, ], traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e3,
  consumers_value = "population_under_15_error", providers_id = "lid",
  providers_value = "capacity", providers_location = c("long", "lat")
) - population$daycare_per_1k)

## over 4
population$daycare_over_4_per_1k <- catchment_ratio(
  population, locations[locations$age_min > 4, ], traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e3,
  consumers_value = "population_over_4", providers_id = "lid",
  providers_value = "capacity", providers_location = c("long", "lat")
)
population$daycare_over_4_per_1k_error <- abs(catchment_ratio(
  population, locations[locations$age_min > 4, ], traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e3,
  consumers_value = "population_over_4_error", providers_id = "lid",
  providers_value = "capacity", providers_location = c("long", "lat")
) - population$daycare_over_4_per_1k)

## under 10
population$daycare_under_10_per_1k <- catchment_ratio(
  population, locations[locations$age_max < 10, ], traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e3,
  consumers_value = "population_under_10", providers_id = "lid",
  providers_value = "capacity", providers_location = c("long", "lat")
)
population$daycare_under_10_per_1k_error <- abs(catchment_ratio(
  population, locations[locations$age_max < 10, ], traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e3,
  consumers_value = "population_under_10_error", providers_id = "lid",
  providers_value = "capacity", providers_location = c("long", "lat")
) - population$daycare_under_10_per_1k)

block_groups <- population[
  substring(population$GEOID, 1, 5) %in% names(county_districts),
  c("GEOID", grep("^(?:population|daycare)", colnames(population), value = TRUE)),
  drop = TRUE
]

# aggregate and reformat
agger <- function(d, part = NULL) {
  ratios <- grep("_per_", colnames(d), fixed = TRUE, value = TRUE)
  ratios <- ratios[!grepl("_error", ratios, fixed = TRUE)]
  as.data.frame(c(
    GEOID = if (missing(part)) county_districts[[substring(d[1, "GEOID"], 1, 5)]] else substring(d[1, "GEOID"], 1, part),
    as.list(c(
      daycare_min_drivetime = mean(d$daycare_min_drivetime, na.rm = TRUE),
      daycare_min_drivetime_error = mean(d$daycare_min_drivetime_error, na.rm = TRUE),
      daycare_capacity = sum(d$daycare_capacity, na.rm = TRUE),
      daycare_capacity_error = sum(d$daycare_capacity_error, na.rm = TRUE),
      unlist(lapply(c("", "over_4", "under_10"), function(s) {
        pop <- paste0("population_", if (s == "") "under_15" else s)
        vs <- paste0("daycare_", if (s != "") paste0(s, "_"), "per_1k", c("", "_error"))
        totals <- c(sum(d[[pop]], na.rm = TRUE), sum(d[[paste0(pop, "_error")]], na.rm = TRUE))
        totals[totals == 0] <- 1
        ragg <- sum(d[[vs[1]]] * d[[pop]], na.rm = TRUE) / totals[1]
        structure(c(
          ragg, abs(sum((d[[vs[2]]] + d[[vs[1]]]) * d[[paste0(pop, "_error")]], na.rm = TRUE) / totals[2] - ragg)
        ), names = vs)
      }))
    ))
  ))
}
d <- list(
  block_groups = block_groups,
  tracts = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 11)), agger, 11)),
  counties = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 5)), agger, 5)),
  districts = do.call(rbind, lapply(split(block_groups, county_districts[substring(block_groups$GEOID, 1, 5)]), agger))
)
d$block_groups <- d$block_groups[, colnames(d$districts)]
d <- do.call(rbind, d)
varnames <- colnames(d)[-1]
varnames <- varnames[!grepl("_error", varnames, fixed = TRUE)]
varerrors <- paste0(varnames, "_error")
final <- do.call(rbind, lapply(split(d, seq_len(nrow(d))), function(r) {
  data.frame(
    geoid = r$GEOID,
    year = year,
    measure = varnames,
    value = as.numeric(r[varnames]),
    moe = as.numeric(r[varerrors])
  )
}))

write.csv(final, xzfile(paste0(dir, "/distribution/vdss.csv.xz")), row.names = FALSE)
