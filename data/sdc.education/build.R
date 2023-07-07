library(community)

entities_file <- "../entities.rds"
if (file.exists(entities_file)) {
  entities <- readRDS(entities_file)
} else {
  file <- tempfile(fileext = ".csv.xz")
  download.file(paste0(
    "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/",
    "docs/distribution/geographies_metadata.csv.xz"
  ), file)
  entities <- vroom::vroom(file)
  entities <- entities[!duplicated(entities$geoid), c("geoid", "region_type")]
  saveRDS(entities, entities_file, compress = "xz")
}

# check data and measure info
check_repository(dataset = structure(entities$region_type, names = entities$geoid))

# rebuild site
## unify original files
datasets <- list.dirs(".", recursive = FALSE)
datasets <- paste0(datasets, "/data/", rep(c("distribution", "other"), each = length(datasets)))
datasets <- datasets[dir.exists(datasets)]
data_reformat_sdad(
  list.files(datasets, "\\.csv", full.names = TRUE), "docs/data",
  metadata = entities, entity_info = NULL, overwrite = TRUE
)
info <- lapply(list.files(datasets, "measure_info\\.json", full.names = TRUE), jsonlite::read_json)
agg_info <- list()
for (m in info) {
  for (e in names(m)) {
    agg_info[[e]] <- if (e %in% names(agg_info)) c(agg_info[[e]], m[[e]]) else m[[e]]
  }
}
if (length(agg_info)) {
  jsonlite::write_json(agg_info, "docs/data/measure_info.json", auto_unbox = TRUE, pretty = TRUE)
}

## add unified files
files <- paste0("docs/data/", list.files("docs/data/", "\\.csv\\.xz$"))

### make complete maps
dir.create("docs/maps", FALSE)
map_files <- list.files("docs/maps")
if (!length(map_files)) {
  if (!require(catchment)) {
    remotes::install_github("uva-bi-sdad/catchment")
    library(catchment)
  }
  ids <- unique(unlist(lapply(files, function(f) {
    unique(vroom::vroom(f, col_select = "ID", show_col_types = FALSE)[[1]])
  })))
  states <- unique(substring(ids[
    ids %in% entities$geoid[entities$region_type %in% c("county", "tract", "block group")]
  ], 1, 2))
  years <- as.numeric(unique(unlist(lapply(files, function(f) {
    unique(vroom::vroom(f, col_select = "time", show_col_types = FALSE)[[1]])
  }))))
  years <- years[years > 2012 & years < 2023]
  for (y in years) {
    for (l in c("county", "tract", "bg")) {
      f <- paste0("docs/maps/", l, "_", y, ".geojson")
      if (!file.exists(f)) {
        ms <- do.call(rbind, lapply(states, function(s) {
          download_census_shapes(
            fips = s, entity = l, name = paste0(l, y, s), year = y
          )[, "GEOID", drop = FALSE]
        }))
        sf::st_write(ms, f)
      }
    }
  }
}

data_add(
  structure(files, names = gsub("^docs/data/|\\.csv\\.xz$", "", files)),
  meta = list(
    ids = list(variable = "ID"),
    time = "time",
    variables = "docs/data/measure_info.json"
  ),
  dir = "docs/data"
)

site_build(".", serve = TRUE, aggregate = FALSE)
