# load extra metadata (health district associations and rurality)
districts <- read.csv(
  "VA/State Geographies/Health Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv"
)
districts <- structure(districts$hd_geoid, names = districts$ct_geoid)
rurality <- read.csv("entities/data/original/rurality_va.csv")
rurality <- structure(rurality$type, names = rurality$ID)

# find all map files, and split into states
files <- grep(
  "distribution", list.files(".", "geojson$", recursive = TRUE),
  fixed = TRUE, value = TRUE
)
regions <- split(files, sub("/.*$", "", files))
regions <- regions[names(regions) != "NCR"]

# get features from maps, then attach extra metadata and save as JSON
for (region in names(regions)) {
  entities <- do.call(rbind, lapply(
    regions[[region]],
    function(f) sf::read_sf(f)[, c("geoid", "region_type", "region_name"), drop = TRUE]
  ))
  entities <- entities[!duplicated(entities$geoid), ]
  entities$region_name[entities$region_type %in% c("block group", "tract")] <- NA
  if (region == "VA") {
    entities <- entities[entities$region_type != "block group", ]
    entities$district <- unname(districts[substring(entities$geoid, 1, 5)])
    entities$type <- unname(rurality[entities$geoid])
    su <- is.na(entities$type)
    entities$type[su] <- unname(rurality[substring(entities$geoid[su], 1, 5)])
  }
  entities$region_type[entities$region_type == "health district"] <- "district"
  colnames(entities)[3] <- "name"
  entities$name <- sub(",.*$", "", entities$name)
  entities$region_type <- gsub(" ", "_", entities$region_type, fixed = TRUE)
  jsonlite::write_json(Filter(length, lapply(
    split(entities[, -2], entities$region_type),
    function(r) {
      Filter(length, lapply(split(r[, -1], r$geoid), function(l) {
        l <- as.list(l)
        l[!is.na(l)]
      }))
    }
  )), paste0("entities/data/distribution/", region, ".json"), auto_unbox = TRUE)
}

# combine states for National Capital Region
ncr_entities <- unlist(lapply(
  paste0("entities/data/distribution/", c("DC", "MD", "VA"), ".json"), jsonlite::read_json
), recursive = FALSE)
ncr_counties <- unlist(unname(ncr_entities[names(ncr_entities) == "county"]), recursive = FALSE)
ncr_entities <- ncr_entities[!names(ncr_entities) %in% c("county", "tract")]
ncr_entities$county <- ncr_counties
jsonlite::write_json(
  ncr_entities[names(ncr_entities) != "tract"],
  "entities/data/distribution/NCR.json",
  auto_unbox = TRUE
)
