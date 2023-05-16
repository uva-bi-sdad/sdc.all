library(community)
library(jsonlite)

# check data and measure info
check_repository()

# rebuild site
entities_file <- "../entities.rds"
if (file.exists(entities_file)) {
  entities <- readRDS(entities_file)
} else {
  entities <- vroom::vroom(
    "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/geographies_metadata.csv"
  )
  entities <- entities[!duplicated(entities$geoid), c("geoid", "region_name", "region_type")]
  saveRDS(entities, entities_file, compress = "xz")
}
entities <- rbind(entities, data.frame(
  geoid = c("11_hd_01", "24_hd_01"),
  region_name = c("District of Columbia", "Maryland"),
  region_type = rep("health district", 2)
))

datasets <- paste0(list.dirs("."), "/data/distribution")
datasets <- datasets[dir.exists(datasets)]
data_reformat_sdad(
  list.files(datasets, "\\.csv", full.names = TRUE), metadata = entities, "docs/data"
)
info <- lapply(list.files(datasets, "measure_info\\.json", full.names = TRUE), read_json)
agg_info <- list()
for (m in info) {
  for (e in names(m)) {
    agg_info[[e]] <- if (e %in% names(agg_info)) c(agg_info[[e]], m[[e]]) else m[[e]]
  }
}
if (length(agg_info)) {
  write_json(
    agg_info, "docs/data/measure_info.json",
    auto_unbox = TRUE, pretty = TRUE
  )
}

files <- paste0("docs/data/", list.files("docs/data/", "\\.csv\\.xz$"))
data_add(
  structure(files, names = gsub("^docs/data/|\\.csv\\.xz$", "", files)),
  meta = list(
    ids = list(variable = "ID", map = "data/entity_info.json"),
    time = "time",
    variables = "docs/data/measure_info.json"
  ),
  dir = "docs/data"
)

site_build(".", serve = TRUE, options = list(
  polygon_outline = .5, color_scale_center = "median"
))
