library(community)
library(jsonlite)

datasets <- paste0(list.dirs("data", recursive = FALSE), "/distribution")
data_reformat_sdad(list.files(datasets, "\\.csv\\.[gbx]z2?$", full.names = TRUE), "docs/data")
info <- lapply(list.files(datasets, "measure_info.*\\.json", full.names = TRUE), read_json)
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

data_add(
  c(
    county = "county.csv.xz",
    tract = "tract.csv.xz",
    block_group = "block_group.csv.xz"
  ),
  meta = rep(list(list(
    ids = list(variable = "ID", map = "data/entity_info.json"),
    time = "time",
    variables = "docs/data/measure_info.json"
  )), 3),
  dir = "docs/data",
  clean = TRUE,
  refresh = TRUE
)

site_build(".", serve = TRUE, options = list(
  polygon_outline = .5, color_scale_center = "median"
))
