base_dir <- "Postsecondary/data"
dir.create(paste0(base_dir, "/original/reference_shapes"), FALSE, TRUE)

# get health district associations
va_id_map <- jsonlite::read_json("https://uva-bi-sdad.github.io/community/dist/shapes/VA/entity_info.json")
county_districts <- c(
  unlist(lapply(va_id_map$county, "[[", "district")),
  "11001" = "11_hd_01", "24017" = "24_hd_01", "24021" = "24_hd_01",
  "24031" = "24_hd_01", "24033" = "24_hd_01"
)
missing_districts <- county_districts[!county_districts %in% names(va_id_map$district)]
names(missing_districts) <- missing_districts

states <- c("DC", "MD", "VA")
years <- 2010:2021

# download and load maps
entity_info <- c(
  lapply(va_id_map$district, function(e) list(region_name = e$name)),
  lapply(missing_districts, function(e) list(region_name = e))
)
for (location in tolower(states)) {
  for (year in c(2020, 2010)) {
    for (level in list(c("Block%20Group", "census_block_groups"), c("Tract", "census_tracts"), c("County", "counties"))) {
      name <- paste0(location, "_geo_census_cb_", year, "_", level[[2]])
      file <- paste0(
        base_dir, "/original/reference_shapes/", location, "_",
        sub("census_", "", level[[2]], fixed = TRUE), "_", year, ".geojson"
      )
      if (!file.exists(file)) {
        tryCatch(download.file(paste0(
          "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/",
          toupper(location), "/Census%20Geographies/", level[[1]], "/", year,
          "/data/distribution/", name, ".geojson"
        ), file), error = function(e) NULL)
      }
      if (file.exists(file)) {
        d <- jsonlite::read_json(file)
        for (e in d$features) entity_info[[e$properties$geoid]] <- e$properties
      }
    }
  }
}
entity_names <- unlist(lapply(entity_info, "[[", "region_name"))
entity_names <- entity_names[!grepl(", NA", entity_names, fixed = TRUE)]

# download and aggregate ACS data
## for margin or error:
## https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2021_ACS_Accuracy_Document_Worked_Examples.pdf
data <- do.call(rbind, lapply(states, function(state) {
  do.call(rbind, lapply(years, function(year) {
    retrieve <- function(layer) {
      d <- tidycensus::get_acs(
        layer,
        variables = c(
          total = "B06009_001",
          in_college = "B06009_004",
          college_complete = "B06009_005",
          post_college = "B06009_006"
        ),
        year = year,
        state = state,
        output = "wide"
      )
      d$acs_postsecondary_count <- rowSums(
        d[, c("in_collegeE", "college_completeE", "post_collegeE")],
        na.rm = TRUE
      )
      d$acs_postsecondary_count_error <- sqrt(rowSums(
        (d[, c("in_collegeM", "college_completeM", "post_collegeM")] / 1.645)^2,
        na.rm = TRUE
      )) * 1.645
      d$acs_postsecondary_percent <- d$acs_postsecondary_count / d$totalE * 100
      d$acs_postsecondary_percent[!is.finite(d$acs_postsecondary_percent)] <- 0
      d$acs_postsecondary_percent_error <- 1 / d$totalE * (
        (d$acs_postsecondary_count_error / 1.645)^2 -
          (d$acs_postsecondary_percent / 100)^2 * (d$totalM / 1.645)^2
      )^.5 * 100 * 1.645
      d$acs_postsecondary_percent_error[!is.finite(d$acs_postsecondary_percent_error)] <- 0
      d <- d[d$GEOID %in% names(entity_names), ]
      d$region_type <- layer
      d$region_name <- entity_names[d$GEOID]
      d$year <- year
      list(wide = d, tall = list(rbind(
        cbind(
          d[, c("GEOID", "region_type", "region_name", "year")],
          measure = "acs_postsecondary_count",
          value = d$acs_postsecondary_count,
          moe = d$acs_postsecondary_count_error,
          measure_type = "count"
        ),
        cbind(
          d[, c("GEOID", "region_type", "region_name", "year")],
          measure = "acs_postsecondary_percent",
          value = d$acs_postsecondary_percent,
          moe = d$acs_postsecondary_percent_error,
          measure_type = "percent"
        )
      )))
    }
    counties <- retrieve("county")
    do.call(rbind, c(
      retrieve("tract")$tall,
      counties$tall,
      lapply(
        list(county_districts[substring(counties$wide$GEOID, 1, 5)]),
        function(set) {
          counties$wide$GEOID <- set
          do.call(rbind, lapply(split(counties$wide, counties$wide$GEOID), function(e) {
            id <- e$GEOID[[1]]
            total <- sum(e$acs_postsecondary_count)
            type <- c("5" = "county", "8" = "health district")[[as.character(nchar(id))]]
            data.frame(
              GEOID = id,
              region_type = type,
              region_name = entity_names[[id]],
              measure = c("acs_postsecondary_count", "acs_postsecondary_percent"),
              year = year,
              value = c(total, total / sum(e$totalE) * 100),
              moe = c(
                mean(e$acs_postsecondary_count_error), mean(e$acs_postsecondary_percent_error)
              ),
              measure_type = c("count", "percent")
            )
          }))
        }
      )
    ))
  }))
}))
colnames(data) <- tolower(colnames(data))

vroom::vroom_write(data, paste0(base_dir, "/distribution/acs.csv.xz"), ",")
