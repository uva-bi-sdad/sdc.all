base_dir <- "Postsecondary/data"
dir.create(paste0(base_dir, "/original/reference_shapes"), FALSE, TRUE)

# get health district associations
districts <- read.csv(paste0(
  "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/",
  "VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv"
))
county_districts <- structure(districts$hd_geoid, names = districts$ct_geoid)

states <- c("DC", "MD", "VA")
years <- 2010:2021

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
        output = "wide",
        save = TRUE
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
      d$year <- year
      list(wide = d, tall = list(rbind(
        cbind(
          d[, c("GEOID", "year")],
          measure = "acs_postsecondary_count",
          value = d$acs_postsecondary_count,
          moe = d$acs_postsecondary_count_error
        ),
        cbind(
          d[, c("GEOID", "year")],
          measure = "acs_postsecondary_percent",
          value = d$acs_postsecondary_percent,
          moe = d$acs_postsecondary_percent_error
        )
      )))
    }
    counties <- retrieve("county")
    districts <- county_districts[substring(counties$wide$GEOID, 1, 5)]
    districts <- districts[!is.na(districts)]
    do.call(rbind, c(
      retrieve("tract")$tall,
      counties$tall,
      if (length(districts)) {
        lapply(
          list(districts),
          function(set) {
            hd_counties <- counties$wide[counties$wide$GEOID %in% names(set), ]
            hd_counties$GEOID <- set[hd_counties$GEOID]
            do.call(rbind, lapply(split(hd_counties, hd_counties$GEOID), function(e) {
              id <- e$GEOID[[1]]
              total <- sum(e$acs_postsecondary_count)
              data.frame(
                GEOID = id,
                measure = c("acs_postsecondary_count", "acs_postsecondary_percent"),
                year = year,
                value = c(total, total / sum(e$totalE) * 100),
                moe = c(
                  tidycensus::moe_sum(
                    e$acs_postsecondary_count_error, e$acs_postsecondary_count, TRUE
                  ),
                  tidycensus::moe_sum(
                    e$acs_postsecondary_percent_error, e$acs_postsecondary_percent, TRUE
                  )
                )
              )
            }))
          }
        )
      }
    ))
  }))
}))
colnames(data) <- tolower(colnames(data))

vroom::vroom_write(data, paste0(base_dir, "/distribution/acs.csv.xz"), ",")
