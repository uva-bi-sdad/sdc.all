input_variable("shapes", list(
  "selected_variable == 3rd_grade_mean_median_read_score:mean_read_pass_rate" = "county",
  "selected_variable == 3rd_grade_mean_median_read_score:median_read_pass_rate" = "county",
  "selected_variable == health_literacy_estimates:health_literacy_estimate" = "county",
  "selected_variable == post_hs_education:num_post_hs_edu" = "tract",
  "selected_variable == post_hs_education:perc_post_hs_edu" = "tract"
), "block_group")

input_dataview(
  id = "default_view", y = "selected_variable", time_agg = "selected_time", dataset = "shapes"
)

page_section(
  wraps = "col", sizes = c(4, NA),
  page_section(
    wraps = "row",
    output_info(title = "variables.short_name"),
    input_button("Download", "export", query = list(include = "selected_variable"), class = "btn-full"),
    output_info(body = "variables.sources"),
    page_section(
      wraps = "col", sizes = c(NA, 1),
      input_select(
        "Variable",
        options = "variables", default = 5,
        id = "selected_variable"
      ),
      input_number("Time", min = "filter.time_min", max = "filter.time_max", default = "max", id = "selected_time")
    ),
    output_legend(id = "main_legend", subto = c("main_map", "main_plot")),
    output_info(
      default = c(body = "Hover over output elements for more information."),
      title = "features.name",
      subto = c("main_map", "main_plot", "main_legend")
    ),
    output_info(
      body = c(
        "variables.long_name" = "selected_variable",
        "variables.statement"
      ),
      row_style = c("stack", "table"),
      subto = c("main_map", "main_plot", "main_legend"),
      variable_info = FALSE
    )
  ),
  page_section(
    {
      layers <- lapply(list.files("data/nces", "^points_", recursive = TRUE, full.names = TRUE), function(f) {
        list(
          url = f, time = as.numeric(gsub("[^0-9]", "", f))
        )
      })
      output_map(
        list(
          list(
            name = "block_group",
            time = 2010,
            url = "docs/map_2010.geojson",
            id_property = "geoid"
          ),
          list(
            name = "block_group",
            time = 2020,
            url = "docs/map_2020.geojson",
            id_property = "geoid"
          ),
          list(
            name = "tract",
            time = 2010,
            url = paste0(
              "https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/",
              "va_geo_census_cb_2010_census_tracts/distribution/va_geo_census_cb_2010_census_tracts.geojson"
            )
          ),
          list(
            name = "county",
            time = 2010,
            url = paste0(
              "https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/",
              "va_geo_census_cb_2010_counties/distribution/va_geo_census_cb_2010_counties.geojson"
            )
          ),
          list(
            name = "tract",
            time = 2020,
            url = paste0(
              "https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/",
              "va_geo_census_cb_2020_census_tracts/distribution/va_geo_census_cb_2020_census_tracts.geojson"
            )
          ),
          list(
            name = "county",
            time = 2020,
            url = paste0(
              "https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/",
              "va_geo_census_cb_2020_counties/distribution/va_geo_census_cb_2020_counties.geojson"
            )
          )
        ),
        overlays = c(
          list(
            list(
              variable = "nces:schools_2year_per_100k",
              source = layers,
              filter = list(feature = "ICLEVEL", operator = "=", value = 2)
            ),
            list(
              variable = "nces:schools_under2year_per_100k",
              source = layers,
              filter = list(feature = "ICLEVEL", operator = "=", value = 3)
            ),
            list(
              variable = "nces:schools_2year_min_drivetime",
              source = layers,
              filter = list(feature = "ICLEVEL", operator = "=", value = 2)
            ),
            list(
              variable = "nces:schools_under2year_min_drivetime",
              source = layers,
              filter = list(feature = "ICLEVEL", operator = "=", value = 3)
            )
          ),
          lapply(c("biomedical", "computer", "engineering", "physical", "science"), function(p) {
            list(
              variable = paste0("nces:schools_2year_with_", p, "_program_per_100k"),
              source = layers,
              filter = list(
                list(feature = "ICLEVEL", operator = "=", value = 2),
                list(feature = p, operator = "=", value = 1)
              )
            )
          })
        ),
        id = "main_map",
        subto = c("main_plot", "main_legend"),
        options = list(
          attributionControl = FALSE,
          scrollWheelZoom = FALSE,
          height = "500px",
          zoomAnimation = "settings.map_zoom_animation"
        ),
        tiles = list(
          light = list(url = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png"),
          dark = list(url = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png")
        ),
        attribution = list(
          list(
            name = "Stamen toner-light",
            url = "https://stamen.com",
            description = "Light-theme map tiles by Stamen Design"
          ),
          list(
            name = "CARTO Dark Matter",
            url = "https://carto.com/attributions",
            description = "Dark-theme map tiles by CARTO"
          ),
          list(
            name = "OpenStreetMap",
            url = "https://www.openstreetmap.org/copyright"
          )
        )
      )
    },
    output_plot(
      x = "time", y = "selected_variable", id = "main_plot", subto = c("main_map", "main_legend"),
      options = list(
        layout = list(
          xaxis = list(title = FALSE, fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE, zeroline = FALSE)
        ),
        data = data.frame(
          type = c("plot_type", "box"), fillcolor = c(NA, "transparent"),
          hoverinfo = c("text", NA), mode = "lines+markers", showlegend = FALSE,
          name = c(NA, "Summary"), marker.line.color = "#767676", marker.line.width = 1
        ),
        config = list(modeBarButtonsToRemove = c("select2d", "lasso2d", "sendDataToCloud"))
      )
    )
  )
)
