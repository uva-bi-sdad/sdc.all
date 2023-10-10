library(readr)
library(tidycensus)
census_api_key(Sys.getenv("census_api_key"))

clean <- function(data, geoid_col, measures) {
  #' clean data to fit standard
  #' param: data(dataframe) -> data to clean
  #' param: geoid_col(character) -> current name of geoid column
  #' return: clean dataframe
  cleaned <- data.frame(data)
  colnames(cleaned)[colnames(cleaned) == geoid_col] = "geoid"

  cleaned <- cleaned %>%
    mutate(
      measure = measures,
      moe = ""
    )

  return(cleaned)
}

get_weighted_values <- function(base, group_by_cols) {
  #' get aggregate values for dataframe based on weight
  #' param: base(dataframe) -> base data
  #' param: group_by_cols(character vector) -> columns to group by
  #' return: new aggregated dataframe
  aggregated <- base %>%
    group_by_at(group_by_cols) %>%
    summarise(
      weight = sum(weight),
      pop_wgt_val = sum(pop_wgt_val)
    )
  #aggregated <- aggregated %>% filter(weight != 0)
  aggregated$value <- aggregated$pop_wgt_val / aggregated$weight

  return(aggregated)
}
#, na.rm = TRUE

get_pop_data <- function(base_level, weight_col, table_name="B01003") {
  #' get population data for given population at given level
  #' param: base_level(character) -> geography level to get
  #' param: weight_col(character) -> acs code for population variable to get
  #' param: table_name(character -- optional, default "B01003") -> table
  #'              containing wanted variable
  #' return: dataframe with population data
  yrs = seq(2015, 2022)  # 2022 ACS data not yet available.  Used 2021 instead
  pop_base <- NULL

  for (j in 1:length(yrs))
  {
    y = ifelse(yrs[j] == 2022, 2021, yrs[j])

    pop_base_yr <- get_acs(geography = base_level, table = table_name, state = "VA",
                           year = y, geometry = TRUE, survey = "acs5", cache_table = TRUE,
                           output = "wide", keep_geo_vars = TRUE) %>%
      mutate(
        geoid = GEOID,
        year = yrs[j]
      )

    colnames(pop_base_yr)[colnames(pop_base_yr) == weight_col] = "weight"
    pop_base_yr <- pop_base_yr %>% select(geoid, year, weight)

    pop_base <- rbind(pop_base, pop_base_yr)
  }

  return(pop_base)
}

aggregate <- function(data, base_level, method=mean, weight_col=NULL) {
  #' aggregate lower level geography data up to health district
  #' param: data(dataframe) -> starting lower level geography data
  #' param: base_level(character) -> geography level of data
  #'            (must be tract or county)
  #' param: method(function -- optional, default mean) -> aggregation method
  #' param: weight_col(character -- optional, default NULL) ->
  #'            acs code of variable to weight by
  #'            !! overrides aggregation method to weighted mean !!
  #' return: combined dataframe of base geography data and aggregated data for
  #'            any above geography levels
  #'            (tract, county, health district OR county, health district)
  ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv",
                              col_types = "cccc")

  # copy original data
  base <- data.frame(data) %>% filter(!is.na(value)) %>% mutate(value=as.numeric(value))
  base$geoid <- as.character(base$geoid)

  measures = unique(base$measure)

  # aggregate to next level
  base$st_fips <- substr(base$geoid, 1, 5)

  # aggregate by weighted mean
  if (!is.null(weight_col)) {
    # add population data to base data
    pop_base <- get_pop_data(base_level, weight_col)
    base <- merge(base, pop_base, by = c("geoid", "year"), all.x = TRUE)
    base$pop_wgt_val <- base$value * base$weight

    if (length(setdiff(base$geoid, pop_base$geoid)) > 0)  # empty - good
    {
      stop("Inconsistent tract ids")
    }

    combined <- data.frame(base) %>%
      select(geoid, measure, value, year, moe)

    # get aggregate data for county
    if (base_level == "tract") {
      county <- get_weighted_values(base, c("st_fips", "year"))
      county <- clean(county, "st_fips", measures)

      # county becomes new base
      base <- county

      # combine
      county <- county %>% select(geoid, measure, value, year, moe)
      combined <- rbind(combined, county)
    }

    # get aggregate data for health district
    base <- merge(base, ct_hd_crosswalk[ , c("ct_geoid", "hd_geoid")],
                  by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

    hlth_dis <- get_weighted_values(base, c("hd_geoid", "year"))
    hlth_dis <- clean(hlth_dis, "hd_geoid", measures) %>%
      select(geoid, measure, value, year, moe)

    # combine
    combined <- rbind(combined, hlth_dis)
  }

  # aggregate by specified method
  else {
    combined <- data.frame(base) %>%
      select(geoid, measure, value, year, moe)

    # get county aggregates
    if (base_level == "tract") {
      county <- base %>%
        group_by(st_fips, year) %>%
        summarise(
          value = method(value)
        )

      county <- clean(county, "st_fips", measures)

      # county becomes new base
      base <- county

      # combine
      county <- county %>% select(geoid, measure, value, year, moe)
      combined <- rbind(combined, county)
    }

    # get health district aggregates
    base <- merge(base, ct_hd_crosswalk[ , c("ct_geoid", "hd_geoid")],
                  by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

    hlth_dis <- base %>%
      group_by(hd_geoid, year) %>%
      summarise(
        value = mean(value)
      )

    hlth_dis <- clean(hlth_dis, "hd_geoid", measures) %>%
      select(geoid, measure, value, year, moe)

    # combine
    combined <- rbind(combined, hlth_dis)
  }

  return(combined)
}
