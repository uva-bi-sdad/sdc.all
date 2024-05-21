library(data.table)
library(tidycensus)
source("utils/distribution/aggregate.R")


#function for getting different years acs data
get_acsdata <- function(geography, table, state, survey, start_year, end_year) {
  for (year in start_year:end_year) {
    acsdata_temp <- get_acs(geography = geography,
                            table = table,
                            year = year,
                            state = state,
                            survey = survey)
    
    acsdata_temp$year <- year
    if (!exists("acsdata")) acsdata = acsdata_temp else acsdata = rbindlist(list(acsdata, acsdata_temp))
  }
  return(acsdata)
}

# get acs data for tracts
acs_yos_tr <- get_acsdata(geography = "tract",
                       table = "B15003",
                       state = "VA",
                       survey = "acs5",
                       start_year = 2015,
                       end_year = 2022)

# get acs data for counties
acs_yos_ct <- get_acsdata(geography = "county",
                          table = "B15003",
                          state = "VA",
                          survey = "acs5",
                          start_year = 2015,
                          end_year = 2022)

# combine tracts and counties
acs_yos <- rbindlist(list(acs_yos_tr, acs_yos_ct))

# set table to wide (every variable a column)
dc <- dcast(acs_yos, GEOID + NAME + year ~ variable, value.var = "estimate")

# set values for each grade
grade_values <- c("B15003_005" = 1, "B15003_006" = 2, "B15003_007" = 3, "B15003_008" = 4, "B15003_009" = 5, "B15003_010" = 6,
                  "B15003_011" = 7, "B15003_012" = 8, "B15003_013" = 9, "B15003_014" = 10, "B15003_015" = 11, "B15003_016" = 12,
                  "B15003_017" = 12, "B15003_018" = 12, "B15003_019" = 12.5, "B15003_020" = 13, "B15003_021" = 14,
                  "B15003_022" = 16, "B15003_023" = 18, "B15003_024" = 19, "B15003_025" = 20)

# create score columns for each grade
for (i in 1:length(grade_values)) {
  colname <- paste0("score", substr(names(grade_values[i]), 8, 10))
  dc[, (colname) := (get(names(grade_values[i]))/B15003_001)*grade_values[[i]]]
}

# sum score columns to get avg years of schooling
dc[, score_sum := rowSums(.SD), .SDcols=patterns("score")]

# set final table format
avg_yos <- dc[, .(geoid = GEOID, year, measure = "average_years_schooling", value = round(score_sum, 2), moe = "")]


# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## convert
stnd <- standardize_all(avg_yos)

# aggregate counties to health districts
hds <- aggregate(stnd[nchar(stnd$geoid)==5,], "county")
stnd <- rbindlist(list(stnd, hds[!nchar(hds$geoid)==5,]), use.names = T)

# save standardized file
write.csv(stnd, file = xzfile("Years of Schooling/data/distribution/va_hdtrct_acs5_2015_2021_years_of_schooling_std.csv.xz"), row.names = FALSE)








