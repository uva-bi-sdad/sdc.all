library(tidycensus)
library(data.table)
library(dplyr)
library(tidyr)

census_api_key(Sys.getenv('CENSUS_API_KEY'))
source("~/git/sdc.education_dev/utils/distribution/aggregate.R")

#function for calculating ays
# get_ays <- function(acs_data, tract_geoid) {
#   tract_data <- acs_data[acs_data$GEOID==tract_geoid, ]
# 
#   # total population
#   pop <- tract_data[tract_data$variable %in% c("B15003_001"), c("estimate")][[1]]
# 
#   # value assigned to each grade level (no schooling, nursery school, kindergarten each assigned 0)
#   values <- c("005" = 1, "006" = 2, "007" = 3, "008" = 4, "009" = 5, "010" = 6,
#               "011" = 7, "012" = 8, "013" = 9, "014" = 10, "015" = 11, "016" = 12,
#               "017" = 12, "018" = 12, "019" = 12.5, "020" = 13, "021" = 14,
#               "022" = 16, "023" = 18, "024" = 19, "025" = 20)
# 
#   ays <- 0
# 
#   # calculate years of schooling for each variable to get average years of schooling
#   for (var in names(values)) {
#     temp <- tract_data[tract_data$variable == paste0("B15003_", var),] %>% filter(!is.na(estimate))
# 
#     # add current grade level to average years of schooling
#     if (!is.null(temp)){
#       ays <- ays + ((sum(temp$estimate)/pop) * values[[var]])
#     }
#   }
# 
#   return (ays)
# 
# }

get_ays <- function(acs_data, tract_geoid) {
  #browser()
  pop_mf <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_001"), c("estimate")][[1]]
  pop_m <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_002"), c("estimate")][[1]]
  pop_f <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_043"), c("estimate")][[1]]

  m_lt_9gr <-       acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_004", "B15001_012", "B15001_020", "B15001_028", "B15001_036"),]
  m_hs_grad_no <-   acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_005", "B15001_013", "B15001_021", "B15001_029", "B15001_037"),]
  m_hs_grad_yes <-  acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_006", "B15001_014", "B15001_022", "B15001_030", "B15001_038"),]
  m_col_some <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_007", "B15001_015", "B15001_023", "B15001_031", "B15001_039"),]
  m_col_asoc <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_008", "B15001_016", "B15001_024", "B15001_032", "B15001_040"),]
  m_col_bach <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_009", "B15001_017", "B15001_025", "B15001_033", "B15001_041"),]
  m_col_grad <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_010", "B15001_018", "B15001_026", "B15001_034", "B15001_042"),]

  f_lt_9gr <-       acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_045", "B15001_053", "B15001_061", "B15001_069", "B15001_077"),]
  f_hs_grad_no <-   acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_046", "B15001_054", "B15001_062", "B15001_070", "B15001_078"),]
  f_hs_grad_yes <-  acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_047", "B15001_055", "B15001_063", "B15001_071", "B15001_079"),]
  f_col_some <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_048", "B15001_056", "B15001_064", "B15001_072", "B15001_080"),]
  f_col_asoc <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_049", "B15001_057", "B15001_065", "B15001_073", "B15001_081"),]
  f_col_bach <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_050", "B15001_058", "B15001_066", "B15001_074", "B15001_082"),]
  f_col_grad <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_051", "B15001_059", "B15001_067", "B15001_075", "B15001_083"),]

  lt_9gr <- rbind(m_lt_9gr, f_lt_9gr)
  lt_9gr_ays <- (sum(lt_9gr$estimate)/pop_mf) * 7.5

  hs_grad_no <- rbind(m_hs_grad_no, f_hs_grad_no)
  hs_grad_no_ays <- (sum(hs_grad_no$estimate)/pop_mf) * 11

  hs_grad_yes <- rbind(m_hs_grad_yes, f_hs_grad_yes)
  hs_grad_yes_ays <- (sum(hs_grad_yes$estimate)/pop_mf) * 12

  col_some <- rbind(m_col_some, f_col_some)
  col_some_ays <- (sum(col_some$estimate)/pop_mf) * 13

  col_asoc <- rbind(m_col_asoc, f_col_asoc)
  col_asoc_ays <- (sum(col_asoc$estimate)/pop_mf) * 14

  col_bach <- rbind(m_col_bach, f_col_bach)
  col_bach_ays <- (sum(col_bach$estimate)/pop_mf) * 16

  col_grad <- rbind(m_col_grad, f_col_grad)
  col_grad_ays <- (sum(col_grad$estimate)/pop_mf) * 19

  ays <- sum(lt_9gr_ays, hs_grad_no_ays, hs_grad_yes_ays, col_some_ays, col_asoc_ays, col_bach_ays, col_grad_ays)
  ays
}



#function for getting different years acs data
get_acsdata <- function(geography, table, state, survey, start_year, end_year) {
  acsdata <- NULL
  for (year in start_year:end_year) {
    acsdata_temp <- get_acs(geography = geography,
                            table = table,
                            year = year,
                            state = state,
                            survey = survey) %>% mutate(year=year)

    acsdata <- rbind(acsdata, acsdata_temp)
  }

  return(acsdata)
}

#get acs data for seven years tracts
acs_yos <- get_acsdata(geography = "tract",
                       table = "B15001",
                       state = "VA",
                       survey = "acs5",
                       start_year = 2015,
                       end_year = 2021)

all_tr <- NULL
#different years calculation
for (yr in c(2021, 2020, 2019, 2018, 2017, 2016, 2015)) {
  # Get the data for the current year
  acsdata_year <- acs_yos %>% filter(year==yr)
  acsdata_year_dt <- data.table::setDT(acsdata_year)

  tract_ays <- NULL

  # Calculate AYS for all tracts
  unq_tracts <- unique(acsdata_year_dt$GEOID)
  for (t in unq_tracts) {
    ays_t <- get_ays(acsdata_year_dt, t)

    tract_ays <- data.table::rbindlist(list(tract_ays, data.table(GEOID=c(t), value=c(ays_t)))) #rbind(tract_ays, data.frame(GEOID=c(t), value=c(ays_t)))
  }

  acsdata_year_dt <- acsdata_year_dt %>% group_by(GEOID) %>% summarise(year=first(year), moe='')
  acsdata_year_dt <- merge(acsdata_year_dt, tract_ays, by='GEOID')

  all_tr <- data.table::rbindlist(list(all_tr, acsdata_year_dt))
}

all_tr <- all_tr %>% filter(!is.nan(value)) %>% rename(geoid=GEOID)

#get acs data for seven years counties
acs_yos <- get_acsdata(geography = "county",
                       table = "B15001",
                       state = "VA",
                       survey = "acs5",
                       start_year = 2015,
                       end_year = 2021)

all_ct <- NULL
#different years calculation
for (yr in c(2021, 2020, 2019, 2018, 2017, 2016, 2015)) {
  # Get the data for the current year
  acsdata_year <- acs_yos %>% filter(year==yr)
  acsdata_year_dt <- data.table::setDT(acsdata_year)

  tract_ays <- NULL

  # Calculate AYS for all tracts
  unq_tracts <- unique(acsdata_year$GEOID)
  for (t in unq_tracts) {
    ays_t <- get_ays(acsdata_year_dt, t)

    tract_ays <- data.table::rbindlist(list(tract_ays, data.table(GEOID=c(t), value=c(ays_t)))) #tract_ays <- rbind(tract_ays, data.frame(GEOID=c(t), value=c(ays_t)))
  }

  acsdata_year_dt <- acsdata_year_dt %>% group_by(GEOID) %>% summarise(year=first(year), moe='')
  acsdata_year_dt <- merge(acsdata_year_dt, tract_ays, by='GEOID')

  all_ct <- data.table::rbindlist(list(all_ct, acsdata_year_dt))
}

all_ct <- all_ct %>% filter(!is.nan(value)) %>% rename(geoid=GEOID)

# combine tract, county, health district data
tr_ct <- rbind(all_tr, all_ct)

readr::write_csv(tr_ct, xzfile('Years of Schooling/data/working/va_trct_acs5_2015_2021_years_of_schooling_2.csv', compression=9))

# testing <- readxl::read_excel('~/git/HOI V3_14 Variables_Raw Scores (1).xlsx') %>%
#   select('CT2', 'Education')
#
# merged <- merge(testing, all_tr %>% filter(year==2020) %>% mutate(value1=round(value, 1)), by.x='CT2', by.y='geoid')
# ggplot(merged, aes(x=Education, y=value1)) + geom_point()  + geom_abline(slope=1)






