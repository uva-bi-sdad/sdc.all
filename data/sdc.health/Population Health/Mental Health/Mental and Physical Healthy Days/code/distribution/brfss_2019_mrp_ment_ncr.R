
library(tidycensus)
library(dplyr)
library(purrr)
library(data.table)
library(cmdstanr)
library(posterior)
library(survey)

source("~/git/mrp_utils.R")

### get data to fit MRP model ###
ncr_brfss <- readRDS("~/projects_data/ncr_2011_clean.rds")

# get ACS county data  #
acs_var_2011 <- c("B16009_001", "B16009_002",
                  "B23025_001", "B23025_002",
                  "B06009_001", "B06009_002")

acs_cnty_var <- map_df(c(51, 11, 24),
                       function(s) {
                         get_acs(geography = "county",
                                 state = s,
                                 variables = acs_var_2011,
                                 year = 2011,
                                 survey = "acs5",
                                 cache_table = TRUE,
                                 output = "wide",
                                 geometry = FALSE,
                                 keep_geo_vars = FALSE)
                       }
)

acs_cnty_var <- acs_cnty_var %>%
  transmute(fips = GEOID,
            frac_poverty = B16009_002E / B16009_001E,
            frac_in_labor_force = B23025_002E / B23025_001E,
            frac_less_than_HS = B06009_002E / B06009_001E)

acs_cnty_var$frac_poverty <- (acs_cnty_var$frac_poverty - mean(acs_cnty_var$frac_poverty))/sd(acs_cnty_var$frac_poverty)
acs_cnty_var$frac_in_labor_force <- (acs_cnty_var$frac_in_labor_force - mean(acs_cnty_var$frac_in_labor_force))/sd(acs_cnty_var$frac_in_labor_force)
acs_cnty_var$frac_less_than_HS <- (acs_cnty_var$frac_less_than_HS - mean(acs_cnty_var$frac_less_than_HS))/sd(acs_cnty_var$frac_less_than_HS)

acs_cnty_var <- acs_cnty_var %>%
  filter(fips %in% unique(ncr_brfss$fips)) %>%
  arrange(fips) %>%
  as.data.frame()

### get prediction data at tract level ###

# get ACS tract data for model #
acs_var_tract <- c("B17020_001", "B17020_002",
                   "B23025_001", "B23025_002",
                   "B06009_001", "B06009_002")

acs_tract_var <- map_df(c(51, 11, 24),
                        function(s) {
                          get_acs(geography = "tract",
                                  state = s,
                                  variables = acs_var_tract,
                                  year = 2019,
                                  survey = "acs5",
                                  cache_table = TRUE,
                                  output = "wide",
                                  geometry = FALSE,
                                  keep_geo_vars = FALSE)
                        }
)

acs_tract_var <- acs_tract_var %>%
  transmute(tract_fips = GEOID,
            fips = substr(tract_fips, 1, 5),
            st_fips = substr(tract_fips, 1, 2),
            frac_poverty = B17020_002E / B17020_001E,
            frac_in_labor_force = B23025_002E / B23025_001E,
            frac_less_than_HS = B06009_002E / B06009_001E)

acs_tract_var <- acs_tract_var[complete.cases(acs_tract_var),]

acs_tract_var$frac_poverty <- (acs_tract_var$frac_poverty - mean(acs_tract_var$frac_poverty))/sd(acs_tract_var$frac_poverty)
acs_tract_var$frac_in_labor_force <- (acs_tract_var$frac_in_labor_force - mean(acs_tract_var$frac_in_labor_force))/sd(acs_tract_var$frac_in_labor_force)
acs_tract_var$frac_less_than_HS <- (acs_tract_var$frac_less_than_HS - mean(acs_tract_var$frac_less_than_HS))/sd(acs_tract_var$frac_less_than_HS)

acs_tract_var <- acs_tract_var %>%
  arrange(fips, tract_fips) %>%
  as.data.frame()

sort_fips <- sort(unique(acs_tract_var$fips))
acs_tract_var$fips_cat <- match(acs_tract_var$fips, sort_fips)

sort_st_fips <- sort(unique(acs_tract_var$st_fips))
acs_tract_var$st_fips_cat <- match(acs_tract_var$st_fips, sort_st_fips)

# get poststratification data #

acs_ps_vars <- c(
  # male, 18-19, white
  "B01001A_007",
  # male, 20-24, white
  "B01001A_008",
  # male, 25-29, white
  "B01001A_009",
  # male, 30-34, white
  "B01001A_010",
  # male, 35-44, white
  "B01001A_011",
  # male, 45-54, white
  "B01001A_012",
  # male, 55-64, white
  "B01001A_013",
  # male, 65-74, white
  "B01001A_014",
  # male, 75-84, white
  "B01001A_015",
  # male, 85+, white
  "B01001A_016",
  # female, 18-19, white
  "B01001A_022",
  # female, 20-24, white
  "B01001A_023",
  # female, 25-29, white
  "B01001A_024",
  # female, 30-34, white
  "B01001A_025",
  # female, 35-44, white
  "B01001A_026",
  # female, 45-54, white
  "B01001A_027",
  # female, 55-64, white
  "B01001A_028",
  # female, 65-74, white
  "B01001A_029",
  # female, 75-84, white
  "B01001A_030",
  # female, 85+, white
  "B01001A_031",

  # male, 18-19, black
  "B01001B_007",
  # male, 20-24, black
  "B01001B_008",
  # male, 25-29, black
  "B01001B_009",
  # male, 30-34, black
  "B01001B_010",
  # male, 35-44, black
  "B01001B_011",
  # male, 45-54, black
  "B01001B_012",
  # male, 55-64, black
  "B01001B_013",
  # male, 65-74, black
  "B01001B_014",
  # male, 75-84, black
  "B01001B_015",
  # male, 85+, black
  "B01001B_016",
  # female, 18-19, black
  "B01001B_022",
  # female, 20-24, black
  "B01001B_023",
  # female, 25-29, black
  "B01001B_024",
  # female, 30-34, black
  "B01001B_025",
  # female, 35-44, black
  "B01001B_026",
  # female, 45-54, black
  "B01001B_027",
  # female, 55-64, black
  "B01001B_028",
  # female, 65-74, black
  "B01001B_029",
  # female, 75-84, black
  "B01001B_030",
  # female, 85+, black
  "B01001B_031",

  # male, 18-19, aian
  "B01001C_007",
  # male, 20-24, aian
  "B01001C_008",
  # male, 25-29, aian
  "B01001C_009",
  # male, 30-34, aian
  "B01001C_010",
  # male, 35-44, aian
  "B01001C_011",
  # male, 45-54, aian
  "B01001C_012",
  # male, 55-64, aian
  "B01001C_013",
  # male, 65-74, aian
  "B01001C_014",
  # male, 75-84, aian
  "B01001C_015",
  # male, 85+, aian
  "B01001C_016",
  # female, 18-19, aian
  "B01001C_022",
  # female, 20-24, aian
  "B01001C_023",
  # female, 25-29, aian
  "B01001C_024",
  # female, 30-34, aian
  "B01001C_025",
  # female, 35-44, aian
  "B01001C_026",
  # female, 45-54, aian
  "B01001C_027",
  # female, 55-64, aian
  "B01001C_028",
  # female, 65-74, aian
  "B01001C_029",
  # female, 75-84, aian
  "B01001C_030",
  # female, 85+, aian
  "B01001C_031",

  # male, 18-19, asian
  "B01001D_007",
  # male, 20-24, asian
  "B01001D_008",
  # male, 25-29, asian
  "B01001D_009",
  # male, 30-34, asian
  "B01001D_010",
  # male, 35-44, asian
  "B01001D_011",
  # male, 45-54, asian
  "B01001D_012",
  # male, 55-64, asian
  "B01001D_013",
  # male, 65-74, asian
  "B01001D_014",
  # male, 75-84, asian
  "B01001D_015",
  # male, 85+, asian
  "B01001D_016",
  # female, 18-19, asian
  "B01001D_022",
  # female, 20-24, asian
  "B01001D_023",
  # female, 25-29, asian
  "B01001D_024",
  # female, 30-34, asian
  "B01001D_025",
  # female, 35-44, asian
  "B01001D_026",
  # female, 45-54, asian
  "B01001D_027",
  # female, 55-64, asian
  "B01001D_028",
  # female, 65-74, asian
  "B01001D_029",
  # female, 75-84, asian
  "B01001D_030",
  # female, 85+, asian
  "B01001D_031",

  # male, 18-19, nhpi
  "B01001E_007",
  # male, 20-24, nhpi
  "B01001E_008",
  # male, 25-29, nhpi
  "B01001E_009",
  # male, 30-34, nhpi
  "B01001E_010",
  # male, 35-44, nhpi
  "B01001E_011",
  # male, 45-54, nhpi
  "B01001E_012",
  # male, 55-64, nhpi
  "B01001E_013",
  # male, 65-74, nhpi
  "B01001E_014",
  # male, 75-84, nhpi
  "B01001E_015",
  # male, 85+, nhpi
  "B01001E_016",
  # female, 18-19, nhpi
  "B01001E_022",
  # female, 20-24, nhpi
  "B01001E_023",
  # female, 25-29, nhpi
  "B01001E_024",
  # female, 30-34, nhpi
  "B01001E_025",
  # female, 35-44, nhpi
  "B01001E_026",
  # female, 45-54, nhpi
  "B01001E_027",
  # female, 55-64, nhpi
  "B01001E_028",
  # female, 65-74, nhpi
  "B01001E_029",
  # female, 75-84, nhpi
  "B01001E_030",
  # female, 85+, nhpi
  "B01001E_031",

  # male, 18-19, other
  "B01001F_007",
  # male, 20-24, other
  "B01001F_008",
  # male, 25-29, other
  "B01001F_009",
  # male, 30-34, other
  "B01001F_010",
  # male, 35-44, other
  "B01001F_011",
  # male, 45-54, other
  "B01001F_012",
  # male, 55-64, other
  "B01001F_013",
  # male, 65-74, other
  "B01001F_014",
  # male, 75-84, other
  "B01001F_015",
  # male, 85+, other
  "B01001F_016",
  # female, 18-19, other
  "B01001F_022",
  # female, 20-24, other
  "B01001F_023",
  # female, 25-29, other
  "B01001F_024",
  # female, 30-34, other
  "B01001F_025",
  # female, 35-44, other
  "B01001F_026",
  # female, 45-54, other
  "B01001F_027",
  # female, 55-64, other
  "B01001F_028",
  # female, 65-74, other
  "B01001F_029",
  # female, 75-84, other
  "B01001F_030",
  # female, 85+, other
  "B01001F_031",

  # male, 18-19, multi
  "B01001G_007",
  # male, 20-24, multi
  "B01001G_008",
  # male, 25-29, multi
  "B01001G_009",
  # male, 30-34, multi
  "B01001G_010",
  # male, 35-44, multi
  "B01001G_011",
  # male, 45-54, multi
  "B01001G_012",
  # male, 55-64, multi
  "B01001G_013",
  # male, 65-74, multi
  "B01001G_014",
  # male, 75-84, multi
  "B01001G_015",
  # male, 85+, multi
  "B01001G_016",
  # female, 18-19, multi
  "B01001G_022",
  # female, 20-24, multi
  "B01001G_023",
  # female, 25-29, multi
  "B01001G_024",
  # female, 30-34, multi
  "B01001G_025",
  # female, 35-44, multi
  "B01001G_026",
  # female, 45-54, multi
  "B01001G_027",
  # female, 55-64, multi
  "B01001G_028",
  # female, 65-74, multi
  "B01001G_029",
  # female, 75-84, multi
  "B01001G_030",
  # female, 85+, multi
  "B01001G_031",

  # male, 18-19, hispanic
  "B01001I_007",
  # male, 20-24, hispanic
  "B01001I_008",
  # male, 25-29, hispanic
  "B01001I_009",
  # male, 30-34, hispanic
  "B01001I_010",
  # male, 35-44, hispanic
  "B01001I_011",
  # male, 45-54, hispanic
  "B01001I_012",
  # male, 55-64, hispanic
  "B01001I_013",
  # male, 65-74, hispanic
  "B01001I_014",
  # male, 75-84, hispanic
  "B01001I_015",
  # male, 85+, hispanic
  "B01001I_016",
  # female, 18-19, hispanic
  "B01001I_022",
  # female, 20-24, hispanic
  "B01001I_023",
  # female, 25-29, hispanic
  "B01001I_024",
  # female, 30-34, hispanic
  "B01001I_025",
  # female, 35-44, hispanic
  "B01001I_026",
  # female, 45-54, hispanic
  "B01001I_027",
  # female, 55-64, hispanic
  "B01001I_028",
  # female, 65-74, hispanic
  "B01001I_029",
  # female, 75-84, hispanic
  "B01001I_030",
  # female, 85+, hispanic
  "B01001I_031"
)

acs_ps_var <- map_df(c(51, 11, 24),
                     function(s) {
                       get_acs(geography = "tract",
                               state = s,
                               variables = acs_ps_vars,
                               year = 2019,
                               survey = "acs5",
                               cache_table = TRUE,
                               output = "wide",
                               geometry = FALSE,
                               keep_geo_vars = FALSE)
                     }
)

acs_ps_var <- acs_ps_var %>%
  transmute(fips = substr(GEOID, 1, 5),
            tract_fips = GEOID,
            male_18_24_white = B01001A_007E + B01001A_008E,
            male_25_34_white = B01001A_009E + B01001A_010E,
            male_35_44_white = B01001A_011E,
            male_45_54_white = B01001A_012E,
            male_55_64_white = B01001A_013E,
            male_65_74_white = B01001A_014E,
            male_75_plus_white = B01001A_015E + B01001A_016E,
            female_18_24_white = B01001A_022E + B01001A_023E,
            female_25_34_white = B01001A_024E + B01001A_025E,
            female_35_44_white = B01001A_026E,
            female_45_54_white = B01001A_027E,
            female_55_64_white = B01001A_028E,
            female_65_74_white = B01001A_029E,
            female_75_plus_white = B01001A_030E + B01001A_031E,
            male_18_24_black = B01001B_007E + B01001B_008E,
            male_25_34_black = B01001B_009E + B01001B_010E,
            male_35_44_black = B01001B_011E,
            male_45_54_black = B01001B_012E,
            male_55_64_black = B01001B_013E,
            male_65_74_black = B01001B_014E,
            male_75_plus_black = B01001B_015E + B01001B_016E,
            female_18_24_black = B01001B_022E + B01001B_023E,
            female_25_34_black = B01001B_024E + B01001B_025E,
            female_35_44_black = B01001B_026E,
            female_45_54_black = B01001B_027E,
            female_55_64_black = B01001B_028E,
            female_65_74_black = B01001B_029E,
            female_75_plus_black = B01001B_030E + B01001B_031E,
            male_18_24_aian = B01001C_007E + B01001C_008E,
            male_25_34_aian = B01001C_009E + B01001C_010E,
            male_35_44_aian = B01001C_011E,
            male_45_54_aian = B01001C_012E,
            male_55_64_aian = B01001C_013E,
            male_65_74_aian = B01001C_014E,
            male_75_plus_aian = B01001C_015E + B01001C_016E,
            female_18_24_aian = B01001C_022E + B01001C_023E,
            female_25_34_aian = B01001C_024E + B01001C_025E,
            female_35_44_aian = B01001C_026E,
            female_45_54_aian = B01001C_027E,
            female_55_64_aian = B01001C_028E,
            female_65_74_aian = B01001C_029E,
            female_75_plus_aian = B01001C_030E + B01001C_031E,
            male_18_24_asian = B01001D_007E + B01001D_008E,
            male_25_34_asian = B01001D_009E + B01001D_010E,
            male_35_44_asian = B01001D_011E,
            male_45_54_asian = B01001D_012E,
            male_55_64_asian = B01001D_013E,
            male_65_74_asian = B01001D_014E,
            male_75_plus_asian = B01001D_015E + B01001D_016E,
            female_18_24_asian = B01001D_022E + B01001D_023E,
            female_25_34_asian = B01001D_024E + B01001D_025E,
            female_35_44_asian = B01001D_026E,
            female_45_54_asian = B01001D_027E,
            female_55_64_asian = B01001D_028E,
            female_65_74_asian = B01001D_029E,
            female_75_plus_asian = B01001D_030E + B01001D_031E,
            male_18_24_nhpi = B01001E_007E + B01001E_008E,
            male_25_34_nhpi = B01001E_009E + B01001E_010E,
            male_35_44_nhpi = B01001E_011E,
            male_45_54_nhpi = B01001E_012E,
            male_55_64_nhpi = B01001E_013E,
            male_65_74_nhpi = B01001E_014E,
            male_75_plus_nhpi = B01001E_015E + B01001E_016E,
            female_18_24_nhpi = B01001E_022E + B01001E_023E,
            female_25_34_nhpi = B01001E_024E + B01001E_025E,
            female_35_44_nhpi = B01001E_026E,
            female_45_54_nhpi = B01001E_027E,
            female_55_64_nhpi = B01001E_028E,
            female_65_74_nhpi = B01001E_029E,
            female_75_plus_nhpi = B01001E_030E + B01001E_031E,
            male_18_24_other = B01001F_007E + B01001F_008E,
            male_25_34_other = B01001F_009E + B01001F_010E,
            male_35_44_other = B01001F_011E,
            male_45_54_other = B01001F_012E,
            male_55_64_other = B01001F_013E,
            male_65_74_other = B01001F_014E,
            male_75_plus_other = B01001F_015E + B01001F_016E,
            female_18_24_other = B01001F_022E + B01001F_023E,
            female_25_34_other = B01001F_024E + B01001F_025E,
            female_35_44_other = B01001F_026E,
            female_45_54_other = B01001F_027E,
            female_55_64_other = B01001F_028E,
            female_65_74_other = B01001F_029E,
            female_75_plus_other = B01001F_030E + B01001F_031E,
            male_18_24_multi = B01001G_007E + B01001G_008E,
            male_25_34_multi = B01001G_009E + B01001G_010E,
            male_35_44_multi = B01001G_011E,
            male_45_54_multi = B01001G_012E,
            male_55_64_multi = B01001G_013E,
            male_65_74_multi = B01001G_014E,
            male_75_plus_multi = B01001G_015E + B01001G_016E,
            female_18_24_multi = B01001G_022E + B01001G_023E,
            female_25_34_multi = B01001G_024E + B01001G_025E,
            female_35_44_multi = B01001G_026E,
            female_45_54_multi = B01001G_027E,
            female_55_64_multi = B01001G_028E,
            female_65_74_multi = B01001G_029E,
            female_75_plus_multi = B01001G_030E + B01001G_031E,
            male_18_24_hispanic = B01001I_007E + B01001I_008E,
            male_25_34_hispanic = B01001I_009E + B01001I_010E,
            male_35_44_hispanic = B01001I_011E,
            male_45_54_hispanic = B01001I_012E,
            male_55_64_hispanic = B01001I_013E,
            male_65_74_hispanic = B01001I_014E,
            male_75_plus_hispanic = B01001I_015E + B01001I_016E,
            female_18_24_hispanic = B01001I_022E + B01001I_023E,
            female_25_34_hispanic = B01001I_024E + B01001I_025E,
            female_35_44_hispanic = B01001I_026E,
            female_45_54_hispanic = B01001I_027E,
            female_55_64_hispanic = B01001I_028E,
            female_65_74_hispanic = B01001I_029E,
            female_75_plus_hispanic = B01001I_030E + B01001I_031E
  )

# construct poststratification matrix
ps_mat <- acs_ps_var %>%
  filter(tract_fips %in% unique(acs_tract_var$tract_fips)) %>%
  arrange(fips, tract_fips)

tract_name <- ps_mat$tract_fips

ps_mat <- ps_mat %>%
  select(-c(1, 2)) %>%
  as.data.frame()

rownames(ps_mat) <- tract_name

# indicator for which counties are missing from BRFSS data
county_missing_ind <- as.numeric(!(unique(acs_tract_var$fips) %in% acs_cnty_var$fips))

#### get state prev est from brfss ####
ncr_brfss_2019 <- readRDS("~/projects_data/ncr_brfss_2019.rds")

options(survey.lonely.psu = "adjust")
ncr_brfss_2019_design <- svydesign(ids = ~ xpsu ,
                                   strata = ~ xststr ,
                                   data = ncr_brfss_2019 ,
                                   weights = ~ xllcpwt ,
                                   nest = TRUE)

st_prev_logit <- qlogis(unname(svymean(~menthlth_binary, ncr_brfss_2019_design)[1]))


### MRP ###
brfss_mrp <- cmdstan_model("~/git/brfss_mrp_ncr.stan")


# mental health #
data_list_phys <- list(N = nrow(ncr_brfss),
                       y = ncr_brfss$menthlth_binary,
                       J = ncol(ps_mat),
                       num_obs_cnty = length(unique(ncr_brfss$fips_cat)),
                       num_cnty = length(unique(acs_tract_var$fips)),
                       num_tract = length(unique(acs_tract_var$tract_fips)),
                       num_age = length(unique(ncr_brfss$age_cat)),
                       num_race = length(unique(ncr_brfss$race2)),
                       state_prev = st_prev_logit,
                       gender = ncr_brfss$sex,
                       cnty = ncr_brfss$fips_cat,
                       age = ncr_brfss$age_cat,
                       race = ncr_brfss$race2,
                       poverty = acs_cnty_var$frac_poverty,
                       labor_force = acs_cnty_var$frac_in_labor_force,
                       less_than_HS = acs_cnty_var$frac_less_than_HS,
                       poverty_tract = acs_tract_var$frac_poverty,
                       labor_force_tract = acs_tract_var$frac_in_labor_force,
                       less_than_HS_tract = acs_tract_var$frac_less_than_HS,
                       N_pop = as.vector(t(ps_mat)),
                       cnty_miss_ind = county_missing_ind,
                       tract_cnty_map = acs_tract_var$fips_cat)

brfss_mrp_fit_ment <- brfss_mrp$sample(data = data_list_phys,
                                       seed = 50,
                                       refresh = 0,
                                       chains = 4,
                                       parallel_chains = 4,
                                       iter_warmup = 2500,
                                       iter_sampling = 2500,
                                       step_size = 0.1,
                                       adapt_delta = 0.985)

mrp_ment_draws <- brfss_mrp_fit_ment$draws("y_tract")

res_ment <- as.data.frame(summarise_draws(mrp_ment_draws,
                                          mean, median, sd, mad))

ment_ci <- matrix(NA, nrow = nrow(ps_mat), ncol = 2)
for (i in 1:nrow(ps_mat)) {
  ment_ci[i,] <- spin(mrp_ment_draws[,,i],
                      lower = 0,
                      upper = 1,
                      conf = 0.95)
}

ment_tract_res <- data.frame(tract_code = rownames(ps_mat),
                             mean_ment = res_ment$mean,
                             median_ment = res_ment$median,
                             sd_ment = res_ment$sd,
                             mad_ment = res_ment$mad,
                             lower_pi_ment = ment_ci[,1],
                             upper_pi_ment = ment_ci[,2])

saveRDS(ment_tract_res, "~/git/vdh_brfss_res/ment_2019_pred_ncr.rds")

