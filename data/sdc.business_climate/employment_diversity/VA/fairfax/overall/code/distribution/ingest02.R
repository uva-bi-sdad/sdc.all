# load employment information from ACS

# library
library(tidycensus)
library(dplyr)
library(tidyr)

# get acs data on employment-----------------------------------------------------
census_api_key(Sys.getenv('census_api_key'))

acs_vars <- c(# tot civilian labour force
  "B23025_003",
  # employed
  "B23025_004")

selectyear <- 2014:2019

# ---------- block group -------------------------------------------------------------
# start with year=2013 (block group are only available after 2012)
acs_emp_bg <- get_acs(geography = "block group",
                      state = "51",
                      variables=acs_vars,
                      year=2013,
                      cache_table=TRUE,
                      output="wide")

acs_labor_bg <- acs_emp_bg %>%
  mutate(year=2013, region_type='block group') %>%
  select(geoid=GEOID, region_type, region_name=NAME, year, laborforce=B23025_003E, employed=B23025_004E) %>%
  pivot_longer(c(laborforce,employed), names_to = "measure", values_to = "value") %>%
  mutate(measure_type='count',
         MOE='')

for (t in selectyear) {
  acs_emp_bg <- get_acs(geography = "block group",
                        state = "51",
                        variables=acs_vars,
                        year=t,
                        cache_table=TRUE,
                        output="wide")
  acs_emp_bg <- acs_emp_bg %>%
    mutate(year=t, region_type='block group') %>%
    select(geoid=GEOID, region_type, region_name=NAME, year, laborforce=B23025_003E, employed=B23025_004E) %>%
    pivot_longer(c(laborforce,employed), names_to = "measure", values_to = "value") %>%
    mutate(measure_type='count',
           MOE='')
  
  acs_labor_bg <- rbind(acs_labor_bg,acs_emp_bg)
}

# save the data --------------------------------------------------------------------------------------------------------
#savepath = "ownership_diversity/VA/fairfax/overall/data/working/"
#readr::write_csv(fairfax_features_operation_geo, xzfile(paste0(savepath,"va_bg_acs_laborforce.csv.xz"), compression = 9))


# ---------- tract level -------------------------------------------------------------
acs_emp_tract <- get_acs(geography = "tract",
                      state = "51",
                      variables=acs_vars,
                      year=2013,
                      cache_table=TRUE,
                      output="wide")

acs_labor_tract <- acs_emp_tract %>%
  mutate(year=2013, region_type='tract') %>%
  select(geoid=GEOID, region_type, region_name=NAME, year, laborforce=B23025_003E, employed=B23025_004E) %>%
  pivot_longer(c(laborforce,employed), names_to = "measure", values_to = "value") %>%
  mutate(measure_type='count',
         MOE='')

for (t in selectyear) {
  acs_emp_tract <- get_acs(geography = "tract",
                        state = "51",
                        variables=acs_vars,
                        year=t,
                        cache_table=TRUE,
                        output="wide")
  acs_emp_tract <- acs_emp_tract %>%
    mutate(year=t, region_type='tract') %>%
    select(geoid=GEOID, region_type, region_name=NAME, year, laborforce=B23025_003E, employed=B23025_004E) %>%
    pivot_longer(c(laborforce,employed), names_to = "measure", values_to = "value") %>%
    mutate(measure_type='count',
           MOE='')
  
  acs_labor_tract <- rbind(acs_labor_tract, acs_emp_tract)
}


# ---------- county level -------------------------------------------------------------
acs_emp_county <- get_acs(geography = "county",
                         state = "51",
                         variables=acs_vars,
                         year=2013,
                         cache_table=TRUE,
                         output="wide")

acs_labor_county <- acs_emp_county %>%
  mutate(year=2013, region_type='county') %>%
  select(geoid=GEOID, region_type, region_name=NAME, year, laborforce=B23025_003E, employed=B23025_004E) %>%
  pivot_longer(c(laborforce,employed), names_to = "measure", values_to = "value") %>%
  mutate(measure_type='count',
         MOE='')

for (t in selectyear) {
  acs_emp_county <- get_acs(geography = "county",
                           state = "51",
                           variables=acs_vars,
                           year=t,
                           cache_table=TRUE,
                           output="wide")
  acs_emp_county <- acs_emp_county %>%
    mutate(year=t, region_type='county') %>%
    select(geoid=GEOID, region_type, region_name=NAME, year, laborforce=B23025_003E, employed=B23025_004E) %>%
    pivot_longer(c(laborforce,employed), names_to = "measure", values_to = "value") %>%
    mutate(measure_type='count',
           MOE='')
  
  acs_labor_county <- rbind(acs_labor_county, acs_emp_county)
}

# combined data from different geographies
acs_labor <- rbind(acs_labor_bg, acs_labor_tract, acs_labor_county)

# save the data --------------------------------------------------------------------------------------------------------
savepath = "employment_diversity/VA/fairfax/overall/data/distribution/"
readr::write_csv(acs_labor, xzfile(paste0(savepath,"va_bgtrct_acs_20132019_laborforce.csv.xz"), compression = 9))

