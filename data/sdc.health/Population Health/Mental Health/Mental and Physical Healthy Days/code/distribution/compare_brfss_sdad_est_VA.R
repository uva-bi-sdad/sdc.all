
library(survey)
library(tidycensus)
library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(sf)

# Set options for allowing a single observation per stratum options
options(survey.lonely.psu = "adjust")

### BRFSS ESTIMATES ###

brfss_2015 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/2015_main.rds")

# remove missing outcome
brfss_2015 <- brfss_2015[!(brfss_2015$physhlth %in% c(77, 99, NA)),]
brfss_2015 <- brfss_2015[!(brfss_2015$menthlth %in% c(77, 99, NA)),]

# transform outcomes to binary
brfss_2015$physhlth <- replace(brfss_2015$physhlth, brfss_2015$physhlth == 88, 0)
brfss_2015$physhlth_binary <- ifelse(brfss_2015$physhlth >= 14, 1, 0)

brfss_2015$menthlth <- replace(brfss_2015$menthlth, brfss_2015$menthlth == 88, 0)
brfss_2015$menthlth_binary <- ifelse(brfss_2015$menthlth >= 14, 1, 0)

va_brfss_2015 <- brfss_2015[brfss_2015$xstate == 51,]

brfss_2015_design <- svydesign(id = ~ xpsu,
                               strata = ~ xststr,
                               data = va_brfss_2015,
                               weight = ~ xllcpwt,
                               nest = TRUE)

phys_2015_brfss <- svymean(~physhlth_binary, brfss_2015_design)
ment_2015_brfss <- svymean(~menthlth_binary, brfss_2015_design)


brfss_2016 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/2016_main.rds")

# remove missing outcome
brfss_2016 <- brfss_2016[!(brfss_2016$physhlth %in% c(77, 99, NA)),]
brfss_2016 <- brfss_2016[!(brfss_2016$menthlth %in% c(77, 99, NA)),]

# transform outcomes to binary
brfss_2016$physhlth <- replace(brfss_2016$physhlth, brfss_2016$physhlth == 88, 0)
brfss_2016$physhlth_binary <- ifelse(brfss_2016$physhlth >= 14, 1, 0)

brfss_2016$menthlth <- replace(brfss_2016$menthlth, brfss_2016$menthlth == 88, 0)
brfss_2016$menthlth_binary <- ifelse(brfss_2016$menthlth >= 14, 1, 0)

va_brfss_2016 <- brfss_2016[brfss_2016$xstate == 51,]

brfss_2016_design <- svydesign(id = ~ xpsu,
                               strata = ~ xststr,
                               data = va_brfss_2016,
                               weight = ~ xllcpwt,
                               nest = TRUE)

phys_2016_brfss <- svymean(~physhlth_binary, brfss_2016_design)
ment_2016_brfss <- svymean(~menthlth_binary, brfss_2016_design)


brfss_2017 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/2017_main.rds")

# remove missing outcome
brfss_2017 <- brfss_2017[!(brfss_2017$physhlth %in% c(77, 99, NA)),]
brfss_2017 <- brfss_2017[!(brfss_2017$menthlth %in% c(77, 99, NA)),]

# transform outcomes to binary
brfss_2017$physhlth <- replace(brfss_2017$physhlth, brfss_2017$physhlth == 88, 0)
brfss_2017$physhlth_binary <- ifelse(brfss_2017$physhlth >= 14, 1, 0)

brfss_2017$menthlth <- replace(brfss_2017$menthlth, brfss_2017$menthlth == 88, 0)
brfss_2017$menthlth_binary <- ifelse(brfss_2017$menthlth >= 14, 1, 0)

va_brfss_2017 <- brfss_2017[brfss_2017$xstate == 51,]

brfss_2017_design <- svydesign(id = ~ xpsu,
                               strata = ~ xststr,
                               data = va_brfss_2017,
                               weight = ~ xllcpwt,
                               nest = TRUE)

phys_2017_brfss <- svymean(~physhlth_binary, brfss_2017_design)
ment_2017_brfss <- svymean(~menthlth_binary, brfss_2017_design)



brfss_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/2018_main.rds")

# remove missing outcome
brfss_2018 <- brfss_2018[!(brfss_2018$physhlth %in% c(77, 99, NA)),]
brfss_2018 <- brfss_2018[!(brfss_2018$menthlth %in% c(77, 99, NA)),]

# transform outcomes to binary
brfss_2018$physhlth <- replace(brfss_2018$physhlth, brfss_2018$physhlth == 88, 0)
brfss_2018$physhlth_binary <- ifelse(brfss_2018$physhlth >= 14, 1, 0)

brfss_2018$menthlth <- replace(brfss_2018$menthlth, brfss_2018$menthlth == 88, 0)
brfss_2018$menthlth_binary <- ifelse(brfss_2018$menthlth >= 14, 1, 0)

va_brfss_2018 <- brfss_2018[brfss_2018$xstate == 51,]

brfss_2018_design <- svydesign(id = ~ xpsu,
                               strata = ~ xststr,
                               data = va_brfss_2018,
                               weight = ~ xllcpwt,
                               nest = TRUE)

phys_2018_brfss <- svymean(~physhlth_binary, brfss_2018_design)
ment_2018_brfss <- svymean(~menthlth_binary, brfss_2018_design)



brfss_2019 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/2019_main.rds")

# remove missing outcome
brfss_2019 <- brfss_2019[!(brfss_2019$physhlth %in% c(77, 99, NA)),]
brfss_2019 <- brfss_2019[!(brfss_2019$menthlth %in% c(77, 99, NA)),]

# transform outcomes to binary
brfss_2019$physhlth <- replace(brfss_2019$physhlth, brfss_2019$physhlth == 88, 0)
brfss_2019$physhlth_binary <- ifelse(brfss_2019$physhlth >= 14, 1, 0)

brfss_2019$menthlth <- replace(brfss_2019$menthlth, brfss_2019$menthlth == 88, 0)
brfss_2019$menthlth_binary <- ifelse(brfss_2019$menthlth >= 14, 1, 0)

va_brfss_2019 <- brfss_2019[brfss_2019$xstate == 51,]

brfss_2019_design <- svydesign(id = ~ xpsu,
                               strata = ~ xststr,
                               data = va_brfss_2019,
                               weight = ~ xllcpwt,
                               nest = TRUE)

phys_2019_brfss <- svymean(~physhlth_binary, brfss_2019_design)
ment_2019_brfss <- svymean(~menthlth_binary, brfss_2019_design)


#### SDAD ESTIMATES ####

brfss_tract_path <- "/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/"

total_18_plus_pop <- c("B01001_007", "B01001_008", "B01001_009", "B01001_010",
                       "B01001_011", "B01001_012", "B01001_013", "B01001_014",
                       "B01001_015", "B01001_016", "B01001_017", "B01001_018",
                       "B01001_016", "B01001_017", "B01001_018", "B01001_019",
                       "B01001_020", "B01001_021", "B01001_022", "B01001_023",
                       "B01001_024", "B01001_025", "B01001_031", "B01001_032",
                       "B01001_033", "B01001_034", "B01001_035", "B01001_036",
                       "B01001_037", "B01001_038", "B01001_039", "B01001_040",
                       "B01001_041", "B01001_042", "B01001_043", "B01001_044",
                       "B01001_044", "B01001_045", "B01001_046", "B01001_047",
                       "B01001_048", "B01001_049")

## 2015 ##
tract_pop_2015 <- get_acs(geography = "tract",
                          state = 51,
                          variables = total_18_plus_pop,
                          year = 2015,
                          survey = "acs5",
                          cache_table = TRUE,
                          output = "wide",
                          geometry = FALSE,
                          keep_geo_vars = FALSE)

tract_pop_2015 <- tract_pop_2015 %>%
  transmute(tract_code = as.numeric(GEOID),
            region_name = NAME,
            total_18_plus_pop = B01001_007E + B01001_008E + B01001_009E +
              B01001_010E + B01001_011E + B01001_012E + B01001_013E +
              B01001_014E + B01001_015E + B01001_016E + B01001_017E +
              B01001_018E + B01001_019E + B01001_020E + B01001_021E +
              B01001_022E + B01001_023E + B01001_024E + B01001_025E +
              B01001_031E + B01001_032E + B01001_033E + B01001_034E +
              B01001_035E + B01001_036E + B01001_037E + B01001_038E +
              B01001_039E + B01001_040E + B01001_041E + B01001_042E +
              B01001_043E + B01001_044E + B01001_045E + B01001_046E +
              B01001_047E + B01001_048E + B01001_049E) %>%
  as.data.frame()

sdad_phys_2015 <- readRDS(paste0(brfss_tract_path, "phys_2015_pred.rds"))
sdad_ment_2015 <- readRDS(paste0(brfss_tract_path, "ment_2015_pred.rds"))

sdad_phys_2015 <- sdad_phys_2015 %>%
  transmute(tract_code = as.numeric(tract_code),
            phys_est = mean_phys * 100)

sdad_ment_2015 <- sdad_ment_2015 %>%
  transmute(tract_code = as.numeric(tract_code),
            ment_est = mean_ment * 100)

sdad_2015 <- sdad_phys_2015 %>% inner_join(sdad_ment_2015, by = "tract_code")

aggregate_sdad_2015 <- sdad_2015 %>%
  inner_join(tract_pop_2015, by = "tract_code")

phys_2015_sdad <- sum(aggregate_sdad_2015$phys_est * aggregate_sdad_2015$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2015$total_18_plus_pop, na.rm = TRUE)
ment_2015_sdad <- sum(aggregate_sdad_2015$ment_est * aggregate_sdad_2015$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2015$total_18_plus_pop, na.rm = TRUE)


## 2016 ##
tract_pop_2016 <- get_acs(geography = "tract",
                          state = 51,
                          variables = total_18_plus_pop,
                          year = 2016,
                          survey = "acs5",
                          cache_table = TRUE,
                          output = "wide",
                          geometry = FALSE,
                          keep_geo_vars = FALSE)

tract_pop_2016 <- tract_pop_2016 %>%
  transmute(tract_code = as.numeric(GEOID),
            region_name = NAME,
            total_18_plus_pop = B01001_007E + B01001_008E + B01001_009E +
              B01001_010E + B01001_011E + B01001_012E + B01001_013E +
              B01001_014E + B01001_015E + B01001_016E + B01001_017E +
              B01001_018E + B01001_019E + B01001_020E + B01001_021E +
              B01001_022E + B01001_023E + B01001_024E + B01001_025E +
              B01001_031E + B01001_032E + B01001_033E + B01001_034E +
              B01001_035E + B01001_036E + B01001_037E + B01001_038E +
              B01001_039E + B01001_040E + B01001_041E + B01001_042E +
              B01001_043E + B01001_044E + B01001_045E + B01001_046E +
              B01001_047E + B01001_048E + B01001_049E) %>%
  as.data.frame()

sdad_phys_2016 <- readRDS(paste0(brfss_tract_path, "phys_2016_pred.rds"))
sdad_ment_2016 <- readRDS(paste0(brfss_tract_path, "ment_2016_pred.rds"))

sdad_phys_2016 <- sdad_phys_2016 %>%
  transmute(tract_code = as.numeric(tract_code),
            phys_est = mean_phys * 100)

sdad_ment_2016 <- sdad_ment_2016 %>%
  transmute(tract_code = as.numeric(tract_code),
            ment_est = mean_ment * 100)

sdad_2016 <- sdad_phys_2016 %>% inner_join(sdad_ment_2016, by = "tract_code")

aggregate_sdad_2016 <- sdad_2016 %>%
  inner_join(tract_pop_2016, by = "tract_code")

phys_2016_sdad <- sum(aggregate_sdad_2016$phys_est * aggregate_sdad_2016$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2016$total_18_plus_pop, na.rm = TRUE)
ment_2016_sdad <- sum(aggregate_sdad_2016$ment_est * aggregate_sdad_2016$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2016$total_18_plus_pop, na.rm = TRUE)


## 2017 ##
tract_pop_2017 <- get_acs(geography = "tract",
                          state = 51,
                          variables = total_18_plus_pop,
                          year = 2017,
                          survey = "acs5",
                          cache_table = TRUE,
                          output = "wide",
                          geometry = FALSE,
                          keep_geo_vars = FALSE)

tract_pop_2017 <- tract_pop_2017 %>%
  transmute(tract_code = as.numeric(GEOID),
            region_name = NAME,
            total_18_plus_pop = B01001_007E + B01001_008E + B01001_009E +
              B01001_010E + B01001_011E + B01001_012E + B01001_013E +
              B01001_014E + B01001_015E + B01001_016E + B01001_017E +
              B01001_018E + B01001_019E + B01001_020E + B01001_021E +
              B01001_022E + B01001_023E + B01001_024E + B01001_025E +
              B01001_031E + B01001_032E + B01001_033E + B01001_034E +
              B01001_035E + B01001_036E + B01001_037E + B01001_038E +
              B01001_039E + B01001_040E + B01001_041E + B01001_042E +
              B01001_043E + B01001_044E + B01001_045E + B01001_046E +
              B01001_047E + B01001_048E + B01001_049E) %>%
  as.data.frame()

sdad_phys_2017 <- readRDS(paste0(brfss_tract_path, "phys_2017_pred.rds"))
sdad_ment_2017 <- readRDS(paste0(brfss_tract_path, "ment_2017_pred.rds"))

sdad_phys_2017 <- sdad_phys_2017 %>%
  transmute(tract_code = as.numeric(tract_code),
            phys_est = mean_phys * 100)

sdad_ment_2017 <- sdad_ment_2017 %>%
  transmute(tract_code = as.numeric(tract_code),
            ment_est = mean_ment * 100)

sdad_2017 <- sdad_phys_2017 %>% inner_join(sdad_ment_2017, by = "tract_code")

aggregate_sdad_2017 <- sdad_2017 %>%
  inner_join(tract_pop_2017, by = "tract_code")

phys_2017_sdad <- sum(aggregate_sdad_2017$phys_est * aggregate_sdad_2017$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2017$total_18_plus_pop, na.rm = TRUE)
ment_2017_sdad <- sum(aggregate_sdad_2017$ment_est * aggregate_sdad_2017$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2017$total_18_plus_pop, na.rm = TRUE)


## 2018
tract_pop_2018 <- get_acs(geography = "tract",
                          state = 51,
                          variables = total_18_plus_pop,
                          year = 2018,
                          survey = "acs5",
                          cache_table = TRUE,
                          output = "wide",
                          geometry = FALSE,
                          keep_geo_vars = FALSE)

tract_pop_2018 <- tract_pop_2018 %>%
  transmute(tract_code = as.numeric(GEOID),
            region_name = NAME,
            total_18_plus_pop = B01001_007E + B01001_008E + B01001_009E +
              B01001_010E + B01001_011E + B01001_012E + B01001_013E +
              B01001_014E + B01001_015E + B01001_016E + B01001_017E +
              B01001_018E + B01001_019E + B01001_020E + B01001_021E +
              B01001_022E + B01001_023E + B01001_024E + B01001_025E +
              B01001_031E + B01001_032E + B01001_033E + B01001_034E +
              B01001_035E + B01001_036E + B01001_037E + B01001_038E +
              B01001_039E + B01001_040E + B01001_041E + B01001_042E +
              B01001_043E + B01001_044E + B01001_045E + B01001_046E +
              B01001_047E + B01001_048E + B01001_049E) %>%
  as.data.frame()

sdad_phys_2018 <- readRDS(paste0(brfss_tract_path, "phys_2018_pred.rds"))
sdad_ment_2018 <- readRDS(paste0(brfss_tract_path, "ment_2018_pred.rds"))

sdad_phys_2018 <- sdad_phys_2018 %>%
  transmute(tract_code = as.numeric(tract_code),
            phys_est = mean_phys * 100)

sdad_ment_2018 <- sdad_ment_2018 %>%
  transmute(tract_code = as.numeric(tract_code),
            ment_est = mean_ment * 100)

sdad_2018 <- sdad_phys_2018 %>% inner_join(sdad_ment_2018, by = "tract_code")

aggregate_sdad_2018 <- sdad_2018 %>%
  inner_join(tract_pop_2018, by = "tract_code")

phys_2018_sdad <- sum(aggregate_sdad_2018$phys_est * aggregate_sdad_2018$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2018$total_18_plus_pop, na.rm = TRUE)
ment_2018_sdad <- sum(aggregate_sdad_2018$ment_est * aggregate_sdad_2018$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2018$total_18_plus_pop, na.rm = TRUE)


## 2019 ##
tract_pop_2019 <- get_acs(geography = "tract",
                          state = 51,
                          variables = total_18_plus_pop,
                          year = 2019,
                          survey = "acs5",
                          cache_table = TRUE,
                          output = "wide",
                          geometry = FALSE,
                          keep_geo_vars = FALSE)

tract_pop_2019 <- tract_pop_2019 %>%
  transmute(tract_code = as.numeric(GEOID),
            region_name = NAME,
            total_18_plus_pop = B01001_007E + B01001_008E + B01001_009E +
              B01001_010E + B01001_011E + B01001_012E + B01001_013E +
              B01001_014E + B01001_015E + B01001_016E + B01001_017E +
              B01001_018E + B01001_019E + B01001_020E + B01001_021E +
              B01001_022E + B01001_023E + B01001_024E + B01001_025E +
              B01001_031E + B01001_032E + B01001_033E + B01001_034E +
              B01001_035E + B01001_036E + B01001_037E + B01001_038E +
              B01001_039E + B01001_040E + B01001_041E + B01001_042E +
              B01001_043E + B01001_044E + B01001_045E + B01001_046E +
              B01001_047E + B01001_048E + B01001_049E) %>%
  as.data.frame()

sdad_phys_2019 <- readRDS(paste0(brfss_tract_path, "phys_2019_pred.rds"))
sdad_ment_2019 <- readRDS(paste0(brfss_tract_path, "ment_2019_pred.rds"))

sdad_phys_2019 <- sdad_phys_2019 %>%
  transmute(tract_code = as.numeric(tract_code),
            phys_est = mean_phys * 100)

sdad_ment_2019 <- sdad_ment_2019 %>%
  transmute(tract_code = as.numeric(tract_code),
            ment_est = mean_ment * 100)

sdad_2019 <- sdad_phys_2019 %>% inner_join(sdad_ment_2019, by = "tract_code")

aggregate_sdad_2019 <- sdad_2019 %>%
  inner_join(tract_pop_2019, by = "tract_code")

phys_2019_sdad <- sum(aggregate_sdad_2019$phys_est * aggregate_sdad_2019$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2019$total_18_plus_pop, na.rm = TRUE)
ment_2019_sdad <- sum(aggregate_sdad_2019$ment_est * aggregate_sdad_2019$total_18_plus_pop, na.rm = TRUE)/sum(aggregate_sdad_2019$total_18_plus_pop, na.rm = TRUE)


comp_phys_res <- data.frame(year = 2015:2019,
                            brfss = c(phys_2015_brfss, phys_2016_brfss, phys_2017_brfss, phys_2018_brfss, phys_2019_brfss),
                            sdad = c(phys_2015_sdad, phys_2016_sdad, phys_2017_sdad, phys_2018_sdad, phys_2019_sdad))


comp_ment_res <- data.frame(year = 2015:2019,
                            brfss = c(ment_2015_brfss, ment_2016_brfss, ment_2017_brfss, ment_2018_brfss, ment_2019_brfss),
                            sdad = c(ment_2015_sdad, ment_2016_sdad, ment_2017_sdad, ment_2018_sdad, ment_2019_sdad))



### put all SDAD results together ###

# tract level phys health estimates #
tract_res_2015 <- aggregate_sdad_2015 %>%
  select(-total_18_plus_pop) %>%
  mutate(year = 2015) %>%
  as.data.frame()

tract_res_2016 <- aggregate_sdad_2016 %>%
  select(-total_18_plus_pop) %>%
  mutate(year = 2016) %>%
  as.data.frame()

tract_res_2017 <- aggregate_sdad_2017 %>%
  select(-total_18_plus_pop) %>%
  mutate(year = 2017) %>%
  as.data.frame()

tract_res_2018 <- aggregate_sdad_2018 %>%
  select(-total_18_plus_pop) %>%
  mutate(year = 2018) %>%
  as.data.frame()

tract_res_2019 <- aggregate_sdad_2019 %>%
  select(-total_18_plus_pop) %>%
  mutate(year = 2019) %>%
  as.data.frame()

tract_sae_res <- rbind(tract_res_2015,
                       tract_res_2016,
                       tract_res_2017,
                       tract_res_2018,
                       tract_res_2019)

tract_sae_res <- tract_sae_res %>%
  arrange(tract_code, year) %>%
  rename(geoid = tract_code,
         perc_poor_phys_hlth_days_14_and_over = phys_est,
         perc_poor_ment_hlth_days_14_and_over = ment_est) %>%
  pivot_longer(!c(geoid, region_name, year), names_to = "measure", values_to = "value") %>%
  mutate(region_type = "tract",
         measure_type = "prevalence estimate") %>%
  select(geoid, region_type, region_name, year, measure, value, measure_type) %>%
  as.data.frame()


#### aggregate to county and health district ####
#county_hd_map <- read.csv("/project/biocomplexity/sdad/projects_data/mc/data_commons/va_county_to_hd.csv")
ct_hd_crosswalk <- readr::read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")
county_hd_map <- ct_hd_crosswalk %>%
  transmute(cnty_code = ct_geoid,
            health_district = hd_name)

aggregate_sdad_2015 <- aggregate_sdad_2015 %>%
  mutate(cnty_code = substr(tract_code, 1, 5)) %>%
  left_join(county_hd_map, by = "cnty_code") %>%
  as.data.frame()

aggregate_sdad_2016 <- aggregate_sdad_2016 %>%
  mutate(cnty_code = substr(tract_code, 1, 5)) %>%
  left_join(county_hd_map, by = "cnty_code") %>%
  as.data.frame()

aggregate_sdad_2017 <- aggregate_sdad_2017 %>%
  mutate(cnty_code = substr(tract_code, 1, 5)) %>%
  left_join(county_hd_map, by = "cnty_code") %>%
  as.data.frame()

aggregate_sdad_2018 <- aggregate_sdad_2018 %>%
  mutate(cnty_code = substr(tract_code, 1, 5)) %>%
  left_join(county_hd_map, by = "cnty_code") %>%
  as.data.frame()

aggregate_sdad_2019 <- aggregate_sdad_2019 %>%
  mutate(cnty_code = substr(tract_code, 1, 5)) %>%
  left_join(county_hd_map, by = "cnty_code") %>%
  as.data.frame()


# county level #

cnty_res_2015 <- aggregate_sdad_2015 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(cnty_code) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2015) %>%
  as.data.frame()

cnty_res_2016 <- aggregate_sdad_2016 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(cnty_code) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2016) %>%
  as.data.frame()

cnty_res_2017 <- aggregate_sdad_2017 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(cnty_code) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2017) %>%
  as.data.frame()

cnty_res_2018 <- aggregate_sdad_2018 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(cnty_code) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2018) %>%
  as.data.frame()

cnty_res_2019 <- aggregate_sdad_2019 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(cnty_code) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2019) %>%
  as.data.frame()

cnty_sae_res <- rbind(cnty_res_2015,
                      cnty_res_2016,
                      cnty_res_2017,
                      cnty_res_2018,
                      cnty_res_2019) %>%
  arrange(cnty_code, year) %>%
  as.data.frame()

# load in standard health district geographic names
library(dc.metadata)
region_names <- region_names$data
county_name <- region_names[region_names$region_type == "county",]
county_name$geoid <- as.character(county_name$geoid)

cnty_sae_res <- cnty_sae_res %>%
  left_join(cnty_geography_names, by = c("cnty_code" = "geoid")) %>%
  rename(geoid = cnty_code,
         perc_poor_phys_hlth_days_14_and_over = phys_est,
         perc_poor_ment_hlth_days_14_and_over = ment_est) %>%
  pivot_longer(!c(geoid, region_name, year), names_to = "measure", values_to = "value") %>%
  mutate(region_type = "county",
         measure_type = "prevalence estimate") %>%
  select(geoid, region_type, region_name, year, measure, value, measure_type) %>%
  as.data.frame()


# health district level #

hd_res_2015 <- aggregate_sdad_2015 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(health_district) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2015) %>%
  as.data.frame()

hd_res_2016 <- aggregate_sdad_2016 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(health_district) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2016)  %>%
  as.data.frame()

hd_res_2017 <- aggregate_sdad_2017 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(health_district) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2017) %>%
  as.data.frame()

hd_res_2018 <- aggregate_sdad_2018 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(health_district) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2018) %>%
  as.data.frame()

hd_res_2019 <- aggregate_sdad_2019 %>%
  mutate(total_phys = phys_est * total_18_plus_pop,
         total_ment = ment_est * total_18_plus_pop) %>%
  group_by(health_district) %>%
  summarise(phys_est = sum(total_phys)/sum(total_18_plus_pop),
            ment_est = sum(total_ment)/sum(total_18_plus_pop)) %>%
  mutate(year = 2019) %>%
  as.data.frame()

hd_sae_res <- rbind(hd_res_2015,
                    hd_res_2016,
                    hd_res_2017,
                    hd_res_2018,
                    hd_res_2019) %>%
  arrange(health_district, year) %>%
  as.data.frame()

# load in standard health district geographic names
#conn <- dbConnect(drv = PostgreSQL(),
#                   dbname = "sdad",
#                   host = "10.250.124.195",
#                   port = 5432,
#                   user = Sys.getenv(x = "db_userid"),
#                   password = Sys.getenv(x = "db_pwd"))
# 
# hd_geography_names <- st_read(conn, query = "SELECT * FROM dc_common.va_hd_sdad_2021_virginia_health_district_geoids")
# 
# dbDisconnect(conn)

hd_sae_res <- hd_sae_res %>%
  left_join(ct_hd_crosswalk, by = c("hd_name" = "region_name")) %>%
  rename(region_name = hd_name,
         perc_poor_phys_hlth_days_14_and_over = phys_est,
         perc_poor_ment_hlth_days_14_and_over = ment_est) %>%
  pivot_longer(!c(geoid, region_name, region_type, year), names_to = "measure", values_to = "value") %>%
  mutate(region_type = "health district",
         measure_type = "prevalence estimate") %>%
  select(geoid, region_type, region_name, year, measure, value, measure_type) %>%
  as.data.frame()


# write to database #
library(RPostgreSQL)

conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv(x = "db_userid"),
                  password = Sys.getenv(x = "db_pwd")
)

# write to data commons database, auto-detect if writing geom or not, change owner to data_commons
dc_dbWriteTable <-
  function(
    db_conn,
    schema_name,
    table_name,
    table_data,
    table_owner = "data_commons"
  ) {
    # check for geometry/geography columns
    tf <- sapply(table_data, {function(x) inherits(x, 'sfc')})
    # if TRUE, use sf
    if (TRUE %in% tf) {
      sf_write_result <- sf::st_write(obj = table_data, dsn = db_conn, layer = c(schema_name, table_name), row.names = FALSE)
      print(sf_write_result)
      # if FALSE, use DBI
    } else {
      write_result <- DBI::dbWriteTable(conn = db_conn, name = c(schema_name, table_name), value = table_data, row.names = FALSE, overwrite = TRUE)
      print(write_result)
    }
    # change table owner
    chgown_result <- DBI::dbSendQuery(conn = db_conn, statement = paste0("ALTER TABLE ", schema_name, ".", table_name, " OWNER TO ", table_owner))
    print(chgown_result)
  }

dc_dbWriteTable(conn,
                "dc_health_behavior_diet",
                "va_tr_sdad_2015_2019_brfss_sae",
                tract_sae_res,
                "data_commons")

dc_dbWriteTable(conn,
                "dc_health_behavior_diet",
                "va_ct_sdad_2015_2019_brfss_sae",
                cnty_sae_res,
                "data_commons")

dc_dbWriteTable(conn,
                "dc_health_behavior_diet",
                "va_hd_sdad_2015_2019_brfss_sae",
                hd_sae_res,
                "data_commons")

dbDisconnect(conn)


# write to csv
cttrhd_res <- rbind(tract_sae_res, cnty_sae_res, hd_sae_res) 

cttrhd_res$region_type <- NULL
cttrhd_res$region_name <- NULL
cttrhd_res$measure_type <- NULL
cttrhd_res$moe = ""

readr::write_csv(cttrhd_res, xzfile("Population Health/Mental Health/Mental and Physical Healthy Days/data/distribution/va_trcthd_sdad_2015_2019_brfss_sae.csv.xz", compression = 9))
