
library(survey)
library(tidycensus)
library(dplyr)
library(tidyr)
library(purrr)
library(RPostgreSQL)
library(sf)

# Set options for allowing a single observation per stratum options
options(survey.lonely.psu = "adjust")

ncr_df <- read.csv("/project/biocomplexity/sdad/projects_data/mc/data_commons/capital_regions_df.csv")

### BRFSS ESTIMATES ###

# 2015 #
ncr_brfss_2015 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/ncr_brfss_2015.rds")

ncr_brfss_2015_design <- svydesign(ids = ~ xpsu ,
                                   strata = ~ xststr ,
                                   data = ncr_brfss_2015 ,
                                   weights = ~ xllcpwt ,
                                   nest = TRUE)

phys_2015_brfss <- svymean(~physhlth_binary, ncr_brfss_2015_design)
ment_2015_brfss <- svymean(~menthlth_binary, ncr_brfss_2015_design)

# 2016 #
ncr_brfss_2016 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/ncr_brfss_2016.rds")

ncr_brfss_2016_design <- svydesign(ids = ~ xpsu ,
                                   strata = ~ xststr ,
                                   data = ncr_brfss_2016 ,
                                   weights = ~ xllcpwt ,
                                   nest = TRUE)

phys_2016_brfss <- svymean(~physhlth_binary, ncr_brfss_2016_design)
ment_2016_brfss <- svymean(~menthlth_binary, ncr_brfss_2016_design)

# 2017 #
ncr_brfss_2017 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/ncr_brfss_2017.rds")

ncr_brfss_2017_design <- svydesign(ids = ~ xpsu ,
                                   strata = ~ xststr ,
                                   data = ncr_brfss_2017 ,
                                   weights = ~ xllcpwt ,
                                   nest = TRUE)

phys_2017_brfss <- svymean(~physhlth_binary, ncr_brfss_2017_design)
ment_2017_brfss <- svymean(~menthlth_binary, ncr_brfss_2017_design)

# 2018 #
ncr_brfss_2018 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/ncr_brfss_2018.rds")

ncr_brfss_2018_design <- svydesign(ids = ~ xpsu ,
                                   strata = ~ xststr ,
                                   data = ncr_brfss_2018 ,
                                   weights = ~ xllcpwt ,
                                   nest = TRUE)

phys_2018_brfss <- svymean(~physhlth_binary, ncr_brfss_2018_design)
ment_2018_brfss <- svymean(~menthlth_binary, ncr_brfss_2018_design)

# 2019 #
ncr_brfss_2019 <- readRDS("/project/biocomplexity/sdad/projects_data/mc/data_commons/dc_health_behavior_diet/brfss/ncr_brfss_2019.rds")

ncr_brfss_2019_design <- svydesign(ids = ~ xpsu ,
                                   strata = ~ xststr ,
                                   data = ncr_brfss_2019 ,
                                   weights = ~ xllcpwt ,
                                   nest = TRUE)

phys_2019_brfss <- svymean(~physhlth_binary, ncr_brfss_2019_design)
ment_2019_brfss <- svymean(~menthlth_binary, ncr_brfss_2019_design)


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
tract_pop_2015 <- map_df(c(51, 11, 24),
                         function(s) {
                           get_acs(geography = "tract",
                                   state = s,
                                   variables = total_18_plus_pop,
                                   year = 2015,
                                   survey = "acs5",
                                   cache_table = TRUE,
                                   output = "wide",
                                   geometry = FALSE,
                                   keep_geo_vars = FALSE)
                         }
)

tract_pop_2015 <- tract_pop_2015 %>%
  transmute(tract_code = as.numeric(GEOID),
            fips = substr(tract_code, 1, 5),
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
  filter(fips %in% unique(ncr_df$cnty_fips)) %>%
  select(tract_code, region_name, total_18_plus_pop) %>%
  as.data.frame()

sdad_phys_2015 <- readRDS(paste0(brfss_tract_path, "phys_2015_pred_ncr.rds"))
sdad_ment_2015 <- readRDS(paste0(brfss_tract_path, "ment_2015_pred_ncr.rds"))

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
tract_pop_2016 <- map_df(c(51, 11, 24),
                         function(s) {
                           get_acs(geography = "tract",
                                   state = s,
                                   variables = total_18_plus_pop,
                                   year = 2016,
                                   survey = "acs5",
                                   cache_table = TRUE,
                                   output = "wide",
                                   geometry = FALSE,
                                   keep_geo_vars = FALSE)
                         }
)

tract_pop_2016 <- tract_pop_2016 %>%
  transmute(tract_code = as.numeric(GEOID),
            fips = substr(tract_code, 1, 5),
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
  filter(fips %in% unique(ncr_df$cnty_fips)) %>%
  select(tract_code, region_name, total_18_plus_pop) %>%
  as.data.frame()

sdad_phys_2016 <- readRDS(paste0(brfss_tract_path, "phys_2016_pred_ncr.rds"))
sdad_ment_2016 <- readRDS(paste0(brfss_tract_path, "ment_2016_pred_ncr.rds"))

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
tract_pop_2017 <- map_df(c(51, 11, 24),
                         function(s) {
                           get_acs(geography = "tract",
                                   state = s,
                                   variables = total_18_plus_pop,
                                   year = 2017,
                                   survey = "acs5",
                                   cache_table = TRUE,
                                   output = "wide",
                                   geometry = FALSE,
                                   keep_geo_vars = FALSE)
                         }
)

tract_pop_2017 <- tract_pop_2017 %>%
  transmute(tract_code = as.numeric(GEOID),
            fips = substr(tract_code, 1, 5),
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
  filter(fips %in% unique(ncr_df$cnty_fips)) %>%
  select(tract_code, region_name, total_18_plus_pop) %>%
  as.data.frame()


sdad_phys_2017 <- readRDS(paste0(brfss_tract_path, "phys_2017_pred_ncr.rds"))
sdad_ment_2017 <- readRDS(paste0(brfss_tract_path, "ment_2017_pred_ncr.rds"))

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


## 2018 ##
tract_pop_2018 <- map_df(c(51, 11, 24),
                         function(s) {
                           get_acs(geography = "tract",
                                   state = s,
                                   variables = total_18_plus_pop,
                                   year = 2018,
                                   survey = "acs5",
                                   cache_table = TRUE,
                                   output = "wide",
                                   geometry = FALSE,
                                   keep_geo_vars = FALSE)
                         }
)

tract_pop_2018 <- tract_pop_2018 %>%
  transmute(tract_code = as.numeric(GEOID),
            fips = substr(tract_code, 1, 5),
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
  filter(fips %in% unique(ncr_df$cnty_fips)) %>%
  select(tract_code, region_name, total_18_plus_pop) %>%
  as.data.frame()


sdad_phys_2018 <- readRDS(paste0(brfss_tract_path, "phys_2018_pred_ncr.rds"))
sdad_ment_2018 <- readRDS(paste0(brfss_tract_path, "ment_2018_pred_ncr.rds"))

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
tract_pop_2019 <- map_df(c(51, 11, 24),
                         function(s) {
                           get_acs(geography = "tract",
                                   state = s,
                                   variables = total_18_plus_pop,
                                   year = 2019,
                                   survey = "acs5",
                                   cache_table = TRUE,
                                   output = "wide",
                                   geometry = FALSE,
                                   keep_geo_vars = FALSE)
                         }
)

tract_pop_2019 <- tract_pop_2019 %>%
  transmute(tract_code = as.numeric(GEOID),
            fips = substr(tract_code, 1, 5),
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
  filter(fips %in% unique(ncr_df$cnty_fips)) %>%
  select(tract_code, region_name, total_18_plus_pop) %>%
  as.data.frame()


sdad_phys_2019 <- readRDS(paste0(brfss_tract_path, "phys_2019_pred_ncr.rds"))
sdad_ment_2019 <- readRDS(paste0(brfss_tract_path, "ment_2019_pred_ncr.rds"))

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
county_hd_map <- read.csv("/project/biocomplexity/sdad/projects_data/mc/data_commons/va_county_to_hd.csv")
county_hd_map <- county_hd_map %>%
  transmute(cnty_code = as.character(county_id),
            health_district)

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
  left_join(county_name, by = c("cnty_code" = "geoid")) %>%
  rename(geoid = cnty_code,
         perc_poor_phys_hlth_days_14_and_over = phys_est,
         perc_poor_ment_hlth_days_14_and_over = ment_est) %>%
  pivot_longer(!c(geoid, region_name, region_type, year), names_to = "measure", values_to = "value") %>%
  mutate(measure_type = "prevalence estimate") %>%
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
                "ncr_tr_sdad_2015_2019_brfss_sae",
                tract_sae_res,
                "data_commons")

dc_dbWriteTable(conn,
                "dc_health_behavior_diet",
                "ncr_ct_sdad_2015_2019_brfss_sae",
                cnty_sae_res,
                "data_commons")

dbDisconnect(conn)

# write to csv
cttr_res <- rbind(tract_sae_res, cnty_sae_res) 
readr::write_csv(cttr_res, xzfile("Population Health/Mental Health/Mental and Physical Healthy Days/data/distribution/ncr_trct_sdad_2015_2019_brfss_sae.csv.xz", compression = 9))


