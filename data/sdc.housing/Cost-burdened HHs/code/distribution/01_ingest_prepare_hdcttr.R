# ACS - Cost Burdened Households: Renters and mortgage-holders that pay more 
# than 30% of income for housing. 

library(dplyr)
library(tidycensus)
library(tidyr)
library(stringr)
library(readr)
library(naniar)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# geographies and years to pull from ACS

geographies <- c("county", "tract")  
years <- c(2014:2021)  

#
# DATA PULL ------------------------------------ 
#

one_pull_dat <- NULL
all_pulls_dat <- NULL

# pull rent, mortgage vars
vars = c("B25074_006", "B25074_007", "B25074_008", "B25074_009",
         "B25074_015", "B25074_016", "B25074_017", "B25074_018",
         "B25074_024", "B25074_025", "B25074_026", "B25074_027", 
         "B25074_033", "B25074_034", "B25074_035", "B25074_036",
         "B25074_042", "B25074_043", "B25074_044", "B25074_045",
         "B25074_051", "B25074_052", "B25074_053", "B25074_054",
         "B25074_060", "B25074_061", "B25074_062", "B25074_063", # rent as above 30%
         "B25101_001", # tot owners
         "B25101_002", # owners with morgage
         "B25101_006", "B25101_010", "B25101_014", "B25101_018",
         "B25101_022", # morgage pay >30% of income
         "B25074_001" # renters
)

# Note: table B25074 available at block group level, but not table B25101

for(year in years)
{
  for(geography in geographies)
  {
    one_pull_dat <- get_acs(geography = geography, variables = vars, state = c("VA", "MD", "DC"), 
                            year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                            output = "wide") %>% 
      transmute(
        geoid = GEOID,
        region_type = as.character(geography),
        #region_name = NAME,
        year = year,
        rent30 = B25074_006E + B25074_007E + B25074_008E + B25074_009E + B25074_015E +
          B25074_016E + B25074_017E + B25074_018E + B25074_024E + B25074_025E +
          B25074_026E + B25074_027E + B25074_033E + B25074_034E + B25074_035E + 
          B25074_036E + B25074_042E + B25074_043E + B25074_044E + B25074_045E +
          B25074_051E + B25074_052E + B25074_053E + B25074_054E + B25074_060E +
          B25074_061E + B25074_062E + B25074_063E, 
        morgage30 = B25101_006E + B25101_010E + B25101_014E + B25101_018E + B25101_022E,
        own_morgage = B25101_002E,
        renters = B25074_001E,
        per_rent_above30 = 100 * rent30 / renters,
        per_mor_above30 = 100 * morgage30 / own_morgage,
        per_above30 = 100 * (rent30 + morgage30)/ (renters + own_morgage)
      ) %>%
      arrange(geoid)
    
    all_pulls_dat <- rbind(all_pulls_dat, one_pull_dat)
  }
}

rm(one_pull_dat)


#
# Missingness check ------------------------------------
#

miss_var_summary(all_pulls_dat)  # NaN in perc when 0/0
all_pulls_dat[is.na(all_pulls_dat$per_rent_above30), "per_rent_above30"] <- 0
all_pulls_dat[is.na(all_pulls_dat$per_mor_above30), "per_mor_above30"] <- 0
all_pulls_dat[is.na(all_pulls_dat$per_above30), "per_above30"] <- 0

#
# Aggregate VA county data to VA Health District data --------------------
#

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get VA county data

va_ct_data <- all_pulls_dat %>%
  filter(substr(geoid,1,2) == "51" & region_type == "county")

# merge with health district info

va_ct_data <- merge(va_ct_data, ct_hd_crosswalk[ , c(1,3)], by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

miss_var_summary(va_ct_data) # no missing data

# aggregate county data to health district data

va_hd_data <- va_ct_data %>%
  group_by(hd_geoid, year) %>%
  summarize(rent30_cnt = sum(rent30),
            morgage30_cnt = sum(morgage30),
            own_morgage_cnt = sum(own_morgage),
            renters_cnt = sum(renters)) %>%
  mutate(rent30_pct = 100 * (rent30_cnt/renters_cnt),
         morgage30_pct = 100 * (morgage30_cnt/own_morgage_cnt),
         tot_above30_pct = 100 * (rent30_cnt + morgage30_cnt)/(renters_cnt + own_morgage_cnt))

colnames(va_hd_data) <- c("geoid", "year", "rent30", "morgage30", "own_morgage", "renters", 
                          "per_rent_above30", "per_mor_above30", "per_above30")
va_hd_data$region_type <- "health district"

va_hd_data <- va_hd_data[ , c(1,10,2:9)]

# add health district data to hh_compdev_all_years

all_pulls_dat <- rbind(all_pulls_dat, va_hd_data)



#
# Wrangle data -------------------------------
#

all_pulls_dat$region_type <- NULL

# format to long
dat_long <- gather(all_pulls_dat, measure, value, rent30:per_above30)
dat_long$moe <- ""


#
# NCR ------------------------------------------
#

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_dat <- dat_long %>% 
  dplyr::filter(str_detect(geoid, ncr_counties))

# save

write_csv(ncr_dat, xzfile("Cost-burdened HHs/data/working/ncr_cttr_acs_2014_2021_cost_burdened_hhs.csv.xz", compression = 9))


#
# VA -----------------------------------------
#

# filter to VA

va_dat <- dat_long %>% 
  dplyr::filter(str_detect(geoid, "^51"))

# save

write_csv(va_dat, xzfile("Cost-burdened HHs/data/working/va_hdcttr_acs_2014_2021_cost_burdened_hhs.csv.xz", compression = 9))
