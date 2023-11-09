# Replicating Income Inequality Indicator of the VHD's HOI Economic Opportunity Profile
# https://apps.vdh.virginia.gov/omhhe/hoi/economic-opportunity-profile

# packages
library(readxl)
library(dplyr)
# library(tidyverse)
library(sf)
library(reshape2)

# data from HOI website
orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/Gini_ct_map.xlsx")
df_tracts <- orig_df[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
                        "Ctfips", "Indicator Selector")] 
df_tracts <- df_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# assign quintiles
df_tracts <- df_tracts %>% mutate(
  quintiles = case_when(
    `Indicator Selector` == "Very Low" ~ 1, 
    `Indicator Selector` == "Low" ~2,
    `Indicator Selector` == "Average" ~3,
    `Indicator Selector` == "High" ~4,
    `Indicator Selector` == "Very High" ~5
  )
)
df_tracts["new_id"] <- paste0(df_tracts$Ctfips, "_", df_tracts$quintiles)
# remove duplicates
df_tracts_nodups <- df_tracts %>% distinct()
# check if the census tarct ID is unique
#df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique

# rename measures
out_df <- df_tracts_nodups %>% 
  rename( "geoid"= "Ctfips",
          "income_inequality_indicator" = "quintiles")
out_df <- out_df[,c("geoid", "income_inequality_indicator")]


# long format
out_long <- melt(out_df,
                 id.vars=c("geoid"),
                 variable.name="measure",
                 value.name="value"
)

# add missing columns
out_long["year"] <- "2017"
out_long["moe"] <- ""

# Select final columns
out_long <- out_long[, c("geoid", "year", "measure", "value", "moe")]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

out_long[out_long$geoid == "51515050100", "geoid"] <- "51019050100"

ct_hd_crosswalk <- read_csv("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv", 
                            col_types = "cccc")

# get population data

yrs = c(2017, 2022)  # 2022 ACS data not yet available.  Used 2021 instead
pop_tr <- NULL 

for (j in 1:length(yrs))
{
  y = ifelse(yrs[j] == 2022, 2021, yrs[j])
  
  pop_tr_yr <- get_acs(geography = "tract", variables = "B01003_001", state = "VA", 
                       year = y, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                       output = "wide") %>%
    transmute(
      geoid = GEOID,
      year = yrs[j],
      pop = B01003_001E
    ) 
  
  pop_tr <- rbind(pop_tr, pop_tr_yr)
}

tract <- out_long
tract$geoid <- as.character(tract$geoid)

m = unique(tract$measure)

# aggregate to county level using population weighted estimate

tract$st_fips <- substr(tract$geoid, 1, 5)

if (length(setdiff(tract$geoid, pop_tr$geoid)) > 0)  # empty - good
{ 
  print(paste0("Check tract ids in ", tr_files[i]))
}

tract <- merge(tract, pop_tr, by = c("geoid", "year"), all.x = TRUE)
tract$pop_wgt_val <- tract$value * tract$pop

county <- tract %>%
  group_by(st_fips, year) %>%
  summarise(
    ct_pop = sum(pop),
    ct_pop_wgt_val = sum(pop_wgt_val)
  )

county$value <- county$ct_pop_wgt_val / county$ct_pop

county <- county %>%
  rename(geoid = st_fips) %>%
  mutate(
    measure = m,
    moe = ""
  ) 

# aggregate to health district level using population weighted estimates

county <- merge(county, ct_hd_crosswalk[ , c("ct_geoid", "hd_geoid")], 
                by.x = "geoid", by.y = "ct_geoid", all.x = TRUE)

hlth_dis <- county %>%
  group_by(hd_geoid, year) %>%
  summarise(
    hd_pop = sum(ct_pop),
    hd_pop_wgt_val = sum(ct_pop_wgt_val)
  )

hlth_dis$value <- hlth_dis$hd_pop_wgt_val / hlth_dis$hd_pop

hlth_dis <- hlth_dis %>%
  rename(geoid = hd_geoid) %>%
  mutate(
    measure = m,
    moe = ""
  ) 

tract <- tract %>%
  select(geoid, measure, value, year, moe)

county <- county %>%
  select(geoid, measure, value, year, moe)

hlth_dis <- hlth_dis %>%
  select(geoid, measure, value, year, moe)

hdcttr <- rbind(hlth_dis, county, tract)

# save the dataset 
write_csv(hdcttr, xzfile(paste0("Population Health/Health Opportunity Index/data/distribution/va_hdcttr_vdh_2017_income_inequality_index.csv.xz"), compression = 9) )


# save the dataset 
# write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_income_inequality_index.csv.xz", compression = 9))
