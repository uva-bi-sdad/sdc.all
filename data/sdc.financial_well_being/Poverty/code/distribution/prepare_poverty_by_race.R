library(tidycensus)
library(dplyr)
library(tidyr)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# create named acs variable list
tract_var_list = list(
  below_m = "B17001_003",
  below_f = "B17001_017",
  above_m = "B17001_032",
  above_f = "B17001_046",
  below_m_wht = "B17001A_003",
  below_f_wht = "B17001A_017",
  above_m_wht = "B17001A_032",
  above_f_wht = "B17001A_046",
  below_m_afr_amer = "B17001B_003",
  below_f_afr_amer = "B17001B_017",
  above_m_afr_amer = "B17001B_032",
  above_f_afr_amer = "B17001B_046",
  below_m_asian = "B17001D_003",
  below_f_asian = "B17001D_017",
  above_m_asian = "B17001D_032",
  above_f_asian = "B17001D_046"
)

# read in tract data
acs_tract <- data.table::setDT(
  tidycensus::get_acs(
    year = 2021,
    state = "51",
    county = "059", # fairfax county
    geography = "tract",
    variables = tract_var_list
  )
)

# rename columns
colnames(acs_tract) = c("geoid", "name", "variable", "estimate", "moe")
rm(tract_var_list)

# long to wide
acs_tract_wide = reshape(acs_tract[,c(1,3,4)], idvar = "geoid", timevar = "variable", direction = "wide")
names(acs_tract_wide)[2:ncol(acs_tract_wide)] = substr(names(acs_tract_wide)[2:ncol(acs_tract_wide)],10,30000)

# calculate totals for "other" race
acs_tract_wide$below_m = acs_tract_wide$below_m - acs_tract_wide$below_m_wht - acs_tract_wide$below_m_afr_amer - acs_tract_wide$below_m_asian
acs_tract_wide$below_f = acs_tract_wide$below_f - acs_tract_wide$below_f_wht - acs_tract_wide$below_f_afr_amer - acs_tract_wide$below_f_asian
acs_tract_wide$above_m = acs_tract_wide$above_m - acs_tract_wide$above_m_wht - acs_tract_wide$above_m_afr_amer - acs_tract_wide$above_m_asian
acs_tract_wide$above_f = acs_tract_wide$above_f - acs_tract_wide$above_f_wht - acs_tract_wide$above_f_afr_amer - acs_tract_wide$above_f_asian

names(acs_tract_wide)[2:5] = c("below_m_other","below_f_other","above_m_other","above_f_other")

# calculate poverty as a percent and raw total
tract_pov_percent = data.frame(geoid = acs_tract_wide$geoid,
                               pov_pct_f_wht = acs_tract_wide$below_f_wht / (acs_tract_wide$below_f_wht + acs_tract_wide$above_f_wht),
                               pov_pct_m_wht = acs_tract_wide$below_m_wht / (acs_tract_wide$below_m_wht + acs_tract_wide$above_m_wht),
                               pov_pct_f_afr_amer = acs_tract_wide$below_f_afr_amer / (acs_tract_wide$below_f_afr_amer + acs_tract_wide$above_f_afr_amer),
                               pov_pct_m_afr_amer = acs_tract_wide$below_m_afr_amer / (acs_tract_wide$below_m_afr_amer + acs_tract_wide$above_m_afr_amer),
                               pov_pct_f_asian = acs_tract_wide$below_f_asian / (acs_tract_wide$below_f_asian + acs_tract_wide$above_f_asian),
                               pov_pct_m_asian = acs_tract_wide$below_m_asian / (acs_tract_wide$below_m_asian + acs_tract_wide$above_m_asian),
                               pov_pct_f_other = acs_tract_wide$below_f_other / (acs_tract_wide$below_f_other + acs_tract_wide$above_f_other),
                               pov_pct_m_other = acs_tract_wide$below_m_other / (acs_tract_wide$below_m_other + acs_tract_wide$above_m_other))

tract_pov_below = data.frame(geoid = acs_tract_wide$geoid,
                             below_pov_f_wht = acs_tract_wide$below_f_wht,
                             below_pov_m_wht = acs_tract_wide$below_m_wht,
                             below_pov_f_afr_amer = acs_tract_wide$below_f_afr_amer,
                             below_pov_m_afr_amer = acs_tract_wide$below_m_afr_amer,
                             below_pov_f_asian = acs_tract_wide$below_f_asian,
                             below_pov_m_asian = acs_tract_wide$below_m_asian,
                             below_pov_f_other = acs_tract_wide$below_f_other,
                             below_pov_m_other = acs_tract_wide$below_m_other)

tract_total = data.frame(geoid = acs_tract_wide$geoid,
                         tot_f_wht =  (acs_tract_wide$below_f_wht + acs_tract_wide$above_f_wht),
                         tot_m_wht = (acs_tract_wide$below_m_wht + acs_tract_wide$above_m_wht),
                         tot_f_afr_amer = (acs_tract_wide$below_f_afr_amer + acs_tract_wide$above_f_afr_amer),
                         tot_m_afr_amer = (acs_tract_wide$below_m_afr_amer + acs_tract_wide$above_m_afr_amer),
                         tot_f_asian = (acs_tract_wide$below_f_asian + acs_tract_wide$above_f_asian),
                         tot_m_asian = (acs_tract_wide$below_m_asian + acs_tract_wide$above_m_asian),
                         tot_f_other = (acs_tract_wide$below_f_other + acs_tract_wide$above_f_other),
                         tot_m_other = (acs_tract_wide$below_m_other + acs_tract_wide$above_m_other))

# merge the values
acs_tract_wide = left_join(tract_total,tract_pov_below) %>% left_join(tract_pov_percent)

# wide to long transformation
acs_tract_long = pivot_longer(acs_tract_wide,
                              cols = 2:25,
                              names_to = "measure",
                              values_to = "value")
acs_tract_long$moe = NA
acs_tract_long$year = 2021
acs_tract_long = acs_tract_long[,c("geoid","year","measure","value","moe")]

# save the data
savepath = "Poverty/data/distribution/"
readr::write_csv(acs_tract_long, xzfile(paste0(savepath,"va059_tr_acs_2021_poverty_demographics.csv.xz"), compression = 9))