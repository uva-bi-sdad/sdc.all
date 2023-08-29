#
# ingest ACS data: adults below the poverty line
#

library(dplyr)
library(tidycensus)
library(tidyr)
library(stringr)
library(readr)
library(naniar)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# geographies and years to pull from ACS

geographies <- c("tract")  
years <- c(2021)  

#
# DATA PULL ------------------------------------ 
#

data_one_yr <- NULL
data_all_yrs <- NULL


# create list of variables to pull from B17001# tables
vars = list()

# men_pov = 10:16, men_no_pov = 39:45, women_pov = 24:30, women_no_pov = 53:59
v = c(10:16, 39:45, 24:30, 53:59)  

# A-wht, B-blk, C-aian, D-asian, E-hipi, F-othr, G-2plus, H-wht_not_hispanic, I-hispanic  
tables = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')

for(t in tables)
{
  var_list = paste0("B17001", t, "_0", v)
  vars = append(vars, var_list)
}

   
for(year in years)
{
  for(geography in geographies)
  {
    data_one_yr <- get_acs(geography = geography, variables = vars, state = c("VA", "MD", "DC"), 
                             year = year, geometry = FALSE, survey = "acs5", cache_table = TRUE, 
                             output = "wide") %>% 
      transmute(
        geoid = GEOID,
        year = year,
        wht_men_pov_cnt = eval(parse(text = paste(paste0("B17001A_0", 10:16, "E"), collapse="+"))),
        wht_men_cnt = wht_men_pov_cnt + eval(parse(text = paste(paste0("B17001A_0", 39:45, "E"), collapse="+"))),
        wht_men_pov_pct = ifelse(wht_men_cnt == 0, 0, 100*wht_men_pov_cnt/wht_men_cnt),
        wht_women_pov_cnt = eval(parse(text = paste(paste0("B17001A_0", 24:30, "E"), collapse="+"))), 
        wht_women_cnt = wht_women_pov_cnt + eval(parse(text = paste(paste0("B17001A_0", 53:59, "E"), collapse="+"))),
        wht_women_pov_pct =  ifelse(wht_women_cnt == 0, 0, 100*wht_women_pov_cnt/wht_women_cnt),
        
        blk_men_pov_cnt = eval(parse(text = paste(paste0("B17001B_0", 10:16, "E"), collapse="+"))),
        blk_men_cnt = blk_men_pov_cnt + eval(parse(text = paste(paste0("B17001B_0", 39:45, "E"), collapse="+"))),
        blk_men_pov_pct = ifelse(blk_men_cnt == 0, 0, 100*blk_men_pov_cnt/blk_men_cnt),
        blk_women_pov_cnt = eval(parse(text = paste(paste0("B17001B_0", 24:30, "E"), collapse="+"))), 
        blk_women_cnt = blk_women_pov_cnt + eval(parse(text = paste(paste0("B17001B_0", 53:59, "E"), collapse="+"))),
        blk_women_pov_pct = ifelse(blk_women_cnt == 0, 0, 100*blk_women_pov_cnt/blk_women_cnt),
        
        asian_men_pov_cnt = eval(parse(text = paste(paste0("B17001D_0", 10:16, "E"), collapse="+"))),
        asian_men_cnt = asian_men_pov_cnt + eval(parse(text = paste(paste0("B17001D_0", 39:45, "E"), collapse="+"))),
        asian_men_pov_pct = ifelse(asian_men_cnt == 0, 0, 100*asian_men_pov_cnt/asian_men_cnt),
        asian_women_pov_cnt = eval(parse(text = paste(paste0("B17001D_0", 24:30, "E"), collapse="+"))), 
        asian_women_cnt = asian_women_pov_cnt + eval(parse(text = paste(paste0("B17001D_0", 53:59, "E"), collapse="+"))),
        asian_women_pov_pct = ifelse(asian_women_cnt == 0, 0, 100*asian_women_pov_cnt/asian_women_cnt),
        
        othr_men_pov_cnt = eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 10:16, "E"), collapse="+"))),
        othr_men_cnt = othr_men_pov_cnt + eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 39:45, "E"), collapse="+"))),
        othr_men_pov_pct = ifelse(othr_men_cnt == 0, 0, 100*othr_men_pov_cnt /othr_men_cnt),
        othr_women_pov_cnt = eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 24:30, "E"), collapse="+"))), 
        othr_women_cnt = othr_women_pov_cnt + eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 53:59, "E"), collapse="+"))),
        othr_women_pov_pct = ifelse(othr_women_cnt == 0, 0, 100*othr_women_pov_cnt/othr_women_cnt),
        
        wht_nonhis_men_pov_cnt = eval(parse(text = paste(paste0("B17001H_0", 10:16, "E"), collapse="+"))),
        wht_nonhis_men_cnt = wht_nonhis_men_pov_cnt + eval(parse(text = paste(paste0("B17001H_0", 39:45, "E"), collapse="+"))),
        wht_nonhis_men_pov_pct = ifelse(wht_nonhis_men_cnt == 0, 0, 100*wht_nonhis_men_pov_cnt/wht_nonhis_men_cnt),
        wht_nonhis_women_pov_cnt = eval(parse(text = paste(paste0("B17001H_0", 24:30, "E"), collapse="+"))), 
        wht_nonhis_women_cnt = wht_nonhis_women_pov_cnt + eval(parse(text = paste(paste0("B17001H_0", 53:59, "E"), collapse="+"))),
        wht_nonhis_women_pov_pct = ifelse(wht_nonhis_women_cnt == 0, 0, 100*wht_nonhis_women_pov_cnt/wht_nonhis_women_cnt),
        
        his_men_pov_cnt = eval(parse(text = paste(paste0("B17001I_0", 10:16, "E"), collapse="+"))),
        his_men_cnt = his_men_pov_cnt + eval(parse(text = paste(paste0("B17001I_0", 39:45, "E"), collapse="+"))),
        his_men_pov_pct = ifelse(his_men_cnt == 0, 0, 100*his_men_pov_cnt/his_men_cnt),
        his_women_pov_cnt = eval(parse(text = paste(paste0("B17001I_0", 24:30, "E"), collapse="+"))), 
        his_women_cnt = his_women_pov_cnt + eval(parse(text = paste(paste0("B17001I_0", 53:59, "E"), collapse="+"))),
        his_women_pov_pct = ifelse(his_women_cnt == 0, 0, 100*his_women_pov_cnt/his_women_cnt)
      ) %>%
      arrange(geoid)
    
    data_all_yrs <- rbind(data_all_yrs, data_one_yr)
  }
}

rm(data_one_yr)


#
# Missingness check ------------------------------------
#

temp = miss_var_summary(data_all_yrs)  # nothing missing

#
# Wrangle data -------------------------------
#

# Format to long 

df_long <- gather(data_all_yrs, measure, value, wht_men_pov_cnt:his_women_pov_pct)
df_long$moe <- ""


#
# NCR ------------------------------------------
#

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_df <- df_long %>% 
  dplyr::filter(str_detect(geoid, ncr_counties))

# save

write_csv(ncr_df, xzfile("Poverty/data/distribution/ncr_tr_acs_2021_poverty_adults.csv.xz", compression = 9))



