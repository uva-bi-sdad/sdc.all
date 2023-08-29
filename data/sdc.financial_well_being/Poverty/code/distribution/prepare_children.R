#
# ingest ACS data: children below the poverty line
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

# boys_pov = 4:9, boys_no_pov = 33:38, girls_pov = 18:23, girls_no_pov = 47:52
v = str_pad( c(4:9, 33:38, 18:23, 47:52), 2, pad ="0" )  

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
        wht_boys_pov_cnt = eval(parse(text = paste(paste0("B17001A_0", str_pad(4:9, 2, pad="0"), "E"), collapse="+"))),
        wht_boys_cnt = wht_boys_pov_cnt + eval(parse(text = paste(paste0("B17001A_0", 33:38, "E"), collapse="+"))),
        wht_boys_pov_pct = ifelse(wht_boys_cnt == 0, 0, 100*wht_boys_pov_cnt/wht_boys_cnt),
        wht_girls_pov_cnt = eval(parse(text = paste(paste0("B17001A_0", 18:23, "E"), collapse="+"))), 
        wht_girls_cnt = wht_girls_pov_cnt + eval(parse(text = paste(paste0("B17001A_0", 47:52, "E"), collapse="+"))),
        wht_girls_pov_pct =  ifelse(wht_girls_cnt == 0, 0, 100*wht_girls_pov_cnt/wht_girls_cnt),
        
        blk_boys_pov_cnt = eval(parse(text = paste(paste0("B17001B_0", str_pad(4:9, 2, pad="0"), "E"), collapse="+"))),
        blk_boys_cnt = blk_boys_pov_cnt + eval(parse(text = paste(paste0("B17001B_0", 33:38, "E"), collapse="+"))),
        blk_boys_pov_pct = ifelse(blk_boys_cnt == 0, 0, 100*blk_boys_pov_cnt/blk_boys_cnt),
        blk_girls_pov_cnt = eval(parse(text = paste(paste0("B17001B_0", 18:23, "E"), collapse="+"))), 
        blk_girls_cnt = blk_girls_pov_cnt + eval(parse(text = paste(paste0("B17001B_0", 47:52, "E"), collapse="+"))),
        blk_girls_pov_pct = ifelse(blk_girls_cnt == 0, 0, 100*blk_girls_pov_cnt/blk_girls_cnt),
        
        asian_boys_pov_cnt = eval(parse(text = paste(paste0("B17001D_0", str_pad(4:9, 2, pad="0"), "E"), collapse="+"))),
        asian_boys_cnt = asian_boys_pov_cnt + eval(parse(text = paste(paste0("B17001D_0", 33:38, "E"), collapse="+"))),
        asian_boys_pov_pct = ifelse(asian_boys_cnt == 0, 0, 100*asian_boys_pov_cnt/asian_boys_cnt),
        asian_girls_pov_cnt = eval(parse(text = paste(paste0("B17001D_0", 18:23, "E"), collapse="+"))), 
        asian_girls_cnt = asian_girls_pov_cnt + eval(parse(text = paste(paste0("B17001D_0", 47:52, "E"), collapse="+"))),
        asian_girls_pov_pct = ifelse(asian_girls_cnt == 0, 0, 100*asian_girls_pov_cnt/asian_girls_cnt),
        
        othr_boys_pov_cnt = eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", str_pad(4:9, 2, pad="0"), "E"), collapse="+"))),
        othr_boys_cnt = othr_boys_pov_cnt + eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 33:38, "E"), collapse="+"))),
        othr_boys_pov_pct = ifelse(othr_boys_cnt == 0, 0, 100*othr_boys_pov_cnt /othr_boys_cnt),
        othr_girls_pov_cnt = eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 18:23, "E"), collapse="+"))), 
        othr_girls_cnt = othr_girls_pov_cnt + eval(parse(text = paste(paste0("B17001", c(rep("C",7), rep("E",7), rep("F",7), rep("G",7)), "_0", 47:52, "E"), collapse="+"))),
        othr_girls_pov_pct = ifelse(othr_girls_cnt == 0, 0, 100*othr_girls_pov_cnt/othr_girls_cnt),
        
        wht_nonhis_boys_pov_cnt = eval(parse(text = paste(paste0("B17001H_0", str_pad(4:9, 2, pad="0"), "E"), collapse="+"))),
        wht_nonhis_boys_cnt = wht_nonhis_boys_pov_cnt + eval(parse(text = paste(paste0("B17001H_0", 33:38, "E"), collapse="+"))),
        wht_nonhis_boys_pov_pct = ifelse(wht_nonhis_boys_cnt == 0, 0, 100*wht_nonhis_boys_pov_cnt/wht_nonhis_boys_cnt),
        wht_nonhis_girls_pov_cnt = eval(parse(text = paste(paste0("B17001H_0", 18:23, "E"), collapse="+"))), 
        wht_nonhis_girls_cnt = wht_nonhis_girls_pov_cnt + eval(parse(text = paste(paste0("B17001H_0", 47:52, "E"), collapse="+"))),
        wht_nonhis_girls_pov_pct = ifelse(wht_nonhis_girls_cnt == 0, 0, 100*wht_nonhis_girls_pov_cnt/wht_nonhis_girls_cnt),
        
        his_boys_pov_cnt = eval(parse(text = paste(paste0("B17001I_0", str_pad(4:9, 2, pad="0"), "E"), collapse="+"))),
        his_boys_cnt = his_boys_pov_cnt + eval(parse(text = paste(paste0("B17001I_0", 33:38, "E"), collapse="+"))),
        his_boys_pov_pct = ifelse(his_boys_cnt == 0, 0, 100*his_boys_pov_cnt/his_boys_cnt),
        his_girls_pov_cnt = eval(parse(text = paste(paste0("B17001I_0", 18:23, "E"), collapse="+"))), 
        his_girls_cnt = his_girls_pov_cnt + eval(parse(text = paste(paste0("B17001I_0", 47:52, "E"), collapse="+"))),
        his_girls_pov_pct = ifelse(his_girls_cnt == 0, 0, 100*his_girls_pov_cnt/his_girls_cnt)
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

df_long <- gather(data_all_yrs, measure, value, wht_boys_pov_cnt:his_girls_pov_pct)
df_long$moe <- ""


#
# NCR ------------------------------------------
#

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_df <- df_long %>% 
  dplyr::filter(str_detect(geoid, ncr_counties))

# save

write_csv(ncr_df, xzfile("Poverty/data/distribution/ncr_tr_acs_2021_poverty_children.csv.xz", compression = 9))



