# download all lodes data for virginia

# library
library(dplyr)
library(lehdr)
library(tidyr)
library(sf)
library(tigris)

# Extract LODES WAC at the block group level
savepath = "Employment/Worker_diversity/data/distribution/"

# get the lodes data for the whole NCR and virginia ----------------------------------------------------------------------------
selected_year <- 2010:2019
lodes_va_bg <- grab_lodes(state = 'va', 
                          year = selected_year, 
                          lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                          job_type = "JT00",        # all jobs 
                          segment = "S000", 
                          state_part = "main",
                          agg_geo = "bg" ) %>% select(year, state, geoid=w_bg, starts_with('CR'))

lodes_md_bg <- grab_lodes(state = 'md', 
                          year = selected_year, 
                          lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                          job_type = "JT00",        # all jobs 
                          segment = "S000", 
                          state_part = "main",
                          agg_geo = "bg" ) %>% select(year, state, geoid=w_bg, starts_with('CR'))

lodes_dc_bg <- grab_lodes(state = 'dc', 
                          year = selected_year, 
                          lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                          job_type = "JT00",        # all jobs 
                          segment = "S000", 
                          state_part = "main",
                          agg_geo = "bg" ) %>% select(year, state, geoid=w_bg, starts_with('CR'))

lodes_bg0 <- rbind(lodes_va_bg,lodes_md_bg,lodes_dc_bg)



# compute lodes estimates at the ncr (block group, tract, county) -----------------------------------------------------------------

# filter at the ncr region
ncr_county <- c("51013", "51059", "51107", "51510", "51600", "51153", "51683", "51685", "51610", "11001", "24031", "24033", "24017", "24021")
lodes_bg <- lodes_bg0 %>%
  mutate(geoid_cnty=substr(geoid,1,5)) %>%
  filter(geoid_cnty %in% ncr_county) %>%
  select(-geoid_cnty)

# reshape the data
lodes_bg_lg <- lodes_bg %>% 
  pivot_longer(cols = starts_with("CR"), names_to = "race_id", values_to = "jobs") %>%
  mutate(region_type='block group',
         race=case_when(
           race_id=='CR01' ~ "White_alone",
           race_id=='CR02' ~ "Black_alone",
           race_id=='CR03' ~ "American_indian&alaska_native_alone",
           race_id=='CR04' ~ "Asian_alone",
           race_id=='CR05' ~ "Hawaii&pacific_islander_alone",
           race_id=='CR07' ~ "Two_or_more_races")) %>%
  select(state,year,geoid,region_type,race,jobs) %>%
  mutate(minority=if_else(race=='White_alone','Nonminority','Minority')) %>%
  group_by(geoid,region_type,year,minority) %>%
  summarise(value=sum(jobs, na.rm=T))


# estimate at the block group level -----------------------------------
temp_bg <- lodes_bg_lg %>%
  mutate(measure_type='count',
         measure=paste0(minority,'_employment'),
         moe=NA) %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save the data
#readr::write_csv(temp, xzfile(paste0(savepath,"ncr_bg_lodes_",min(employment_diversity$year),max(employment_diversity$year),"_employment_by_minority_workers.csv.xz"), compression = 9))


# estimate at the tract level and save -------------------
temp_tr <- temp_bg %>%
  select(geoid,year,measure,value) %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,measure) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(measure_type='count',
         moe=NA)

#readr::write_csv(temp1, xzfile(paste0(savepath,"ncr_tr_lodes_",min(temp1$year),max(temp1$year),"_employment_by_minority_workers.csv.xz"), compression = 9))


# estimate at the county level and save -------------------
temp_ct <- temp_bg %>%
  select(geoid,year,measure,value) %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,measure) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(measure_type='count',
         moe=NA)

# save all geo-levels
temp <- rbind(temp_bg, temp_tr, temp_ct)
readr::write_csv(temp, xzfile(paste0(savepath,"ncr_cttrbg_lodes_",min(temp$year),'_',max(temp$year),"_employment_by_minority_workers.csv.xz"), compression = 9))






# filter lodes estimates to fairfax ----------------------------------------------------------------------------------------------------------

# filter to the fairfax county
fairfax_county <- "51059"
lodes_bg <- lodes_bg0 %>%
  mutate(geoid_cnty=substr(geoid,1,5)) %>%
  filter(geoid_cnty %in% fairfax_county) %>%
  select(-geoid_cnty)

# reshape the data
lodes_bg_lg <- lodes_bg %>% 
  pivot_longer(cols = starts_with("CR"), names_to = "race_id", values_to = "jobs") %>%
  mutate(region_type='block group',
         race=case_when(
           race_id=='CR01' ~ "White_alone",
           race_id=='CR02' ~ "Black_alone",
           race_id=='CR03' ~ "American_indian&alaska_native_alone",
           race_id=='CR04' ~ "Asian_alone",
           race_id=='CR05' ~ "Hawaii&pacific_islander_alone",
           race_id=='CR07' ~ "Two_or_more_races")) %>%
  select(state,year,geoid,region_type,race,jobs) %>%
  mutate(minority=if_else(race=='White_alone','Nonminority','Minority')) %>%
  group_by(geoid,region_type,year,minority) %>%
  summarise(value=sum(jobs, na.rm=T))


# estimate at the block group level -----------------------------------
temp_bg <- lodes_bg_lg %>%
  mutate(measure_type='count',
         measure=paste0(minority,'_employment'),
         moe=NA) %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save the data
#readr::write_csv(temp, xzfile(paste0(savepath,"va059_bg_lodes_",min(employment_diversity$year),max(employment_diversity$year),"_employment_by_minority_workers.csv.xz"), compression = 9))


# estimate at the tract level and save -------------------
temp_tr <- temp_bg %>%
  select(geoid,year,measure,value) %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,measure) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(measure_type='count',
         moe=NA)

#readr::write_csv(temp1, xzfile(paste0(savepath,"va059_tr_lodes_",min(temp1$year),max(temp1$year),"_employment_by_minority_workers.csv.xz"), compression = 9))


# estimate at the county level and save -------------------
temp_ct <- temp_bg %>%
  select(geoid,year,measure,value) %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,measure) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(measure_type='count',
         moe=NA)

# save all geo-levels
temp <- rbind(temp_bg, temp_tr, temp_ct)
readr::write_csv(temp, xzfile(paste0(savepath,"va059_cttrbg_lodes_",min(temp$year),'_',max(temp$year),"_employment_by_minority_workers.csv.xz"), compression = 9))







# filter lodes to specific counties: Richmond city, Henrico county, Chesterfield county ---------------------------------------------------------------------
va_county <- c("51159", "51087", "51041")
lodes_bg <- lodes_bg0 %>%
  mutate(geoid_cnty=substr(geoid,1,5)) %>%
  filter(geoid_cnty %in% va_county) %>%
  select(-geoid_cnty)

# reshape the data
lodes_bg_lg <- lodes_bg %>% 
  pivot_longer(cols = starts_with("CR"), names_to = "race_id", values_to = "jobs") %>%
  mutate(region_type='block group',
         race=case_when(
           race_id=='CR01' ~ "White_alone",
           race_id=='CR02' ~ "Black_alone",
           race_id=='CR03' ~ "American_indian&alaska_native_alone",
           race_id=='CR04' ~ "Asian_alone",
           race_id=='CR05' ~ "Hawaii&pacific_islander_alone",
           race_id=='CR07' ~ "Two_or_more_races")) %>%
  select(state,year,geoid,region_type,race,jobs) %>%
  mutate(minority=if_else(race=='White_alone','Nonminority','Minority')) %>%
  group_by(geoid,region_type,year,minority) %>%
  summarise(value=sum(jobs, na.rm=T))


# estimate at the block group level -----------------------------------
temp_bg <- lodes_bg_lg %>%
  mutate(measure_type='count',
         measure=paste0(minority,'_employment'),
         moe=NA) %>%
  ungroup() %>%
  select(geoid,year,measure,value,measure_type,moe)

# save the data
#readr::write_csv(temp, xzfile(paste0(savepath,"va_bg_lodes_",min(employment_diversity$year),max(employment_diversity$year),"_employment_by_minority_workers.csv.xz"), compression = 9))


# estimate at the tract level and save -------------------
temp_tr <- temp_bg %>%
  select(geoid,year,measure,value) %>%
  mutate(geoid=substr(geoid,1,11)) %>%
  group_by(geoid,year,measure) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(measure_type='count',
         moe=NA)

#readr::write_csv(temp1, xzfile(paste0(savepath,"va_tr_lodes_",min(temp1$year),max(temp1$year),"_employment_by_minority_workers.csv.xz"), compression = 9))


# estimate at the county level and save -------------------
temp_ct <- temp_bg %>%
  select(geoid,year,measure,value) %>%
  mutate(geoid=substr(geoid,1,5)) %>%
  group_by(geoid,year,measure) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(measure_type='count',
         moe=NA)

# save all geo-levels
temp <- rbind(temp_bg, temp_tr, temp_ct)
readr::write_csv(temp, xzfile(paste0(savepath,"rva_cttrbg_lodes_",min(temp$year),'_',max(temp$year),"_employment_by_minority_workers.csv.xz"), compression = 9))
