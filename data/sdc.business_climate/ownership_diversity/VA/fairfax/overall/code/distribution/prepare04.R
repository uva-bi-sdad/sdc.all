# mergent intellect: compute the diversity metrics at the block group level
#   - key questions at the census block-level:
#         - Do we have small/minority business?
#         - In which industry are they concentrate
#   - key question across census blocks
#         - Where locaated small/minority in some specific industry
#         - How the location map with demographics characteristics



# library
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(reshape2)
library(crosstable)
library(tidyr)
library(scales)


# upload the data ----------------------------------------------------------------------------------
uploadpath = "ownership_diversity/VA/fairfax/overall/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features.csv.xz"))

# keep company features into a single year
mi_features <- mi_fairfax_features %>% 
  select(duns, company_name, minority, small, solo_proprietor, company_type, trade_status, subsidiary, marketing) %>%
  unique()

# set geoid information
geofeatures <- block_groups("VA", "059", 2020) %>% 
  select(geoid=GEOID,
         region_name=NAMELSAD)

geofeatures <- setDT(st_drop_geometry(geofeatures))


# description of business activities in fairfax
tab <- crosstable(mi_features, c(small, solo_proprietor, company_type, trade_status) , by=minority, total='both')


# summary of all business activities in fairfax at the county level -------------------------------------------
temp01 <- mi_fairfax_features %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment), .groups='drop') %>%
  group_by(year,naics_name) %>%
  summarise(emp_year = mean(emp_year),
            numb_companies = length(unique(duns)),
            numb_small_companies = length(unique(duns[small==1])),
            numb_minorityowned_companies = length(unique(duns[minority==1])),
            numb_soloproprietor_companies = length(unique(duns[solo_proprietor==1])),
            emp_industry = sum(employment),
            emp_minority = sum(employment[minority==1]),
            emp_small = sum(employment[small==1]),
            pct_small_companies = round(100*numb_small_companies/numb_companies,2),
            pct_soloproprietor_companies = round(100*numb_soloproprietor_companies/numb_companies,2),
            pct_minorityowned_companies = round(100*numb_minorityowned_companies/numb_companies,2),
            emp_share_industry = round(100*emp_industry/emp_year,2),
            emp_share_small = round(100*emp_small/emp_industry,2),
            emp_share_minority = round(100*emp_minority/emp_industry,2),
            average_emp_per_minority = round(emp_minority/numb_minorityowned_companies,2),
            HHI_all = round(sum( (100*employment/emp_industry)^2, na.rm=T),2),
            HHI_small = round(sum( (100*employment[small==1]/emp_small)^2, na.rm=T),2),
            HHI_minority = round(sum( (100*employment[minority==1]/emp_minority)^2, na.rm=T),2),.groups='keep') %>%
  select(year,naics_name,
         numb_companies,
         numb_small_companies,
         numb_minorityowned_companies,
         emp_industry,
         emp_minority,
         emp_small,
         pct_small_companies,
         pct_soloproprietor_companies,
         pct_minorityowned_companies,
         emp_share_industry,
         emp_share_small,
         emp_share_minority,
         average_emp_per_minority,
         HHI_all,
         HHI_small,
         HHI_minority) %>%
  ungroup()

business_activities01 <- temp01 %>%
  pivot_longer(!c('year','naics_name'), names_to = 'measure01', values_to = 'value') %>%
  mutate(measure = str_replace_all(paste0(naics_name,'_',measure01),' ','_'),
         geoid = '059',
         region_type = 'County',
         region_name = 'Fairfax county',
         measure_type = case_when(
           grepl('numb',measure)==T ~ "count",
           grepl('emp_ind',measure)==T ~ "count",
           grepl('emp_minority',measure)==T ~ "count",
           grepl('emp_small',measure)==T ~ "count",
           grepl('pct',measure)==T ~ "percentage",
           grepl('share',measure)==T ~ "percentage",
           grepl('average',measure)==T ~ "mean",
           grepl('HHI',measure)==T ~ "index"),
         MOE='') %>%
  select(geoid,region_type,region_name,year,measure,value,measure_type,MOE)

# save the data ---------------------------------------------------------------------------------------
savepath = "ownership_diversity/VA/fairfax/overall/data/distribution/"
readr::write_csv(business_activities01, xzfile(paste0(savepath,"va059_ct_mi_20102020_all_business_activities_by_industry.csv.xz"), compression = 9))



# summary of small business activities in fairfax at the county level -------------------------------------------
temp02 <- mi_fairfax_features %>%
  filter(small==1) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment), .groups='drop') %>%
  group_by(year,naics_name) %>%
  summarise(emp_year = mean(emp_year),
            numb_companies = length(unique(duns)),
            numb_minorityowned_companies = length(unique(duns[minority==1])),
            numb_soloproprietor_companies = length(unique(duns[solo_proprietor==1])),
            emp_industry = sum(employment),
            emp_minority = sum(employment[minority==1]),
            pct_soloproprietor_companies = round(100*numb_soloproprietor_companies/numb_companies,2),
            pct_minorityowned_companies = round(100*numb_minorityowned_companies/numb_companies,2),
            emp_share_industry = round(100*emp_industry/emp_year,2),
            emp_share_minority = round(100*emp_minority/emp_industry,2),
            average_emp_per_minority = round(emp_minority/numb_minorityowned_companies,2),
            HHI_all = round(sum( (100*employment/emp_industry)^2, na.rm=T),2),
            HHI_minority = round(sum( (100*employment[minority==1]/emp_minority)^2, na.rm=T),2),.groups='keep') %>%
  select(year,naics_name,
         numb_companies,
         numb_minorityowned_companies,
         emp_industry,
         emp_minority,
         pct_soloproprietor_companies,
         pct_minorityowned_companies,
         emp_share_industry,
         emp_share_minority,
         average_emp_per_minority,
         HHI_all,
         HHI_minority) %>%
  ungroup()

business_activities02 <- temp02 %>%
  pivot_longer(!c('year','naics_name'), names_to = 'measure01', values_to = 'value') %>%
  mutate(measure = str_replace_all(paste0(naics_name,'_',measure01),' ','_'),
         geoid = '059',
         region_type = 'County',
         region_name = 'Fairfax county',
         measure_type = case_when(
           grepl('numb',measure)==T ~ "count",
           grepl('emp_ind',measure)==T ~ "count",
           grepl('emp_minority',measure)==T ~ "count",
           grepl('emp_small',measure)==T ~ "count",
           grepl('pct',measure)==T ~ "percentage",
           grepl('share',measure)==T ~ "percentage",
           grepl('average',measure)==T ~ "mean",
           grepl('HHI',measure)==T ~ "index"),
         MOE='') %>%
  select(geoid,region_type,region_name,year,measure,value,measure_type,MOE)

# save the data ---------------------------------------------------------------------------------------
savepath = "ownership_diversity/VA/fairfax/overall/data/distribution/"
readr::write_csv(business_activities02, xzfile(paste0(savepath,"va059_ct_mi_20102020_small_business_activities_by_industry.csv.xz"), compression = 9))



# get the block group name
temp_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(geoid=GEOID, region_name=NAMELSAD, geometry) %>%
  mutate(year_census=2010)

temp_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(geoid=GEOID, region_name=NAMELSAD, geometry) %>%
  mutate(year_census=2020)

fairfax_bg <- setDT(st_drop_geometry(rbind(temp_bg2010,temp_bg2020)))


# summary of all business activities in fairfax at the blog group level -------------------------------------------
temp03 <- mi_fairfax_features %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment),
         emp_year_small=sum(employment[small==1]),
         emp_year_minority=sum(employment[minority==1])) %>%
  group_by(year,geoid) %>%
  mutate(emp_geo=sum(employment),
         emp_geo_small=sum(employment[small==1]),
         emp_geo_minority=sum(employment[minority==1])) %>%
  group_by(year,naics_name) %>%
  mutate(emp_all_ind=sum(employment),
         emp_all_ind_small=sum(employment[small==1]),
         emp_all_ind_minority=sum(employment[minority==1])) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(year_census=if_else(year<2020,2010,2020),
            emp_year = mean(emp_year),
            emp_year_small = mean(emp_year_small),
            emp_year_minority = mean(emp_year_minority),
            emp_geo = mean(emp_geo),
            emp_geo_small = mean(emp_geo_small),
            emp_geo_minority = mean(emp_geo_minority),
            emp_all_ind = mean(emp_all_ind),
            emp_all_ind_small = mean(emp_all_ind_small),
            emp_all_ind_minority = mean(emp_all_ind_minority),
            numb_companies = length(unique(duns)),
            numb_small_companies = length(unique(duns[small==1])),
            numb_minorityowned_companies = length(unique(duns[minority==1])),
            numb_soloproprietor_companies = length(unique(duns[solo_proprietor==1])),
            emp_industry = sum(employment),
            emp_small = sum(employment[small==1]),
            emp_minority = sum(employment[minority==1]),
            pct_small_companies = round(100*numb_small_companies/numb_companies,2),
            pct_soloproprietor_companies = round(100*numb_soloproprietor_companies/numb_companies,2),
            pct_minorityowned_companies = round(100*numb_minorityowned_companies/numb_companies,2),
            emp_share_industry = round(100*emp_industry/emp_geo,2),
            emp_share_small = round(100*emp_small/emp_industry,2),
            emp_share_minority = round(100*emp_minority/emp_industry,2),
            average_emp_per_minority = round(emp_minority/numb_minorityowned_companies,2),
            HHI_all = round(sum( (100*employment/emp_industry)^2, na.rm=T),2),
            HHI_small = round(sum( (100*employment[small==1]/emp_small)^2, na.rm=T),2),
            HHI_minority = round(sum( (100*employment[minority==1]/emp_minority)^2, na.rm=T),2),
            LQ_all= round((emp_industry/emp_geo)/(emp_all_ind/emp_year),2), 
            LQ_small= round((emp_small/emp_geo_small)/(emp_all_ind_small/emp_year_small),2),
            LQ_minority= round((emp_minority/emp_geo_minority)/(emp_all_ind_minority/emp_year_minority),2), .groups='keep') %>%
  select(year,geoid,naics_name,year_census,
         numb_companies,
         numb_small_companies,
         numb_minorityowned_companies,
         numb_soloproprietor_companies,
         emp_industry,
         emp_minority,
         emp_small,
         pct_small_companies,
         pct_soloproprietor_companies,
         pct_minorityowned_companies,
         emp_share_industry,
         emp_share_small,
         emp_share_minority,
         average_emp_per_minority,
         HHI_all,
         HHI_small,
         HHI_minority, 
         LQ_all,
         LQ_small,
         LQ_minority) %>%
  ungroup()

temp03 <- merge(temp03,fairfax_bg, by.x=c('geoid','year_census'), by.y=c('geoid','year_census'), all.x=T)%>%
  select(-year_census)

business_activities03 <- temp03 %>%
  pivot_longer(!c('year','geoid','region_name','naics_name'), names_to = 'measure01', values_to = 'value') %>%
  mutate(measure = str_replace_all(paste0(naics_name,'_',measure01),' ','_'),
         region_type = 'Block group',
         measure_type = case_when(
           grepl('numb',measure)==T ~ "count",
           grepl('emp_ind',measure)==T ~ "count",
           grepl('emp_minority',measure)==T ~ "count",
           grepl('emp_small',measure)==T ~ "count",
           grepl('pct',measure)==T ~ "percentage",
           grepl('share',measure)==T ~ "percentage",
           grepl('average',measure)==T ~ "mean",
           grepl('LQ',measure)==T ~ "quotient",
           grepl('HHI',measure)==T ~ "index"),
         MOE='') %>%
  select(geoid,region_type,region_name,year,measure,value,measure_type,MOE)

savepath = "ownership_diversity/VA/fairfax/overall/data/distribution/"
readr::write_csv(business_activities03, xzfile(paste0(savepath,"va059_bg_mi_20102020_all_business_activities_by_industry.csv.xz"), compression = 9))




# summary of small business activities in fairfax at the blog group level -------------------------------------------
temp04 <- mi_fairfax_features %>%
  filter(small==1) %>%
  group_by(year) %>%
  mutate(emp_year=sum(employment),
         emp_year_small=sum(employment[small==1]),
         emp_year_minority=sum(employment[minority==1])) %>%
  group_by(year,geoid) %>%
  mutate(emp_geo=sum(employment),
         emp_geo_small=sum(employment[small==1]),
         emp_geo_minority=sum(employment[minority==1])) %>%
  group_by(year,naics_name) %>%
  mutate(emp_all_ind=sum(employment),
         emp_all_ind_small=sum(employment[small==1]),
         emp_all_ind_minority=sum(employment[minority==1])) %>%
  group_by(year,geoid,naics_name) %>%
  summarise(year_census=if_else(year<2020,2010,2020),
            emp_year = mean(emp_year),
            emp_year_small = mean(emp_year_small),
            emp_year_minority = mean(emp_year_minority),
            emp_geo = mean(emp_geo),
            emp_geo_small = mean(emp_geo_small),
            emp_geo_minority = mean(emp_geo_minority),
            emp_all_ind = mean(emp_all_ind),
            emp_all_ind_small = mean(emp_all_ind_small),
            emp_all_ind_minority = mean(emp_all_ind_minority),
            numb_companies = length(unique(duns)),
            numb_small_companies = length(unique(duns[small==1])),
            numb_minorityowned_companies = length(unique(duns[minority==1])),
            numb_soloproprietor_companies = length(unique(duns[solo_proprietor==1])),
            emp_industry = sum(employment),
            emp_small = sum(employment[small==1]),
            emp_minority = sum(employment[minority==1]),
            pct_small_companies = round(100*numb_small_companies/numb_companies,2),
            pct_soloproprietor_companies = round(100*numb_soloproprietor_companies/numb_companies,2),
            pct_minorityowned_companies = round(100*numb_minorityowned_companies/numb_companies,2),
            emp_share_industry = round(100*emp_industry/emp_geo,2),
            emp_share_small = round(100*emp_small/emp_industry,2),
            emp_share_minority = round(100*emp_minority/emp_industry,2),
            average_emp_per_minority = round(emp_minority/numb_minorityowned_companies,2),
            HHI_all = round(sum( (100*employment/emp_industry)^2, na.rm=T),2),
            HHI_small = round(sum( (100*employment[small==1]/emp_small)^2, na.rm=T),2),
            HHI_minority = round(sum( (100*employment[minority==1]/emp_minority)^2, na.rm=T),2),
            LQ_all= round((emp_industry/emp_geo)/(emp_all_ind/emp_year),2), 
            LQ_small= round((emp_small/emp_geo_small)/(emp_all_ind_small/emp_year_small),2),
            LQ_minority= round((emp_minority/emp_geo_minority)/(emp_all_ind_minority/emp_year_minority),2), .groups='keep') %>%
  select(year,geoid,naics_name,year_census,
         numb_companies,
         numb_small_companies,
         numb_minorityowned_companies,
         numb_soloproprietor_companies,
         emp_industry,
         emp_minority,
         emp_small,
         pct_small_companies,
         pct_soloproprietor_companies,
         pct_minorityowned_companies,
         emp_share_industry,
         emp_share_small,
         emp_share_minority,
         average_emp_per_minority,
         HHI_all,
         HHI_small,
         HHI_minority, 
         LQ_all,
         LQ_small,
         LQ_minority) %>%
  ungroup()

temp04 <- merge(temp04,fairfax_bg, by.x=c('geoid','year_census'), by.y=c('geoid','year_census'), all.x=T) %>%
  select(-year_census)

business_activities04 <- temp04 %>%
  pivot_longer(!c('year','geoid','region_name','naics_name'), names_to = 'measure01', values_to = 'value') %>%
  mutate(measure = str_replace_all(paste0(naics_name,'_',measure01),' ','_'),
         region_type = 'Block group',
         measure_type = case_when(
           grepl('numb',measure)==T ~ "count",
           grepl('emp_ind',measure)==T ~ "count",
           grepl('emp_minority',measure)==T ~ "count",
           grepl('emp_small',measure)==T ~ "count",
           grepl('pct',measure)==T ~ "percentage",
           grepl('share',measure)==T ~ "percentage",
           grepl('average',measure)==T ~ "mean",
           grepl('LQ',measure)==T ~ "quotient",
           grepl('HHI',measure)==T ~ "index"),
         MOE='') %>%
  select(geoid,region_type,region_name,year,measure,value,measure_type,MOE)

savepath = "ownership_diversity/VA/fairfax/overall/data/distribution/"
readr::write_csv(business_activities04, xzfile(paste0(savepath,"va059_bg_mi_20102020_small_business_activities_by_industry.csv.xz"), compression = 9))











