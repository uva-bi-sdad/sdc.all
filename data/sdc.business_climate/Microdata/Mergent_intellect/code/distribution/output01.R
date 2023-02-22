# Compute at the county level:
#       - The industry activities of all businesses
#       - The industry activities of minority owned businesses
#       - Business dynamism in fairfax


# library
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(tidyverse)
library(data.table)
library(ggplot2)
library(tidygeocoder)
library(cowplot)
library(ggpubr)


# Load the data ------------------------------------------------------

# upload mergent intellect
uploadpath = "ownership_diversity/VA/fairfax/overall/data/distribution/"
mict <-  read_csv(paste0(uploadpath,"va059_ct_mi_20102020_all_business_activities_by_industry.csv.xz"))

# from measure, separate the industry name and the metrics computed
metrics <- c('_numb_companies',
             '_numb_small_companies',
             '_numb_minorityowned_companies',
             '_emp_industry',
             '_emp_minority',
             '_emp_small',
             '_pct_small_companies',
             '_pct_soloproprietor_companies',
             '_pct_minorityowned_companies',
             '_emp_share_industry',
             '_emp_share_small',
             '_emp_share_minority',
             '_average_emp_per_minority',
             '_HHI_all',
             '_HHI_small',
             '_HHI_minority')

mict <- mict %>%
  mutate(industry=str_remove_all(measure, paste(metrics, collapse = "|")),
         measure=str_remove_all(measure, paste(unique(industry), collapse = "|")),
         industry=str_replace_all(industry,'_',' '),
         measure=str_replace(measure, '_', ''),
         industry01=case_when(
           industry=="Agriculture, Forestry, Fishing and Hunting" ~ "Agriculture, Forestry, Fishing ...",
           industry=="Mining, Quarrying, and Oil and Gas Extraction" ~ "Mining, Quarrying, and Oil ...",
           industry=="Utilities" ~ "Utilities",
           industry=="Construction" ~ "Construction",
           industry=="Manufacturing" ~ "Manufacturing",
           industry=="Wholesale Trade" ~ "Wholesale Trade",
           industry=="Retail Trade" ~ "Retail Trade",
           industry=="Transportation and Warehousing" ~ "Transportation and Warehousing",
           industry=="Information" ~ "Information",
           industry=="Finance and Insurance" ~ "Finance and Insurance",
           industry=="Real Estate and Rental and Leasing" ~ "Real Estate and Rental and Leasing",
           industry=="Professional, Scientific, and Technical Services" ~ "Professional, Scientific, and Technical Services",
           industry=="Management of Companies and Enterprises" ~ "Management of Companies and Enterprises",
           industry=="Administrative and Support and Waste Management and Remediation Services" ~ "Administrative and Support ...",
           industry=="Educational Services" ~ "Educational Services",
           industry=="Health Care and Social Assistance" ~ "Health Care and Social Assistance",
           industry=="Arts, Entertainment, and Recreation" ~ "Arts, Entertainment, and Recreation",
           industry=="Accommodation and Food Services" ~ "Accommodation and Food Services",
           industry=="Other Services (except Public Administration)" ~ "Other Services (except Public Administration)",
           industry=="Public Administration" ~ "Public Administration",
           industry=="Nonclassifiable Establishments" ~ "Nonclassifiable Establishments"))


# upload the geometry
temp_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(geoid=GEOID, region_name=NAMELSAD, geometry) %>%
  mutate(year_census=2010)

temp_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(geoid=GEOID, region_name=NAMELSAD, geometry) %>%
  mutate(year_census=2020)

fairfax_bg <- setDT(st_drop_geometry(rbind(temp_bg2010,temp_bg2020)))

# changes in industry activities in fairfax
mictdyn <- mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter(year %in% c(2011,2020))



# business fairfax economic in a year  ------------------------------------------------
time = 2020

# all businesses in fairfax
plt1a <-  mict %>%
  filter((year==time)&(measure=='numb_companies')) %>%
  mutate(industry01 = fct_reorder(industry01, value)) %>%  
  ggplot(aes(industry01, value)) +   
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Industry", y="Number of companies in 2020") +
  coord_flip() +
  theme(axis.text.y = element_text(size=10))
plt1a

plt1b <-  mict %>%
  filter((year==time)&(measure %in% c('emp_industry','numb_companies') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, numb_companies)) %>%  
  ggplot(aes(industry01, emp_industry)) +   
  geom_bar(stat = "identity", fill="#E69F00")+
  labs(x = "Industry", y="Total employment in 2020") +
  coord_flip() +
  theme(axis.text.y = element_text(size=10))
plt1b

ggarrange(plt1a, 
          plt1b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))


# small businesses in fairfax
plt2a <-  mict %>%
  filter((year==time)&(measure=='numb_small_companies')) %>%
  mutate(industry01 = fct_reorder(industry01, value)) %>%  
  ggplot(aes(industry01, value)) +   
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Industry", y="Number of small companies in 2020") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt2a

plt2b <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('emp_share_small','numb_small_companies') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, numb_small_companies)) %>%  
  ggplot(aes(industry01, emp_share_small)) +   
  geom_bar(stat = "identity", fill="#E69F00")+
  labs(x = "Industry", y="Employment share by small companies in 2020") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt2b

ggarrange(plt1b, 
          plt2b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))



# Changes in the number of companies and employment by industry

# table
mictdyntable01 <- mictdyn %>%
  filter( measure=='numb_companies' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(numb_companies_2020 - numb_companies_2011)/numb_companies_2011, 2),
         industry01 = fct_reorder(industry01, growth)) 

mictdyntable02 <- mictdyn %>%
  filter( measure %in% c('emp_industry','numb_companies') ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth1=round(100*(numb_companies_2020 - numb_companies_2011)/numb_companies_2011, 2),
         growth2=round(100*(emp_industry_2020 - emp_industry_2011)/emp_industry_2011, 2),
         industry01 = fct_reorder(industry01, growth1))



plt4a <-  mictdyn %>%
  filter( measure=='numb_companies' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(numb_companies_2020 - numb_companies_2011)/numb_companies_2011, 2),
         industry01 = fct_reorder(industry01, growth)) %>%
  filter( !(industry01=='Nonclassifiable Establishments') ) %>%
  ggplot(aes(industry01, growth)) +   
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Industry", y="Growth rate in the number of companies (%)") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt4a

plt4b <-  mictdyn %>%
  filter( measure %in% c('emp_industry','numb_companies') ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth1=round(100*(numb_companies_2020 - numb_companies_2011)/numb_companies_2011, 2),
         growth2=round(100*(emp_industry_2020 - emp_industry_2011)/emp_industry_2011, 2),
         industry01 = fct_reorder(industry01, growth1)) %>%
  filter( !(industry01=='Nonclassifiable Establishments') ) %>%
  ggplot(aes(industry01, growth2)) +   
  geom_bar(stat = "identity", fill="#E69F00")+
  labs(x = "Industry", y="Growth rate in employment (%)") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt4b

ggarrange(plt4a, 
          plt4b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))



# industry concentration

HII_all <- mictdyn %>%
  filter( measure=='HHI_all' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(HHI_all_2020 - HHI_all_2011)/HHI_all_2011, 2),
         industry01 = fct_reorder(industry01, HHI_all_2020))


plt3a <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('HHI_all','HHI_minority') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, HHI_all)) %>%
  ggplot(aes(industry01, HHI_all)) +   
  geom_bar(stat = "identity", fill="red")+
  labs(x = "Industry", y="Herfindahl-Hirschman Index (HHI) ") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt3a

plt6a <-  mictdyn %>%
  filter( measure=='HHI_all' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(HHI_all_2020 - HHI_all_2011)/HHI_all_2011, 2),
         industry01 = fct_reorder(industry01, HHI_all_2020)) %>%
  filter( !(industry01 %in% c('Nonclassifiable Establishments') ) ) %>%
  ggplot(aes(industry01, growth)) +   
  geom_bar(stat = "identity", fill="red")+
  labs(x = "Industry", y="Changes in HHI (in %)") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt6a

ggarrange(plt3a, 
          plt6a + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))




# minority owned businesses --------------------------------------------------------------------

trend_table <- mict %>%
  filter(measure %in% c('numb_companies','numb_minorityowned_companies')) %>%
  filter(year>2010) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(nonminority_comp=numb_companies-numb_minorityowned_companies) %>%
  group_by(year) %>%
  summarise(nonminority_comp=sum(nonminority_comp),
            minority=sum(numb_minorityowned_companies),
            pct_minority=minority/(nonminority_comp+minority))
            
trendfig <- trend00 %>%
  pivot_longer(!year, names_to = "measure", values_to = "value") %>%
  mutate(measure=if_else(measure=='minority','minority owned','non minority owned')) %>%
  ggplot(aes(factor(year), value, fill=measure)) +   
  geom_bar(stat = "identity", colour="black")+
  scale_fill_manual(values=c("#920000","#009999")) +
  labs(x = "year", y="Number companies listed in Fairfax county") +
  theme(axis.text.y = element_text(size=10))+
  text(pct_minority)
trendfig     

# trends inf the number of minority business
trend <- mict %>%
  filter(measure %in% c('numb_companies','numb_minorityowned_companies')) %>%
  filter(year>2010) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(nonminority_comp=numb_companies-numb_minorityowned_companies) %>%
  group_by(year) %>%
  summarise(nonminority_comp=sum(nonminority_comp),
            minority=sum(numb_minorityowned_companies),
            pct_minority=minority/(nonminority_comp+minority)) %>%
  pivot_longer(!year, names_to = "measure", values_to = "value") %>%
  mutate(measure=if_else(measure=='minority','minority owned','non minority owned')) %>%
  ggplot(aes(factor(year), value, fill=measure)) +   
  geom_bar(stat = "identity", colour="black")+
  scale_fill_manual(values=c("#920000","#009999")) +
  labs(x = "year", y="Number companies listed in Fairfax county") +
  theme(axis.text.y = element_text(size=10))
trend


# minority owned business
plt3a <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('pct_minorityowned_companies','numb_minorityowned_companies') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, numb_minorityowned_companies)) %>%
  ggplot(aes(industry01, numb_minorityowned_companies)) +   
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Industry", y="Number of minority-owned companies in 2020") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt3a

plt3b <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('pct_minorityowned_companies','numb_minorityowned_companies') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, numb_minorityowned_companies)) %>%    
  ggplot(aes(industry01, pct_minorityowned_companies)) +   
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Industry", y="Percentage of minority-owned companies in 2020") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt3b

ggarrange(plt3a, 
          plt3b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))






# employment by minority owned business
plt3c <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('emp_share_minority','emp_minority') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, emp_minority)) %>%
  ggplot(aes(industry01, emp_minority)) +   
  geom_bar(stat = "identity", fill="#E69F00")+
  labs(x = "Industry", y="Employment by minority-owned \n companies in 2020") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt3c

plt3d <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('emp_share_minority','emp_minority') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, emp_minority)) %>%    
  ggplot(aes(industry01, emp_share_minority)) +   
  geom_bar(stat = "identity", fill="#E69F00")+
  labs(x = "Industry", y="Employment share of minority-owned \n companies in 2020") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt3d

ggarrange(plt3c, 
          plt3d + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))



# industry changes for minority-owned businesses
growthtable <- mictdyn %>%
  filter( measure %in% c('emp_minority','numb_minorityowned_companies') ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth1=round(100*(numb_minorityowned_companies_2020 - numb_minorityowned_companies_2011)/numb_minorityowned_companies_2011, 2),
         growth2=round(100*(emp_minority_2020 - emp_minority_2011)/emp_minority_2011, 2),
         industry01 = fct_reorder(industry01, growth1))


plt5a <-  mictdyn %>%
  filter( measure=='numb_minorityowned_companies' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(numb_minorityowned_companies_2020 - numb_minorityowned_companies_2011)/numb_minorityowned_companies_2011, 2),
         industry01 = fct_reorder(industry01, growth)) %>%
  filter( !(industry01 %in% c('Nonclassifiable Establishments','Public Administration') ) ) %>%
  ggplot(aes(industry01, growth)) +   
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Industry", y="Growth rate in the number of companies (%)") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt5a

plt5b <-  mictdyn %>%
  filter( measure %in% c('emp_minority','numb_minorityowned_companies') ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth1=round(100*(numb_minorityowned_companies_2020 - numb_minorityowned_companies_2011)/numb_minorityowned_companies_2011, 2),
         growth2=round(100*(emp_minority_2020 - emp_minority_2011)/emp_minority_2011, 2),
         industry01 = fct_reorder(industry01, growth1)) %>%
  filter( !(industry01 %in% c('Nonclassifiable Establishments','Public Administration') ) ) %>%
  ggplot(aes(industry01, growth2)) +   
  geom_bar(stat = "identity", fill="#E69F00")+
  labs(x = "Industry", y="Growth rate in employment (%)") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt5b

ggarrange(plt5a, 
          plt5b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))



# businesses concentration in fairfax
HHIminority <- mictdyn %>%
  filter( measure=='HHI_minority' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(HHI_minority_2020 - HHI_minority_2011)/HHI_minority_2011, 2),
         industry01 = fct_reorder(industry01, HHI_minority_2020))



plt3b <-  mict %>%
  select(geoid,region_type,region_name,year,measure,value,industry01) %>%
  filter((year==time)&(measure %in% c('HHI_all','HHI_minority') )) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(industry01 = fct_reorder(industry01, HHI_minority)) %>%
  ggplot(aes(industry01, HHI_minority)) +   
  geom_bar(stat = "identity", fill="red")+
  labs(x = "Industry", y="Herfindahl-Hirschman Index") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt3b

plt6b <-  mictdyn %>%
  filter( measure=='HHI_minority' ) %>%
  pivot_wider(names_from=c('measure','year'), values_from=value) %>%
  mutate(growth=round(100*(HHI_minority_2020 - HHI_minority_2011)/HHI_minority_2011, 2),
         industry01 = fct_reorder(industry01, HHI_minority_2020)) %>%
  filter( !(industry01 %in% c('Nonclassifiable Establishments','Public Administration') ) ) %>%
  ggplot(aes(industry01, growth)) +   
  geom_bar(stat = "identity", fill="red")+
  labs(x = "Industry", y="Changes in HHI (in %)") +
  coord_flip()+
  theme(axis.text.y = element_text(size=10))
plt6b

ggarrange(plt3b, 
          plt6b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ), 
          nrow = 1,
          widths=c(2,1))





