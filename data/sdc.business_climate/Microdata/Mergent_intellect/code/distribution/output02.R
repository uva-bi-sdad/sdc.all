# Compute at the block group level:
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
mibg <-  read_csv(paste0(uploadpath,"va059_bg_mi_20102020_all_business_activities_by_industry.csv.xz"))


# from measure, separate the industry name and the metrics computed
metrics <- c('_numb_companies',
             '_numb_small_companies',
             '_numb_minorityowned_companies',
             '_numb_soloproprietor_companies',
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
             '_HHI_minority', 
             '_LQ_all',
             '_LQ_small',
             '_LQ_minority')

mibg <- mibg %>%
  mutate(industry=str_remove_all(measure, paste(metrics, collapse = "|")),
         measure=str_remove_all(measure, paste(unique(industry), collapse = "|")),
         industry=str_replace_all(industry,'_',' '),
         measure=str_replace(measure, '_', ''))


# create the census year using year
mibg <- mibg %>%
  mutate(year_census=if_else(year<2020,2010,2020))


# upload the geometry
temp_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(geoid=GEOID, geometry) %>%
  mutate(year_census=2010)

temp_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(geoid=GEOID, geometry) %>%
  mutate(year_census=2020)

fairfax_bgsf <- rbind(temp_bg2010,temp_bg2020)
fairfax_bg <- setDT(st_drop_geometry(fairfax_bgsf))

mibg_sf <- sf::st_as_sf(merge(mibg, fairfax_bgsf, by.x = c('geoid','year_census'), by.y = c('geoid','year_census'), all.x = TRUE))


# business fairfax economic in a year  ------------------------------------------------

# industry specific geographic distribution
indus = 'Agriculture, Forestry, Fishing and Hunting'
indus = 'Real Estate and Rental and Leasing'
time = 2020

# all businesses in the industry
plt2a <-  mibg_sf %>%
  filter((year==time)&(measure=='numb_companies')&(industry==indus)) %>%
  group_by(geoid) %>%
  summarize(value=sum(value)) %>%
  ggplot() + 
  geom_sf(aes(fill = value)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Count", title = "Number of companies")
plt2a


plt2b <-  mibg_sf %>%
  filter((year==time)&(measure=='emp_industry')&(industry==indus)) %>%
  group_by(geoid) %>%
  summarize(value=sum(value)) %>%
  ggplot() + 
  geom_sf(aes(fill = value)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Employment", title = "Total employment by companies")
plt2b


plt2c <-  mibg_sf %>%
  filter((year==time)&(measure=='LQ_all')&(industry==indus)) %>%
  group_by(geoid) %>%
  summarize(value=sum(value)) %>%
  ggplot() + 
  geom_sf(aes(fill = value)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "LQ", title = "Location quotient")
plt2c

ggarrange(plt2a, 
          plt2b, 
          plt2c,
          nrow = 1)



# changes at the census block group between 2011 abd 2019
plt2d <-  mibg_sf %>%
  filter( (year %in% c(2011,2019))&(measure=='numb_companies')&(industry==indus) ) %>%
  mutate(dum=if_else(year==2019,1,-1)) %>%
  group_by(geoid,year) %>%
  summarize(growth=sum(log(value)*dum)-1) %>%
  ggplot() + 
  geom_sf(aes(fill = growth)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "rate", title = "Growth rate in the number of companies")
plt2d


plt2e <-  mibg_sf %>%
  filter( (year %in% c(2011,2019))&(measure=='emp_industry')&(industry==indus) ) %>%
  mutate(dum=if_else(year==2019,1,-1)) %>%
  group_by(geoid,year) %>%
  summarize(growth=sum(log(value)*dum)-1) %>%
  ggplot() + 
  geom_sf(aes(fill = growth)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "rate", title = "Growth rate in the total employment")
plt2e


plt2f <-  mibg_sf %>%
  filter( (year %in% c(2011,2019))&(measure=='LQ_all')&(industry==indus) ) %>%
  mutate(dum=if_else(year==2019,1,-1)) %>%
  group_by(geoid,year) %>%
  summarize(growth=sum(log(value)*dum)-1) %>%
  ggplot() + 
  geom_sf(aes(fill = growth)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "rate", title = "Growth rate in LQ")
plt2f

ggarrange(plt2d, 
          plt2e, 
          plt2f,
          nrow = 1)




# minority owned business
indus = 'Management of Companies and Enterprises'
time = 2020

# all businesses in the industry
plt3a <-  mibg_sf %>%
  filter((year==time)&(measure=='numb_minorityowned_companies')&(industry==indus)) %>%
  group_by(geoid) %>%
  summarize(value=sum(value)) %>%
  ggplot() + 
  geom_sf(aes(fill = value)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Count", title = "Number of minority owned companies")
plt3a


plt3b <-  mibg_sf %>%
  filter((year==time)&(measure=='emp_minority')&(industry==indus)) %>%
  group_by(geoid) %>%
  summarize(value=sum(value)) %>%
  ggplot() + 
  geom_sf(aes(fill = value)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Employment", title = "Total employment by minority owned \n companies")
plt3b


plt3c <-  mibg_sf %>%
  filter((year==time)&(measure=='LQ_minority')&(industry==indus)) %>%
  group_by(geoid) %>%
  summarize(value=sum(value)) %>%
  ggplot() + 
  geom_sf(aes(fill = value)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "LQ", title = "LQ among minority companies")
plt3c

ggarrange(plt3a, 
          plt3b, 
          plt3c,
          nrow = 1)




plt3d <-  mibg_sf %>%
  filter( (year %in% c(2011,2019))&(measure=='numb_companies')&(industry==indus) ) %>%
  mutate(dum=if_else(year==2019,1,-1)) %>%
  group_by(geoid,year) %>%
  summarize(growth=sum(log(value)*dum)-1) %>%
  ggplot() + 
  geom_sf(aes(fill = growth)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "rate", title = "Growth in the number of Minority owned companies")
plt3d


plt3e <-  mibg_sf %>%
  filter( (year %in% c(2011,2019))&(measure=='emp_industry')&(industry==indus) ) %>%
  mutate(dum=if_else(year==2019,1,-1)) %>%
  group_by(geoid,year) %>%
  summarize(growth=sum(log(value)*dum)-1) %>%
  ggplot() + 
  geom_sf(aes(fill = growth)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "rate", title = "Growth in employment by minority \n owned companies")
plt3e


plt3f <-  mibg_sf %>%
  filter( (year %in% c(2011,2019))&(measure=='LQ_all')&(industry==indus) ) %>%
  mutate(dum=if_else(year==2019,1,-1)) %>%
  group_by(geoid,year) %>%
  summarize(growth=sum(log(value)*dum)-1) %>%
  ggplot() + 
  geom_sf(aes(fill = growth)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "rate", title = "Growth rate in LQ")
plt3f

ggarrange(plt3d, 
          plt3e, 
          plt3f,
          nrow = 1)







