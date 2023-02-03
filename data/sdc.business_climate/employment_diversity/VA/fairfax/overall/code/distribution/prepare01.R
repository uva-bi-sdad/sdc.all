# Compute by block group:
#       - Total employed people working in a block group (by race and gender)
#       - association with number of businesses owned by minority


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

# Load the data --------------------

# 1. geoid from fairfax 
temp_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(geoid=GEOID, region_name=NAMELSAD, geometry) %>%
  mutate(year_census=2010)

temp_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(geoid=GEOID, region_name=NAMELSAD, geometry) %>%
  mutate(year_census=2020)

fairfax_bg <- setDT(st_drop_geometry(rbind(temp_bg2010,temp_bg2020)))
fairfax_geoidlist <- unique(as.numeric(fairfax_bg$geoid))

# 2. upload lodes data
uploadpath = "employment_diversity/VA/fairfax/overall/data/working/"
lodes <-  read_csv(paste0(uploadpath,"vadcmd_bgtrct_lodes_20102019_number_of_jobs.csv.xz"))

lodesbg <- lodes %>%
  filter( (region_type=='block group') & (geoid %in% fairfax_geoidlist) ) %>%
  mutate(measure=str_remove(measure,'_number_jobs')) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(totalworkers=Male+Female,
         minorityworker = Black_or_African_american + Asian + American_indian_and_Alaska + Hawaiian_and_other_islander ,
         pct_minorityworker=100*minorityworker/totalworkers,
         pct_female=100*Female/totalworkers) %>%
  select(geoid,year,minorityworker,pct_minorityworker,totalworkers)


# 2. upload mergent intellect
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

# build the distribution of companies and minority companies. We will compare with the lodes data
data <- mibg %>%
  filter((measure %in% c('numb_minorityowned_companies','numb_companies') )) %>%
  group_by(geoid,year,measure,year_census) %>%
  summarize(value=sum(value)) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(pct_minorityowned= 100*numb_minorityowned_companies/numb_companies,
         minorityownedbusiness=numb_minorityowned_companies,
         totalbusiness=numb_companies) %>%
  select(geoid,year,year_census,totalbusiness,minorityownedbusiness,pct_minorityowned)

# merge the two data over geoid and year
lodes_mi <- merge(lodesbg, data, by.x=c("geoid","year"), by.y=c("geoid","year"), all.x = TRUE)
lodes_mi_sf <- sf::st_as_sf(merge(lodes_mi, temp_bg2010, by.x = "geoid", by.y = "geoid", all.y = TRUE))




# plot the maps ------------------------------------------------------

# 1. geography distribution of minority-owned businesses and minority workers
plt1a <-  ggplot() + 
  geom_sf(data=lodes_mi_sf[lodes_mi_sf$year==2019,], aes(fill=minorityownedbusiness)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "count", title = "Minority workers (workplace) by \n block-group in 2019")

plt1b <-  ggplot() + 
  geom_sf(data=lodes_mi_sf[lodes_mi_sf$year==2019,], aes(fill=minorityworker)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "count", title = "Minoriwty-owned businesses by \n block-group in 2019")

ggarrange(plt1a, 
          plt1b, 
          nrow = 1)


# 2. scatter plot between minority workers and minority-owned businesses
plt2 <- ggplot(lodes_mi_sf[lodes_mi_sf$year==2019,], aes(x=log(1+minorityworker), y=log(1+minorityownedbusiness) )) + 
  geom_point(color="blue") +
  labs(x = "Number of minority workers in the workplace (in log)", y="Number of minority-owned businesses (in log)", title= 'Scatter plot of census block group (as workplace) in 2019')
plt2


plt2a <- ggplot(lodes_mi_sf[lodes_mi_sf$year==2019,], aes(x=log(1+totalbusiness), y=log(1+minorityownedbusiness) )) + 
  geom_point(color="blue") +
  labs(x = "Number of businesses (in log)", y="Number of minority-owned businesses (in log)", title= 'Scatter plot of census block group in 2019')
plt2a



# We correct by analyzing pct_minorityowned
plt3a <-  ggplot() + 
  geom_sf(data=lodes_mi_sf[lodes_mi_sf$year==2019,], aes(fill=pct_minorityworker)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "percentage", title = "Minority workers (workplace) by \n block-group in 2019")

plt3b <-  ggplot() + 
  geom_sf(data=lodes_mi_sf[lodes_mi_sf$year==2019,], aes(fill=pct_minorityowned)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "percentage", title = "Minority-owned businesses by \n block-group in 2019")

ggarrange(plt3a, 
          plt3b, 
          nrow = 1)

plt3c <- ggplot(lodes_mi_sf[lodes_mi_sf$year==2019,], aes(x=pct_minorityworker, y=pct_minorityowned )) + 
  geom_point(color="blue") +
  labs(x = "percentage of minority workers in the workplace (in log)", y="percentage of minority-owned businesses (in log)", title= 'Scatter plot of census block group (as workplace) in 2019')+
  theme(axis.title=element_text(size=15))
plt3c
