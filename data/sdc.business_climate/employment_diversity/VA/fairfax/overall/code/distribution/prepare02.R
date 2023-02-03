# Relation between minority-owned businesses and minority workers
#       - statistics and maps


# library
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(tidygeocoder)


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

# build the distribution of companies and minority companies. We will compare with the lodes data
data <- mibg %>%
  filter((measure %in% c('numb_minorityowned_companies','numb_companies') )) %>%
  group_by(geoid,year,measure,year_census) %>%
  summarize(value=sum(value)) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(pct_minority= 100*numb_minorityowned_companies/numb_companies)


# upload the geometry
temp_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(geoid=GEOID, geometry) %>%
  mutate(year_census=2010)

temp_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(geoid=GEOID, geometry) %>%
  mutate(year_census=2020)

fairfax_bgsf <- rbind(temp_bg2010,temp_bg2020)
fairfax_bg <- setDT(st_drop_geometry(fairfax_bgsf))

data_sf <- sf::st_as_sf(merge(data, fairfax_bgsf, by.x = c('geoid','year_census'), by.y = c('geoid','year_census'), all.x = TRUE))





# use of information
lodesbg <- lodes %>%
  filter( (region_type=='block group') & (geoid %in% fairfax_geoidlist) & (str_detect(measure, str_c(measurelist, collapse = "|")))) %>%
  mutate(measure=str_remove(measure,'_number_jobs')) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(total=Male+Female,
         minority = Black_or_African_american + Asian + American_indian_and_Alaska + Hawaiian_and_other_islander ,
         pct_minority=100*minority/total) %>%
  select(geoid,year,minority,total,pct_minority_worker=pct_minority)



# merge the two data
temp <- merge(data, lodesbg, by.x = c('geoid','year'), by.y = c('geoid','year'), all.y = TRUE)

plt1 <- ggplot(temp[temp$year==2019,], aes(x=pct_minority_worker, y=pct_minority )) + 
  geom_point(color="blue") +
  labs(x = "Percentage of minority workers", y="Percentage of minority business", title= 'Scatter plot between the percentage of minority workers and minority business \n across block group')
plt1

plt2 <- ggplot(temp[temp$year==2019,], aes(x=log(minority), y=log(numb_minorityowned_companies) )) + 
  geom_point(color="blue") +
  labs(x = "Number of minority workers (in log)", y="Number of minority business (in log)", title= 'Scatter plot between the number of minority workers and minority business \n across block group')
plt2

ggarrange(plt1, 
          plt2, 
          nrow = 1)



# all industries
time = 2019

data <- mibg %>%
  filter((year==time)&(measure %in% c('numb_minorityowned_companies','numb_companies') )) %>%
  group_by(geoid,measure,year_census) %>%
  summarize(value=sum(value)) %>%
  pivot_wider(names_from=measure, values_from=value) %>%
  mutate(pct_minority= 100*numb_minorityowned_companies/numb_companies)

data_sf <- sf::st_as_sf(merge(data, fairfax_bgsf, by.x = c('geoid','year_census'), by.y = c('geoid','year_census'), all.x = TRUE))

# all businesses in fairfax
plt1a <-  data_sf %>%
  ggplot() + 
  geom_sf(aes(fill = numb_minorityowned_companies)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Count", title = "Total number of minority owned companies")
plt1a


plt1b <-   data_sf %>%
  ggplot() + 
  geom_sf(aes(fill = pct_minority)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "green",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Percentage", title = "Percentage of minority owned companies")
plt1b

ggarrange(plt1a, 
          plt1b, 
          nrow = 1)
