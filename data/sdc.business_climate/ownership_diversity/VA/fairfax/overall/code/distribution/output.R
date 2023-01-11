# - show the use of our final dataset
# - generate all final tables, figures and maps


# 1. upload the data -------------------------------






# 2. maps  ------------------------------------------------
# solo proprietor over time
ggplot(year_features)  + 
  geom_bar(aes(x=year, y=number_firms), stat="identity") +
  geom_line(aes(x=year, y=1000*solo_proprietor), stat="identity", color="red", size=1.2) +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  labs(title= "", x="Year", y="Number of small business") +
  scale_y_continuous(sec.axis=sec_axis(~.*0.001,name="% of solo proprietor business"))


# minority owned over time
ggplot(year_features)  + 
  geom_bar(aes(x=year, y=number_firms), stat="identity") +
  geom_line(aes(x=year, y=1000*minority), stat="identity", color="red", size=1.2) +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  labs(title= "", x="Year", y="Number of small business") +
  scale_y_continuous(sec.axis=sec_axis(~.*0.0001,name="% of minority owned business")) 


industry <- year_indus_features %>%
  filter(year %in% c(2010,2020)) %>%
  pivot_longer(c('number_firms','solo_proprietor','minority'), names_to = 'measure', values_to = 'value') %>%
  pivot_wider(names_from = c('year','measure'), values_from = value) 

industry02 <- year_indus_features02 %>%
  filter(year %in% c(2010,2020)) %>%
  pivot_longer(c('number_firms','solo_proprietor','minority'), names_to = 'measure', values_to = 'value') %>%
  pivot_wider(names_from = c('year','measure'), values_from = value) 



# load the census block group infos

fairfax_bg2020 <- block_groups("VA", "059", 2020) %>% 
  select(geoid_2020=GEOID,
         geometry)

year_geo_features02_sf <- sf::st_as_sf(merge(year_geo_features[year_geo_features$year==2020,], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = year_geo_features02_sf, aes(fill = minority)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Number of \nof business", title = "Number of minority owned business by census \n block group (2020)")



fairfax_bg2010 <- block_groups("VA", "059", 2010) %>% 
  select(countyid=COUNTYFP,
         geoid_2010=GEOID,
         geometry)
year_geo_features02_sf <- sf::st_as_sf(merge(year_geo_features[year_geo_features$year==2010,], fairfax_bg2010, by.x = "geoid", by.y = "geoid_2010", all.y = TRUE))
ggplot() + 
  geom_sf(data = year_geo_features02_sf, aes(fill = minority)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Number of \nof business", title = "Number of minority owned business by census \n block group (2010)")


#------------------------------------------------------------------------------------------------------------------------------------------
year_geo_features03 <- mi_fairfax_features %>%
  filter(small==1) %>%
  filter(naics_name=="Construction") %>%
  mutate(geoid=as.character(geoid)) %>%
  group_by(year,geoid) %>%
  summarise(number_firms = length(unique(duns)),
            minority = sum(minority),
            solo_proprietor = sum(solo_proprietor))


year_geo_features03_sf <- sf::st_as_sf(merge(year_geo_features03[year_geo_features03$year==2020,], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = year_geo_features03_sf, aes(fill = minority)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Number of \nof business", title = "Construction (2020)")


year_geo_features03 <- mi_fairfax_features %>%
  filter(small==1) %>%
  filter(naics_name=="Real Estate and Rental and Leasing") %>%
  mutate(geoid=as.character(geoid)) %>%
  group_by(year,geoid) %>%
  summarise(number_firms = length(unique(duns)),
            minority = sum(minority),
            solo_proprietor = sum(solo_proprietor))


year_geo_features03_sf <- sf::st_as_sf(merge(year_geo_features03[year_geo_features03$year==2020,], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = year_geo_features03_sf, aes(fill = minority)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "Number of \nof business", title = "Real Estate and Rental and Leasing (2020)")




LQ_sf <- sf::st_as_sf(merge(LQ[(LQ$year==2020)&(LQ$naics_name=='Mining, Quarrying, and Oil and Gas Extraction'),], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = LQ_sf, aes(fill = LQ)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "LQ", title = "Mining, Quarrying, and Oil and Gas Extraction (2020)")


LQ_sf <- sf::st_as_sf(merge(LQ[(LQ$year==2020)&(LQ$naics_name=='Professional, Scientific, and Technical Services'),], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = LQ_sf, aes(fill = LQ)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "LQ", title = "Professional, Scientific, and Technical Services (2020)")


LQ_sf <- sf::st_as_sf(merge(LQ02[(LQ02$year==2020)&(LQ02$naics_name=='Agriculture, Forestry, Fishing and Hunting'),], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = LQ_sf, aes(fill = LQ)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "LQ", title = "Agriculture, Forestry, Fishing and Hunting (2020)")


LQ_sf <- sf::st_as_sf(merge(LQ02[(LQ02$year==2020)&(LQ02$naics_name=='Professional, Scientific, and Technical Services'),], fairfax_bg2020, by.x = "geoid", by.y = "geoid_2020", all.y = TRUE))
ggplot() + 
  geom_sf(data = LQ_sf, aes(fill = LQ)) + 
  scale_y_continuous() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       aesthetics ="fill") +
  labs(fill = "LQ", title = "Professional, Scientific, and Technical Services (2020)")


