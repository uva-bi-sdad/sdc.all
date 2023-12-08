setwd("./Health Care Services/Physicians/OB-GYN/Service Access Scores")

library(tidyverse)
library(catchment)
library(osrm)
library(sf)
library(tidygeocoder)
library(tidycensus)

# peek at vars
#vars <- tidycensus::load_variables("acs5", year = 2021)
# var names for women over 14
vars <- paste0("B01001_0", seq(30, 49))

# providers
provider <- read_csv("./data/original/vadcmd_cms_2022_obgyn.csv") %>% 
  mutate(address = paste0(adr_ln_1, " ", adr_ln_2, " ", cty, " ", st, " ", substr(zip, 1, 5), "-", substr(zip, 6, 9))) %>% 
  geocode(address, method = 'census', lat = latitude , long = longitude)

## collapse by location
provider$doctors <- 1

## assign IDs just to be explicit
provider$ID <- paste0("l", seq_len(nrow(provider)))
provider <- provider %>% drop_na(latitude, longitude)

# population
population <- get_acs(geography = "tract",
                      variables = vars, 
                      state = "va",
                      county = "059",
                      year = 2021, 
                      geometry = TRUE) %>% select(-moe) %>% tidyr::pivot_wider(names_from = "variable", values_from = "estimate") %>% mutate(centroid = st_coordinates(st_centroid(geometry))) %>% rowwise() %>%
mutate(female_over_14 = sum(c_across(any_of(vars))))

centroid <- population$centroid %>% as.data.frame() %>% cbind(GEOID = population$GEOID)

# traveltime
options(osrm.server = Sys.getenv("OSRM_SERVER"), osrm.profile = "driving")

  traveltimes <- osrmTable(
    src = centroid[, c("X", "Y")],  #population-demand
    dst = as.data.frame(provider[, c("longitude", "latitude")]),
    #providers supply
  )$duration

population$obgyn_e2sfca <- catchment_ratio(
  population, provider, traveltimes, 30,
  consumers_value = "female_over_14", providers_id = "ID", providers_value = "doctors", verbose = TRUE
) * 1000

obgyn_e2sfca <- population %>% select(geoid = GEOID, obgyn_e2sfca) %>% pivot_longer(cols = obgyn_e2sfca) %>% st_drop_geometry() %>% mutate(year = 2022, moe = NA) %>% rename(measure = name)

readr::write_csv(obgyn_e2sfca, xzfile("./data/distribution/va059_cms_2022_obgyn_access_scores.csv.xz", compression = 9))

library(scico)

obgyn_e2sfca_geo <- population %>% select(geoid = GEOID, obgyn_e2sfca) %>% pivot_longer(cols = obgyn_e2sfca) %>% mutate(year = 2022, moe = NA) %>% rename(measure = name)

obgyn_e2sfca_geo %>% 
  # If more than one variable, filter for the name of the variable you want to map
  ggplot() +
  geom_sf(aes(fill = value)) + # fill = name of column with values to map
  labs(fill = "OBGYN Access Score", # Legend title
       title = "Fairfax County OBGYN Access", # Graph title
       subtitle = "Enhanced 2-Stage Floating Floating Catchment Areas", 
       caption = "Women ages 14 and older are Population served \n
       Data Sources: American Community Survey, 5-Year Estimates, \n Age by Sex by Race tables, \n
       Centers for Medicare & Medicaid Services") + 
  #Graph caption
  theme_void() + # Takes out x and y axis, axis labels
  scale_fill_scico(palette = 'lajolla') + # or palette = "vik" (divergent)
  theme(text = element_text(size = 15))

provider_f <- provider %>% filter(cty == "FAIRFAX")
obgyn_e2sfca_geo %>% 
  # If more than one variable, filter for the name of the variable you want to map
  ggplot() +
  geom_sf(aes(fill = value)) + # fill = name of column with values to map
  labs(fill = "OBGYN Access Score", # Legend title
       title = "Fairfax County OBGYN Access", # Graph title
       subtitle = "Enhanced 2-Stage Floating Floating Catchment Areas", 
       caption = "Women ages 14 and older are Population served \n
       Data Sources: American Community Survey, 5-Year Estimates, \n Age by Sex by Race tables, \n
       Centers for Medicare & Medicaid Services") + 
  #Graph caption
  theme_void() + # Takes out x and y axis, axis labels
  scale_fill_scico(palette = 'lajolla') + # or palette = "vik" (divergent)
  theme(text = element_text(size = 15)) +
  geom_sf(data = st_as_sf(provider_f, coords = c("longitude", "latitude"), crs = 4269), color = "white", )
