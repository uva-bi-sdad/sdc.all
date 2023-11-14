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
options(osrm.server = Sys.getenv("OSRM_SERVER"), osrm.profile = "bus")
if(!file.exists("traveltimes_exercise.csv")){
  traveltimes <- osrmTable(
    src = centroid[, c("X", "Y")],  #population-demand
    dst = as.data.frame(provider[, c("longitude", "latitude")])     #providers supply
  )$duration
  write.csv(
    cbind(GEOID = rownames(traveltimes), as.data.frame(traveltimes)),
    "traveltimes_exercise.csv", row.names = FALSE
  )
}

population$obgyn_e2sfca_bus <- catchment_ratio(
  population, provider, traveltimes, 30,
  consumers_value = "female_over_14", providers_id = "ID", providers_value = "doctors", verbose = TRUE
) * 1000

obgyn_e2sfca_bus <- population %>% select(geoid = GEOID, obgyn_e2sfca_bus) %>% pivot_longer(cols = obgyn_e2sfca_bus) %>% st_drop_geometry() %>% mutate(year = 2022, moe = NA)

readr::write_csv(obgyn_e2sfca_bus, xzfile("./data/distribution/va059_cms_2022_obgyn_access_scores.csv.xz", compression = 9))
