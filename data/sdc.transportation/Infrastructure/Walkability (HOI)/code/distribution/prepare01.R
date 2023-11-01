# unzip to working
original <- "./Infrastructure/Walkability (HOI)/data/original/WalkabilityIndex.zip"
working <- "./Infrastructure/Walkability (HOI)/data/working/"
unzip(zipfile = original, exdir = working)

library(dplyr)
library(sf)
library(readxl)
library(geojsonio)
library(ggplot2)
fc <- sf::st_read("./Infrastructure/Walkability (HOI)/data/working/Natl_WI.gdb")

# Info from methodology ppt:
# Utilized National Index from Smart location database.
# Used the population-weighted method to aggregate the data from block group into census tract
# Crosswalk from 2010 to 2020 Census Tract using NHGIS website crosswalk provided my University of Minnesota.
# Converted the value to Z-Score

#library(esri2sf)

#url <- "https://geodata.epa.gov/arcgis/rest/services/OA/WalkabilityIndex/MapServer/0"
#df <- esri2sf(url)

# Aggregate to Census tract weighting by population

crosswalk_url <- "https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/State%20Geographies/Health%20Districts/2020/data/distribution/va_ct_to_hd_crosswalk.csv"

# TOTPOP ALSO > 0

combined <- fc %>% filter(STATEFP == "51", TotPop > 0) %>%
  mutate(tr_geoid = paste0(STATEFP, COUNTYFP, TRACTCE),
         ct_geoid = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
  group_by(tr_geoid) %>%
  mutate(tr_NatWalkInd = weighted.mean(NatWalkInd, TotPop)) %>%
  group_by(ct_geoid) %>% mutate(ct_NatWalkInd = weighted.mean(NatWalkInd, TotPop)) %>%
  left_join(read.csv(crosswalk_url), by = ("ct_geoid")) %>%
  group_by(hd_geoid) %>% mutate(hd_NatWalkInd = weighted.mean(NatWalkInd, TotPop)) %>% ungroup()

tr <- combined %>% distinct(tr_geoid, tr_NatWalkInd) %>%
  mutate(tr_NatWalkInd_zscore = (tr_NatWalkInd-mean(tr_NatWalkInd))/sd(tr_NatWalkInd)) %>%
  rename(geoid = tr_geoid, walkability_index_raw = tr_NatWalkInd, walkability_index_zscore = tr_NatWalkInd_zscore)
ct <- combined %>% distinct(ct_geoid, ct_NatWalkInd) %>%
  mutate(ct_NatWalkInd_zscore = (ct_NatWalkInd-mean(ct_NatWalkInd))/sd(ct_NatWalkInd)) %>%
  rename(geoid = ct_geoid, walkability_index_raw = ct_NatWalkInd, walkability_index_zscore = ct_NatWalkInd_zscore)
hd <- combined %>% distinct(hd_geoid, hd_NatWalkInd) %>%
  mutate(hd_NatWalkInd_zscore = (hd_NatWalkInd-mean(hd_NatWalkInd))/sd(hd_NatWalkInd)) %>%
  rename(geoid = hd_geoid, walkability_index_raw = hd_NatWalkInd, walkability_index_zscore = hd_NatWalkInd_zscore)

all <- tr %>% rbind(ct) %>% rbind(hd) %>% tidyr::pivot_longer(cols = c(walkability_index_raw, walkability_index_zscore)) %>% rename(measure = name) %>% mutate(moe = NA, year = 2021)

path_to_raw_vals <- "./Infrastructure/Walkability (HOI)/data/original/HOI V3_14 Variables_Raw Scores.xlsx"
hoi_walkability <- read_excel(path_to_raw_vals) %>% select(geoid = CT2, Walkability)

# Calculating correlation
diff <- all %>% left_join(hoi_walkability, by = "geoid") %>% filter(measure == "walkability_index_raw") %>% filter(!(is.na(Walkability)))
cor(diff$value, diff$Walkability)
# Correlation is 0.5217862, outside of range given by Rex
# Unweighted correlation is 0.5268768

shapes <- geojsonio::geojson_sf("https://raw.githubusercontent.com/uva-bi-sdad/sdc.geographies/main/VA/Census%20Geographies/Tract/2020/data/distribution/va_geo_census_cb_2020_census_tracts.geojson")

diff %>% left_join(shapes, by = "geoid") %>%
  ggplot(aes(fill = value)) +
  geom_sf(aes(geometry = geometry), linewidth = 0) +
  scale_fill_stepsn(
    colours = c("purple", "yellow", "orange", "green"),
    breaks = c(5.75, 10.50, 15.25, max(diff$value)),
    labels = c("Least Walkable", "Below Average Walkable", "Above Average Walkable", "Most Walkable")) +
  theme_void() +
  labs(title = "SDAD Replicated HOI Values",
       caption = "Blank regions have mismatched GEOIDs in the two datasets")

# Bins from https://www.epa.gov/sites/default/files/2021-06/documents/national_walkability_index_methodology_and_user_guide_june2021.pdf
diff %>% left_join(shapes, by = "geoid") %>%
  ggplot(aes(fill = Walkability)) +
  geom_sf(aes(geometry = geometry), linewidth = 0) +
  scale_fill_stepsn(
    colours = c("purple", "yellow", "orange", "green"),
    labels = c("Least Walkable", "Below Average Walkable", "Above Average Walkable", "Most Walkable"),
    breaks = c(5.75, 10.50, 15.25, 20)) +
  theme_void() +
  labs(title = "HOI V3 Walkability Raw Values",
       caption = "Blank regions have mismatched GEOIDs in the two datasets")

# Rex says it's good

readr::write_csv(all, xzfile("./Infrastructure/Walkability (HOI)/data/distribution/va_hdcttr_2021_walkability_index.csv.xz", compression = 9))
