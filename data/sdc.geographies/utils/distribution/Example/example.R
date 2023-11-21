library(dplyr)
library(readr)
library(tigris)
library(ggplot2)
library(sf)

source("utils/distribution/tract_conversions.R")

# reading 5 years (2017-2021) Townsend Index (Material Deprivation Index) by tract, county, and health district
data <- read_csv("utils/distribution/Example/va_hdcttr_vdh_2017_2021_material_deprivation_index.csv.xz")

# filter to years and region_type that need to be redistributed 
data <- data %>% filter(year <=2019)
data <- data %>% filter(region_type =='tract')

# use the standardize function
standardized_data <- standardize_all(data)

# this function produces both standardized and original values (see the measure variable)

# producing two maps with standardized and original index values for the year 2019
standardized_data <- standardized_data %>% filter(year ==2019)
standardized_data_std <- standardized_data %>% filter(measure == 'material_deprivation_indicator_std')
standardized_data_org <- standardized_data %>% filter(measure == 'material_deprivation_indicator_orig_2010')

# getting tract shape files for VA
virginia_tracts_2010 <- tracts(state = "51", year = 2010, cb = TRUE)
virginia_tracts_2020 <- tracts(state = "51", year = 2020, cb = TRUE)
virginia_tracts_2010$geoid <- substring(virginia_tracts_2010$GEO_ID, 10)
virginia_tracts_2020$geoid <- virginia_tracts_2020$GEOID



# merging the standardized and original files
standardized_data_std <- merge(virginia_tracts_2020, standardized_data_std, by = "geoid", all.x = TRUE)
standardized_data_org <- merge(virginia_tracts_2010, standardized_data_org, by = "geoid", all = TRUE)

# making maps for comparison
my_plot <- ggplot(data = standardized_data_std) +
  geom_sf(aes(fill = value)) +
  scale_fill_gradient2(low = "white", high = "red", midpoint = median(standardized_data_std$value, na.rm = TRUE), space = "Lab", na.value = "grey50", limits = c(0, 0.5)) +
  labs(title = 'Townsend Index - Standardized', fill = 'Value') +
  theme_minimal()
ggsave("utils/distribution/Example/standardized.png", plot = my_plot, width = 10, height = 8, dpi = 300)


# making maps for comparison
my_plot <- ggplot(data = standardized_data_org) +
  geom_sf(aes(fill = value)) +
  scale_fill_gradient2(low = "white", high = "red", midpoint = median(standardized_data_org$value, na.rm = TRUE), space = "Lab", na.value = "grey50", limits = c(0, 0.5)) +
  labs(title = 'Townsend Index - Original', fill = 'Value') +
  theme_minimal()
ggsave("utils/distribution/Example/original.png", plot = my_plot, width = 10, height = 8, dpi = 300)

