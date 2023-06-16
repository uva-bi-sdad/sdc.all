library(dplyr)
library(lehdr)
library(tidyr)
library(sf)
library(tigris)

# Extract LODES WAC at the block group level
savepath = "Employment/Worker_diversity/data/distribution/"

# get the lodes data for VIRGINIA ----------------------------------------------------------------------------
selected_year <- 2010:2019
segments <- c("SA01", "SA02", "SA03", "SE01", "SE02", "SE03", "SI01", "SI02", "SI03")
table <- NULL
tables <- NULL

for (i in 1:length(segments)){
table <- grab_lodes(state = 'va', 
                          year = selected_year, 
                          lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                          job_type = "JT00",        # all jobs 
                          segment = segments[i], 
                          state_part = "main",
                          agg_geo = "bg") %>% select(year, state, geoid=w_bg, starts_with('CS'))

# reshape the data
table <- table %>% 
  pivot_longer(cols = starts_with("CS"), names_to = "sex_id", values_to = "value") %>%
  mutate(region_type='block group',
         segment = segments[i],
         sex=case_when(
           sex_id=='CS01' ~ "Male",
           sex_id=='CS02' ~ "Female"))

tables <- rbind(tables, table)

}

overall_table <- NULL
overall_tables <- NULL
agg_geos <- c("bg", "tract", "county")
for(i in 1:length(agg_geos)){
overall_table <- grab_lodes(state = 'va', 
                    year = selected_year, 
                    lodes_type = "wac",       # only wac = Workplace Area Characteristic data 
                    job_type = "JT00",        # all jobs 
                    segment = "S000", 
                    state_part = "main",
                    agg_geo = agg_geos[i]) %>% select(year, state, geoid=paste0("w_", agg_geos[i]), starts_with('CS'))

overall_table <- overall_table %>% 
  pivot_longer(cols = starts_with("CS"), names_to = "sex_id", values_to = "value") %>%
  mutate(region_type='block group',
         segment = "overall",
         sex=case_when(
           sex_id=='CS01' ~ "Male",
           sex_id=='CS02' ~ "Female")) %>% select(year, geoid, sex_id, value, segment)

overall_tables <- rbind(overall_tables, overall_table)
}


combined <- tables %>% left_join(overall_tables, by = c("year", "geoid", "sex_id")) %>% mutate(perc = (value.x/value.y)*100)
combined_overall <- combined %>% select(year, geoid, sex_id, value = value.y, segment = segment.y) %>% mutate(aggregation = "count")
combined_count <- combined %>% select(year, geoid, sex_id, value = value.x, segment = segment.x) %>% mutate(aggregation = "count")
combined_percent <- combined %>% select(year, geoid, sex_id, value = perc, segment = segment.x)%>% mutate(aggregation = "perc")
data <- combined_overall %>% rbind(combined_count) %>% rbind(combined_percent)

data <- data %>% mutate(sex_id = ifelse(sex_id == "CS01", "male", "female"),
 segment = case_when(
    segment == "SA01" ~ "age_29_and_under",
    segment == "SA02" ~ "age_30_to_54",
    segment == "SA03" ~ "age_55_and_over",
    segment == "SE01" ~ "earnings_1250_and_under",
    segment == "SE02" ~ "earnings_1251_to_3333",
    segment == "SE03" ~ "earnings_3334_and_over",
    segment == "SI01" ~ "industry_goods",
    segment == "SI02" ~ "industry_trade_transporation_utilities",
    segment == "SI03" ~ "industry_other_services",
    segment == "overall" ~ "overall")) %>%
  mutate(measure = paste0(segment, "_", sex_id, "_", aggregation)) %>% select(-sex_id, -segment, -aggregation) %>% distinct() %>%
  mutate(moe = NA)

# TOO BIG - FIGURE IT OUT!
#write.csv(data, "./Employment/Worker_diversity/data/distribution/va_cttrbg_lodes_2010_2019_workplace_employment_segments_by_sex.csv.xz",
#          row.names = FALSE)

fx <- data %>% filter(stringr::str_detect(geoid, "^51059"))
readr::write_csv(fx, xzfile("./Employment/Worker_diversity/data/distribution/va059_cttrbg_lodes_2010_2019_workplace_employment_segments_by_sex.csv.xz", compression = 9))

