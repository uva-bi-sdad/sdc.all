# Prepare VA county -> heath district crosswalk

library(pdftools)
library(stringr)

#
# read in pdf of crosswalk ------------------------
#

pages <- pdf_text("data/va_geo_vhd_2020_health_districts/original/va_ct_to_hd_crosswalk.pdf")


#
# extract table from pdf -----------------------------
#

cw_text = list()

for(i in 1:length(pages))
{
  temp <- strsplit(pages[i], "\n", fixed = TRUE)  
  cw_text <- append(cw_text, temp[[1]])
}

# extract county names
remaining_text <- str_split(cw_text, "\\s+(?=5)")
ct_name <- sapply(remaining_text[2:length(remaining_text)], function(x) {x[1]})

# extract geoids
remaining_text <- sapply(remaining_text[2:length(remaining_text)], function(x) {x[2]})
remaining_text <- str_split(remaining_text, "(?<=\\d)\\s")
ct_geoid <- sapply(remaining_text, function(x) {x[1]})

# extract health district names
remaining_text <- sapply(remaining_text, function(x) {x[2]})
remaining_text <- str_split(remaining_text, "\\s+(?=\\w+ Region)")
hd_name <- sapply(remaining_text, function(x) {x[1]})


#
# create data frame -----------------------------
#

cw_df = data.frame(ct_geoid, ct_name, hd_name)


#
# merge in health district ids
#

hd_map <- jsonlite::read_json("https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/va_geo_vhd_2020_health_districts/distribution/va_geo_vhd_2020_health_districts.geojson")
hd_geoid <- sapply(hd_map$features, function(x) {x$properties$geoid})
hd_name <- hd_ids <- sapply(hd_map$features, function(x) {x$properties$region_name})

hd_df <- data.frame(hd_geoid, hd_name)

# match up any health district name changes
setdiff(cw_df$hd_name, hd_df$hd_name)  # "Rappahannock", "Rappahannock Rapidan", "Pittsylvania-Danville", "Roanoke" 
setdiff( hd_df$hd_name, cw_df$hd_name) # "Roanoke City", "Rappahannock/Rapidan", "Rappahannock Area", "Pittsylvania/Danville"

cw_df[cw_df$hd_name == "Rappahannock", "hd_name"] <- "Rappahannock Area"
cw_df[cw_df$hd_name == "Rappahannock Rapidan", "hd_name"] <- "Rappahannock/Rapidan"
cw_df[cw_df$hd_name == "Pittsylvania-Danville", "hd_name"] <- "Pittsylvania/Danville"
cw_df[cw_df$hd_name == "Roanoke", "hd_name"] <- "Roanoke City"

# merge 
final_cw <- merge(cw_df, hd_df, by ="hd_name", all = TRUE)

final_cw <- final_cw[ , c(2,3,4,1)]


#
# write to distribution folder -----------------------
#

write.csv(final_cw, "data/va_geo_vhd_2020_health_districts/distribution/va_ct_to_hd_crosswalk.csv",
          row.names = FALSE)
