# Replicating Segregation Indicator of the VHD's HOI Wellness Disparity Profile
# A measure of whether and how much people of different racial and ethnic backgrounds live together in diverse communities. 
# It includes measures of both community diversity and the distance between communities with different racial or ethnic profiles.
# https://apps.vdh.virginia.gov/omhhe/hoi/wellness-disparity-profile



library(readxl)
library(data.table)
library(fabricatr)

# Data
seg_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/seggreg.xlsx", sheet = 1))
hoi_2022 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/HOI V3 14 Variables_For UVA.xlsx", sheet = 1))


# 2017
seg_2017_sel <-
  seg_2017[, .(
    geoid = Ctfips,
    measure = "segregation_indicator",
    value = `Indicator Selector`,
    year = "2017",
    moe = ""
  )]

seg_2017_sel[value == "Very Low", value := "1"]
seg_2017_sel[value == "Low", value := "2"]
seg_2017_sel[value == "Average", value := "3"]
seg_2017_sel[value == "High", value := "4"]
seg_2017_sel[value == "Very High", value := "5"]
seg_2017_sel[, value := as.integer(value)]

seg_2017_sel <- unique(seg_2017_sel)

seg_2017_sel <- seg_2017_sel[, .(geoid, measure, value, year, moe)]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

seg_2017_sel[seg_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"


# 2022
seg_2022_sel <- hoi_2022[,.(geoid = CT2, 
                             measure = "segregation_indicator",
                             value = split_quantile(hoi_2022$`**Spatial Segregation`, 5),
                             year = "2020",
                             moe = "")]

seg_2022_sel[, value := as.integer(value)]
seg_2022_sel <- unique(seg_2022_sel)

seg_2022_sel <- seg_2022_sel[, .(geoid, measure, value, year, moe)]

# invert values
seg_2022_sel$value <- abs(seg_2022_sel$value - 6)


# combine
seg_sel  <- rbindlist(list(seg_2017_sel, seg_2022_sel))

# save the dataset 
readr::write_csv(seg_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_hdcttr_vdh_2017_2020_segregation_index.csv.xz", compression = 9))









# # packages
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(sf)
# library(reshape2)
# 
# # data from HOI website
# orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/seggreg.xlsx")
# df_tracts <- orig_df[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
#                         "Ctfips", "Indicator Selector")] 
# df_tracts <- df_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# # assign quintiles
# df_tracts <- df_tracts %>% mutate(
#   quintiles = case_when(
#     `Indicator Selector` == "Very Low" ~ 1, 
#     `Indicator Selector` == "Low" ~2,
#     `Indicator Selector` == "Average" ~3,
#     `Indicator Selector` == "High" ~4,
#     `Indicator Selector` == "Very High" ~5
#   )
# )
# df_tracts["new_id"] <- paste0(df_tracts$Ctfips, "_", df_tracts$quintiles)
# # remove duplicates
# df_tracts_nodups <- df_tracts %>% distinct()
# # check if the census tarct ID is unique
# # df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique
# 
# # rename measures
# out_df <- df_tracts_nodups %>% 
#   rename( "geoid"= "Ctfips",
#           "segregation_indicator" = "quintiles")
# out_df <- out_df[,c("geoid", "segregation_indicator")]
# 
# # long format
# out_long <- melt(out_df,
#                  id.vars=c("geoid"),
#                  variable.name="measure",
#                  value.name="value"
# )
# 
# # add missing columns
# out_long["year"] <- "2017"
# out_long["moe"] <- ""
# 
# # Select final columns
# out_long <- out_long[, c("geoid", "year", "measure", "value", "moe")]
# 
# # bedford city tract stil in VDH data:
# # bedford city (51515050100) became Bedford County tract (51019050100)
# # updating tract id for bedford city  
# 
# out_long[out_long$geoid == "51515050100", "geoid"] <- "51019050100"
# 
# # save the dataset 
# write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_segregation_index.csv.xz", compression = 9))
