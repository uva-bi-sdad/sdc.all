# Replicating Material Deprivation Indicator of the VHD's HOI Consumer Opportunity Profile
# Based on the Towsend Material Deprivation Index, it examines the private material 
# resources available to households in a community.
# https://apps.vdh.virginia.gov/omhhe/hoi/community-environmental-profile


library(readxl)
library(data.table)
library(fabricatr)

# Data
mat_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/mat_depr.xlsx", sheet = 1))
hoi_2022 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/HOI V3 14 Variables_For UVA.xlsx", sheet = 1))


# 2017
mat_2017_sel <-
  mat_2017[, .(
    geoid = Ctfips,
    measure = "material_deprivation_indicator",
    value = `Indicator Selector`,
    year = "2017",
    moe = ""
  )]

mat_2017_sel[value == "Very Low", value := "1"]
mat_2017_sel[value == "Low", value := "2"]
mat_2017_sel[value == "Average", value := "3"]
mat_2017_sel[value == "High", value := "4"]
mat_2017_sel[value == "Very High", value := "5"]
mat_2017_sel[, value := as.integer(value)]

mat_2017_sel <- unique(mat_2017_sel)

mat_2017_sel <- mat_2017_sel[, .(geoid, measure, value, year, moe)]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

mat_2017_sel[mat_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"


# 2022
mat_2022_sel <- hoi_2022[,.(geoid = CT2, 
                             measure = "material_deprivation_indicator",
                             value = split_quantile(hoi_2022$`Townsend*`, 5),
                             year = "2022",
                             moe = "")]

mat_2022_sel[, value := as.integer(value)]
mat_2022_sel <- unique(mat_2022_sel)

mat_2022_sel <- mat_2022_sel[, .(geoid, measure, value, year, moe)]

# invert values
mat_2022_sel$value <- abs(mat_2022_sel$value - 6)


# combine
mat_sel  <- rbindlist(list(mat_2017_sel, mat_2022_sel))

# save the dataset 
readr::write_csv(mat_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_hdcttr_vdh_2017_2022_material_deprivation_index.csv.xz", compression = 9))








# # packages
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(sf)
# library(reshape2)
# 
# # data from HOI website
# orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/mat_depr.xlsx")
# df_tracts <- orig_df[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
#                         "Ctfips", "Indicator Selector")] 
# df_tracts <- df_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# # assign quintiles
# df_tracts <- df_tracts %>% mutate(
#   quintiles = case_when(
#     `Indicator Selector` == "Very Low" ~ 1, 
#     `Indicator Selector` == "Low" ~ 2,
#     `Indicator Selector` == "Average" ~ 3,
#     `Indicator Selector` == "High" ~ 4,
#     `Indicator Selector` == "Very High" ~ 5
#   )
# )
# df_tracts["new_id"] <- paste0(df_tracts$Ctfips, "_", df_tracts$quintiles)
# # remove duplicates
# df_tracts_nodups <- df_tracts %>% distinct()
# # check if the census tarct ID is unique
# #df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique
# 
# # rename measures
# out_df <- df_tracts_nodups %>% 
#   rename("geoid"= "Ctfips",
#         "material_deprivation_indicator" = "quintiles")
# out_df <- out_df[,c("geoid", "material_deprivation_indicator")]
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
# write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_material_deprivation_index.csv.xz", compression = 9))
