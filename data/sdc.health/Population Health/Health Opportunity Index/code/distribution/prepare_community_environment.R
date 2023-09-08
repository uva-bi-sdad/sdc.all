# Replicating Community Environmental Profile of the VHD's HOI
# The Community Environmental Profile examines the natural, built and social environment of a community
# https://apps.vdh.virginia.gov/omhhe/hoi/community-environmental-profile

library(readxl)
library(data.table)
library(fabricatr)

# Data
comm_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/comm_envir.xlsx", sheet = 1))
hoi_2020 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))


# 2017
comm_2017_sel <-
  comm_2017[, .(
    geoid = Ctfips,
    measure = "community_environment_indicator",
    value = `Indicator Selector`,
    year = "2017",
    moe = ""
  )]

comm_2017_sel[value == "Very Low", value := "1"]
comm_2017_sel[value == "Low", value := "2"]
comm_2017_sel[value == "Average", value := "3"]
comm_2017_sel[value == "High", value := "4"]
comm_2017_sel[value == "Very High", value := "5"]
comm_2017_sel[, value := as.integer(value)]

comm_2017_sel <- unique(comm_2017_sel)

comm_2017_sel <- comm_2017_sel[, .(geoid, measure, value, year, moe)]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

comm_2017_sel[comm_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"


# 2020
comm_2020_sel <- hoi_2020[,.(geoid = CT, 
                             measure = "community_environment_indicator",
                             value = split_quantile(hoi_2020$`Built Environment Profile SI`, 5),
                             year = "2020",
                             moe = "")]

comm_2020_sel[, value := as.integer(value)]
comm_2020_sel <- unique(comm_2020_sel)

comm_2020_sel <- comm_2020_sel[, .(geoid, measure, value, year, moe)]


# combine
comm_sel  <- rbindlist(list(comm_2017_sel, comm_2020_sel))



readr::write_csv(comm_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_2020_community_environment_profile.csv.xz", compression = 9))

# 2020 only
readr::write_csv(comm_2020_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2020_community_environment_profile.csv.xz", compression = 9))



# # packages
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(sf)
# library(reshape2)
# 
# # data from HOI website
# orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/comm_envir.xlsx")
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
# #df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique
# 
# # rename measures
# out_df <- df_tracts_nodups %>% 
#   rename( "geoid"= "Ctfips",
#           "community_environment_indicator" = "quintiles")
# out_df <- out_df[,c("geoid", "community_environment_indicator")]
# 
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
# write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_community_environmental_profile.csv.xz", compression = 9))
