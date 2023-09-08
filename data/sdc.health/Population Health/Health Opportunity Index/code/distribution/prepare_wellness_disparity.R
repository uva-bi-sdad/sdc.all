# Replicating Wellness Disparity Profile of the VHD's HOI
# Wellness Disparity Profile is a measure of the disparate access to services in a community. 
# This profile demonstrates the direct influence of social factors on the opportunity of all Virginians to live 
# a long and healthy life.
# https://apps.vdh.virginia.gov/omhhe/hoi/wellness-disparity-profile


library(readxl)
library(data.table)
library(fabricatr)

# Data
si_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/well_disparity.xlsx", sheet = 1))
hoi_2020 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))


# 2017
si_2017_sel <-
  si_2017[, .(
    geoid = Ctfips,
    measure = "wellness_disparity_indicator",
    value = `Indicator Selector`,
    year = "2017",
    moe = ""
  )]

si_2017_sel[value == "Very Low", value := "1"]
si_2017_sel[value == "Low", value := "2"]
si_2017_sel[value == "Average", value := "3"]
si_2017_sel[value == "High", value := "4"]
si_2017_sel[value == "Very High", value := "5"]
si_2017_sel[, value := as.integer(value)]

si_2017_sel <- unique(si_2017_sel)

si_2017_sel <- si_2017_sel[, .(geoid, measure, value, year, moe)]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

si_2017_sel[si_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"


# 2020
si_2020_sel <- hoi_2020[,.(geoid = CT, 
                             measure = "wellness_disparity_indicator",
                             value = split_quantile(hoi_2020$`Social Impact Profile SI`, 5),
                             year = "2020",
                             moe = "")]

si_2020_sel[, value := as.integer(value)]
si_2020_sel <- unique(si_2020_sel)

si_2020_sel <- si_2020_sel[, .(geoid, measure, value, year, moe)]


# combine
si_sel  <- rbindlist(list(si_2017_sel, si_2020_sel))



readr::write_csv(si_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_2020_wellness_disparity_profile.csv.xz", compression = 9))

# 2020 only
readr::write_csv(si_2020_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2020_wellness_disparity_profile.csv.xz", compression = 9))






# # packages
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(sf)
# library(reshape2)
# 
# # data from HOI website
# orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/well_disparity.xlsx")
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
# # df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique
# 
# # rename measures
# out_df <- df_tracts_nodups %>% 
#   rename( "geoid"= "Ctfips",
#           "wellness_disparity_indicator" = "quintiles")
# out_df <- out_df[,c("geoid", "wellness_disparity_indicator")]
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
# write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_wellness_diparity_profile.csv.xz", compression = 9))
