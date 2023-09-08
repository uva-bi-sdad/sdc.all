# Replicating Consumer Opportunity Profile of the VHD's HOI
# The Consumer Opportunity Profile examines the access each community has to basic consumer resources.
# https://apps.vdh.virginia.gov/omhhe/hoi/consumer-opportunity-profile

library(readxl)
library(data.table)
library(fabricatr)

# Data
consum_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/consum_opp.xlsx", sheet = 1))
hoi_2020 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))

# 2017
consum_2017_sel <-
  consum_2017[, .(
    geoid = Ctfips,
    measure = "consumer_opportunity_indicator",
    value = `Indicator Selector`,
    year = "2017",
    moe = ""
  )]

consum_2017_sel[value == "Very Low", value := "1"]
consum_2017_sel[value == "Low", value := "2"]
consum_2017_sel[value == "Average", value := "3"]
consum_2017_sel[value == "High", value := "4"]
consum_2017_sel[value == "Very High", value := "5"]
consum_2017_sel[, value := as.integer(value)]

consum_2017_sel <- unique(consum_2017_sel)

consum_2017_sel <- consum_2017_sel[, .(geoid, measure, value, year, moe)]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

consum_2017_sel[consum_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"


# 2020
consum_2020_sel <- hoi_2020[,.(geoid = CT, 
                             measure = "consumer_opportunity_indicator",
                             value = split_quantile(hoi_2020$`Consumer Profile SI`, 5),
                             year = "2020",
                             moe = "")]

consum_2020_sel[, value := as.integer(value)]
consum_2020_sel <- unique(consum_2020_sel)

consum_2020_sel <- consum_2020_sel[, .(geoid, measure, value, year, moe)]


# combine
consum_sel  <- rbindlist(list(consum_2017_sel, consum_2020_sel))



readr::write_csv(consum_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_2020_consumer_opportunity_profile.csv.xz", compression = 9))

# just 2020
readr::write_csv(consum_2020_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2020_consumer_opportunity_profile.csv.xz", compression = 9))





# # packages
# library(readxl)
# library(dplyr)
# library(tidyverse)
# library(sf)
# library(reshape2)
# 
# # data from HOI website
# orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/consum_opp.xlsx")
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
#           "consumer_opportunity_indicator" = "quintiles")
# out_df <- out_df[,c("geoid", "consumer_opportunity_indicator")]
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
# write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_consumer_opportunity_profile.csv.xz", compression = 9))
