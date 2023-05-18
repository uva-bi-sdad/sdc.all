# Replicating Walkability Indicator of the VHD's HOI Community Environmental Profile
# A measure of how walkable a community is based on residential and employment density, land use 
# (destination) diversity, street connectivity and public transit accessibility.
# https://apps.vdh.virginia.gov/omhhe/hoi/community-environmental-profile

# packages
library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(reshape2)

# data from HOI website
orig_df <- read_excel("Population Health/Health Opportunity Index/data/original/walk.xlsx")
df_tracts <- orig_df[,c("County Name", "LHD Name", "STFIPS (CountyHOI)", 
                        "Ctfips", "Indicator Selector")] 
df_tracts <- df_tracts %>% filter(is.na(`Indicator Selector`) == FALSE)
# assign quintiles
df_tracts <- df_tracts %>% mutate(
  quintiles = case_when(
    `Indicator Selector` == "Very Low" ~ 1, 
    `Indicator Selector` == "Low" ~ 2,
    `Indicator Selector` == "Average" ~ 3,
    `Indicator Selector` == "High" ~ 4,
    `Indicator Selector` == "Very High" ~ 5
  )
)
df_tracts["new_id"] <- paste0(df_tracts$Ctfips, "_", df_tracts$quintiles)
# remove duplicates
df_tracts_nodups <- df_tracts %>% distinct()
# check if the census tarct ID is unique
#df_tracts_unq <- df_tracts_nodups %>% distinct(Ctfips, .keep_all = TRUE) # it is unique

# rename measures
out_df <- df_tracts_nodups %>% 
  rename("geoid"= "Ctfips",
        "walkability_indicator" = "quintiles")
out_df <- out_df[,c("geoid", "walkability_indicator")]


# long format
out_long <- melt(out_df,
                 id.vars=c("geoid"),
                 variable.name="measure",
                 value.name="value"
)

# add missing columns
out_long["year"] <- "2017"
out_long["moe"] <- ""

# Select final columns
out_long <- out_long[, c("geoid", "year", "measure", "value", "moe")]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

out_long[out_long$geoid == "51515050100", "geoid"] <- "51019050100"

# save the dataset 
write_csv(out_long, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_walkability_index.csv.xz", compression = 9))
