library(dplyr)
library(readxl)
library(xlsx)

# HUD Data

download.file("https://www.huduser.gov/portal/datasets/fmr/fmr2023/FY23_FMRs.xlsx", destfile = "Cost/Rent/data/original/FY23_FMRs.xlsx")

download.file("https://www.huduser.gov/portal/datasets/fmr/fmr2023/fy2023_safmrs.xlsx", destfile = "Cost/Rent/data/original/fy2023_safmrs.xlsx")

# crosswalk from ZCTA to county
download.file("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt", destfile = "Cost/Rent/data/original/zcta_county_rel_10.txt" )
