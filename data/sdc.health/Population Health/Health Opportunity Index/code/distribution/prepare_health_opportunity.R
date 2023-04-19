library(readxl)
library(data.table)
library(sf)

hoi_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi.xlsx", sheet = 1))
hoi_2022 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))

# 2017
hoi_2017_sel <-
  hoi_2017[, .(
    geoid = Ctfips,
    measure = "health_opportunity_indicator",
    measure_type = "index",
    value = `Profile Selector`,
    year = "2017",
    moe = ""
  )]

hoi_2017_sel[value == "Very Low", value := "1"]
hoi_2017_sel[value == "Low", value := "2"]
hoi_2017_sel[value == "Average", value := "3"]
hoi_2017_sel[value == "High", value := "4"]
hoi_2017_sel[value == "Very High", value := "5"]
hoi_2017_sel[, value := as.integer(value)]

hoi_2017_sel <- unique(hoi_2017_sel)

# geographies
geo_names <- fread("https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/geographies.csv")
geo_names <- geo_names[year == "2010", .(geoid, region_name, region_type)]

hoi_2017_sel_mrg <- merge(hoi_2017_sel, geo_names, by = "geoid")
hoi_2017_sel_mrg <- hoi_2017_sel_mrg[, .(geoid, measure, measure_type, region_name, region_type, value, year, moe)]


# 2022
hoi_2022_sel <- 
  hoi_2022[, .(
    geoid = CT, 
    measure = "health_opportunity_indicator",
    measure_type = "index",
    value = `Composite in Quintiles`,
    year = "2022",
    moe = ""
  )]

hoi_2022_sel[value == "Very Low Opportunity", value := "1"]
hoi_2022_sel[value == "Low Opportunity", value := "2"]
hoi_2022_sel[value == "Moderate Opportunity", value := "3"]
hoi_2022_sel[value == "High Opportunity", value := "4"]
hoi_2022_sel[value == "Very High Opportunity", value := "5"]
hoi_2022_sel[, value := as.integer(value)]

hoi_2022_sel <- unique(hoi_2022_sel)

# geographies
geo_names <- fread("https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/geographies.csv")
geo_names <- geo_names[year == "2020", .(geoid, region_name, region_type)]

hoi_2022_sel_mrg <- merge(hoi_2022_sel, geo_names, by = "geoid")
hoi_2022_sel_mrg <- hoi_2022_sel_mrg[, .(geoid, measure, measure_type, region_name, region_type, value, year, moe)]


# combine
hoi_sel <- rbindlist(list(hoi_2017_sel, hoi_2022_sel))



fwrite(hoi_sel_mrg, "Population Health/Health Opportunity Index/data/distribution/va_cttr_vdh_2017_2022_health_opportunity_profile.csv")

