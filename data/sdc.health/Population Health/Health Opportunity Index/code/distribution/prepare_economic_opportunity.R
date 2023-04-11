library(readxl)
library(data.table)
library(sf)
library(fabricatr)

# Data
econ_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/econ_opp.xlsx", sheet = 1))
hoi_2022 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))
# Geographies
geo_names <- fread("https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/geographies.csv")
geo_names <- geo_names[!region_name %like% "District Of Columbia" & 
                         !region_name %like% " city" &
                         !region_name %like% " of " &
                         !region_name %like% " and "]
geo_names <- geo_names[geo_names$geoid %like% "^51",]
geo_names_10 <- geo_names[year == "2010", .(geoid, region_name, region_type)]
geo_names_20 <- geo_names[year == "2020", .(geoid, region_name, region_type)]

# 2017
econ_2017_sel <-
  econ_2017[, .(
    geoid = Ctfips,
    measure = "economic_opportunity_indicator",
    measure_type = "index",
    value = `Indicator Selector`,
    year = "2017",
    moe = ""
  )]

econ_2017_sel[value == "Very Low", value := "1"]
econ_2017_sel[value == "Low", value := "2"]
econ_2017_sel[value == "Average", value := "3"]
econ_2017_sel[value == "High", value := "4"]
econ_2017_sel[value == "Very High", value := "5"]
econ_2017_sel[, value := as.integer(value)]

econ_2017_sel <- unique(econ_2017_sel)

# merge with geo
econ_2017_sel_mrg <- merge(econ_2017_sel, geo_names_10, by = "geoid")
econ_2017_sel_mrg <- econ_2017_sel_mrg[, .(geoid, measure, measure_type, region_name, region_type, value, year, moe)]


# 2022
econ_2022_sel <- hoi_2022[,.(geoid = CT, 
                             measure = "economic_opportunity_indicator",
                             measure_type = "index",
                             value = split_quantile(hoi_2022$`Economic Profile SI`, 5),
                             year = "2022",
                             moe = "")]
            
econ_2022_sel[, value := as.integer(value)]
econ_2022_sel <- unique(econ_2022_sel)

# merge with geo
econ_2022_sel_mrg <- merge(econ_2022_sel, geo_names_20, by = "geoid")
econ_2022_sel_mrg <- econ_2022_sel_mrg[, .(geoid, measure, measure_type, region_name, region_type, value, year, moe)]


# combine
econ_sel_mrg  <- rbindlist(list(econ_2017_sel_mrg, econ_2022_sel_mrg))



fwrite(econ_sel_mrg, "Population Health/Health Opportunity Index/data/distribution/va_cttr_vdh_2017_2022_economic_opportunity_profile.csv")

