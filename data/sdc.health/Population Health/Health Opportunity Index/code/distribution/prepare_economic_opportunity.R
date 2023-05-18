library(readxl)
library(data.table)
library(fabricatr)

# Data
econ_2017 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/econ_opp.xlsx", sheet = 1))
hoi_2022 <- setDT(read_excel("Population Health/Health Opportunity Index/data/original/hoi_indexes_quintile_2022.xlsx", sheet = 1))


# 2017
econ_2017_sel <-
  econ_2017[, .(
    geoid = Ctfips,
    measure = "economic_opportunity_indicator",
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

econ_2017_sel <- econ_2017_sel[, .(geoid, measure, value, year, moe)]

# bedford city tract stil in VDH data:
# bedford city (51515050100) became Bedford County tract (51019050100)
# updating tract id for bedford city  

econ_2017_sel[econ_2017_sel$geoid == "51515050100", "geoid"] <- "51019050100"


# 2022
econ_2022_sel <- hoi_2022[,.(geoid = CT, 
                             measure = "economic_opportunity_indicator",
                             value = split_quantile(hoi_2022$`Economic Profile SI`, 5),
                             year = "2022",
                             moe = "")]
            
econ_2022_sel[, value := as.integer(value)]
econ_2022_sel <- unique(econ_2022_sel)

econ_2022_sel <- econ_2022_sel[, .(geoid, measure, value, year, moe)]


# combine
econ_sel  <- rbindlist(list(econ_2017_sel, econ_2022_sel))



readr::write_csv(econ_sel, xzfile("Population Health/Health Opportunity Index/data/working/tract_data/va_tr_vdh_2017_2022_economic_opportunity_profile.csv.xz", compression = 9))

