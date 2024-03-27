library(data.table)
library(dplyr)
source("utils/distribution/aggregate.R")

dat <- read.csv("Years of Schooling/data/working/va_trct_acs5_2015_2021_years_of_schooling.csv", colClasses=(c(geoid='character')))
dat20172019 <- dat %>% filter(year %in% c(2015, 2016, 2017, 2018, 2019)) %>% mutate(measure='average_years_schooling', measure_type = "index") %>% select(geoid, year, measure, measure_type, value)
dat20202021 <- dat %>% filter(year %in% c(2020, 2021)) %>% mutate(measure='average_years_schooling', measure_type = "index") %>% select(geoid, year, measure, measure_type, value)

# geographies
geo_names <- fread("https://raw.githubusercontent.com/uva-bi-sdad/sdc.metadata/master/geographies.csv")
geo_names <- geo_names[!region_name %like% "District Of Columbia" &
                         !region_name %like% "Manassas Park city" &
                         !region_name %like% "Fairfax city" &
                         !region_name %like% "Alexandria city" &
                         !region_name %like% "Manassas city" &
                         !region_name %like% "Falls Church city"]

geo_names_10 <- geo_names[year == "2010", .(geoid, region_name, region_type)]
geo_names_20 <- geo_names[year == "2020", .(geoid, region_name, region_type)]

dat20172019_mrg <- unique(merge(dat20172019, geo_names_10, by = "geoid"))
dat20202021_mrg <- unique(merge(dat20202021, geo_names_20, by = "geoid"))

dat_mrg <- rbindlist(list(dat20172019_mrg, dat20202021_mrg))

dat_fin <- dat_mrg[,.(geoid, measure, value, year, moe = "")]

dat_fin_tr <- dat_fin %>% filter(nchar(geoid)==11)
dat_fin_ct <- dat_fin %>% filter(nchar(geoid)==5)

# aggregate counties to health district level
ct_hd <- aggregate(dat_fin_ct, "county", method=mean)

# merge counties, tracts, health districts
all <- rbind(ct_hd, dat_fin_tr)

# fwrite(dat_fin, "Years of Schooling/data/distribution/va_tr_acs5_2017_2021_years_of_schooling.csv")
readr::write_csv(all, xzfile("Years of Schooling/data/distribution/va_hdcttr_acs5_2015_2021_years_of_schooling.csv.xz", compression = 9))
