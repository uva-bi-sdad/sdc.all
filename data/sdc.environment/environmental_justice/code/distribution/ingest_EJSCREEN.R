library(data.table)
library(readr)

# csv files must be initially downloaded from https://gaftp.epa.gov/EJSCREEN/

dt2021 <- fread("environmental_justice/data/original/EJSCREEN_2021_USPR.csv")
dt2021tr <- dt2021[substr(as.character(ID), 1, 2)=="51",]
write_csv(dt2021tr, "environmental_justice/data/original/EJSCREEN_2021_USPR_BG_VA.csv")

dt2020 <- fread("environmental_justice/data/original/EJSCREEN_2020_USPR.csv")
dt2020tr <- dt2020[substr(as.character(ID), 1, 2)=="51",]
write_csv(dt2020tr, "environmental_justice/data/original/EJSCREEN_2020_USPR_BG_VA.csv")

dt2019 <- fread("environmental_justice/data/original/EJSCREEN_2019_USPR.csv")
dt2019tr <- dt2019[substr(as.character(ID), 1, 2)=="51",]
write_csv(dt2019tr, "environmental_justice/data/original/EJSCREEN_2019_USPR_BG_VA.csv")

dt2018 <- fread("environmental_justice/data/original/EJSCREEN_Full_USPR_2018.csv")
dt2018tr <- dt2018[substr(as.character(ID), 1, 2)=="51",]
write_csv(dt2018tr, "environmental_justice/data/original/EJSCREEN_2018_USPR_BG_VA.csv")

dt2017 <- fread("environmental_justice/data/original/EJSCREEN_2017_USPR_Public.csv")
dt2017tr <- dt2017[substr(as.character(ID), 1, 2)=="51",]
write_csv(dt2017tr, "environmental_justice/data/original/EJSCREEN_2017_USPR_BG_VA.csv")

dt2016 <- fread("environmental_justice/data/original/EJSCREEN_Full_V3_USPR_TSDFupdate.csv")
dt2016tr <- dt2016[substr(as.character(ID), 1, 2)=="51",]
write_csv(dt2016tr, "environmental_justice/data/original/EJSCREEN_2016_USPR_BG_VA.csv")

dt2015 <- fread("environmental_justice/data/original/EJSCREEN_20150505.csv")
dt2015tr <- dt2015[substr(as.character(FIPS), 1, 2)=="51",]
write_csv(dt2015tr, "environmental_justice/data/original/EJSCREEN_2015_USPR_BG_VA.csv")

