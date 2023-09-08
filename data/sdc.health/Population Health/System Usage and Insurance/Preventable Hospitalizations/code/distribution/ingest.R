library(data.table)
library(xlsx)

download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/2021%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2021.xlsx")
download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2020.xlsx")
download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2019%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xls", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2019.xlsx")
download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2018%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2018.xlsx")
download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2017%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xls", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2017.xlsx")
download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2016%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2016.xlsx")
download.file("https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2015%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls", "sdc.health_dev/Population Health/System Usage and Insurance/Preventable Hospitalizations/data/original/va_county_health_rankings_2015.xlsx")

