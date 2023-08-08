library(tidyverse)

dataURLs <- data.frame(`2023` = "https://www.countyhealthrankings.org/sites/default/files/media/document/2023%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xlsx",
              `2022` = "https://www.countyhealthrankings.org/sites/default/files/media/document/2022%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1.xlsx",
              `2021` = "https://www.countyhealthrankings.org/sites/default/files/media/document/2021%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx",
              `2020` = "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx",
              `2019` = "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2018%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xls",
              `2018` = "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2017%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xls",
              `2017` = "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2017%20County%20Health%20Rankings%20Virginia%20Data%20-%20v2.xls",check.names = FALSE)

fin <- data.frame()

#2023
year2023 <- gdata::read.xls(dataURLs[,'2023'], sheet = 5)
colnames(year2023) <- year2023[1,]
year2023 <- year2023[-1,]
year2023.wide <- year2023 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`, schoolFundAdequacy = `School Funding Adequacy`, voterTurnout = `% Voter Turnout`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), schoolFundAdequacy = as.double(schoolFundAdequacy), voterTurnout = as.double(voterTurnout), year = 2023)
year2023.long <- pivot_longer(year2023.wide, cols = c("disconnectedYouth", "schoolFundAdequacy", "voterTurnout"), names_to = "measure")

#2022
year2022 <- gdata::read.xls(dataURLs[,'2022'], sheet = 5)
colnames(year2022) <- year2022[1,]
year2022 <- year2022[-1,]
year2022.wide <- year2022 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`, schoolFundAdequacy = `School funding`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), schoolFundAdequacy = as.double(schoolFundAdequacy), year = 2022)
year2022.long <- pivot_longer(year2022.wide, cols = c("disconnectedYouth", "schoolFundAdequacy"), names_to = "measure")

#2021
year2021 <- gdata::read.xls(dataURLs[,'2021'], sheet = 5)
colnames(year2021) <- year2021[1,]
year2021 <- year2021[-1,]
year2021.wide <- year2021 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2021)
year2021.long <- pivot_longer(year2021.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2020
year2020 <- gdata::read.xls(dataURLs[,'2020'], sheet = 5)
colnames(year2020) <- year2020[1,]
year2020 <- year2020[-1,]
year2020.wide <- year2020 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2020)
year2020.long <- pivot_longer(year2020.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2019
year2019 <- gdata::read.xls(dataURLs[,'2019'], sheet = 5)
colnames(year2019) <- year2019[1,]
year2019 <- year2019[-1,]
year2019.wide <- year2019 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2019)
year2019.long <- pivot_longer(year2019.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2018
year2018 <- gdata::read.xls(dataURLs[,'2018'], sheet = 5)
colnames(year2018) <- year2018[1,]
year2018 <- year2018[-1,]
year2018.wide <- year2018 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2018)
year2018.long <- pivot_longer(year2018.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2017
year2017 <- gdata::read.xls(dataURLs[,'2017'], sheet = 5)
colnames(year2017) <- year2017[1,]
year2017 <- year2017[-1,]
year2017.wide <- year2017 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2017)
year2017.long <- pivot_longer(year2017.wide, cols = c("disconnectedYouth"), names_to = "measure")

fin <- rbind(fin, year2023.long, year2022.long, year2021.long, year2020.long, year2019.long, year2018.long, year2017.long)



va_ct_20172023_vote_youth <- fin %>% rename(geoid = FIPS) %>% filter(measure != "schoolFundAdequacy")
va_ct_20222023_fundingAdequacy <- fin %>% rename(geoid = FIPS) %>% filter(measure == "schoolFundAdequacy")



write_csv(va_ct_20172023_vote_youth, xzfile("va_ct_20172023_vote_youth.csv.xz", compression = 9))
write_csv(va_ct_20222023_fundingAdequacy, xzfile("va_ct_20222023_fundingAdequacy.csv.xz", compression = 9))








