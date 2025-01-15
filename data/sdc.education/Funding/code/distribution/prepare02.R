library(tidyverse)
install.packages("readxl")
library(readxl)
install.packages("readr")
library(readr)


Data_2023 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2023 County Health Rankings Virginia Data.xlsx")
Data_2022 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2022 County Health Rankings Virginia Data.xlsx")
Data_2021 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2021 County Health Rankings Virginia Data.xlsx")
Data_2020 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2020 County Health Rankings Virginia Data.xlsx")
Data_2019 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2019 County Health Rankings Virginia Data.xls")
Data_2018 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2018 County Health Rankings Virginia Data.xls")
Data_2017 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2017 County Health Rankings Virginia Data.xls")


fin <- data.frame()

#2023
year2023 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2023 County Health Rankings Virginia Data.xlsx", sheet = 5)
colnames(year2023) <- year2023[1,]
year2023 <- year2023[-1,]
year2023.wide <- year2023 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`, schoolFundAdequacy = `School Funding Adequacy`, voterTurnout = `% Voter Turnout`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), schoolFundAdequacy = as.double(schoolFundAdequacy), voterTurnout = as.double(voterTurnout), year = 2023)
year2023.long <- pivot_longer(year2023.wide, cols = c("disconnectedYouth", "schoolFundAdequacy", "voterTurnout"), names_to = "measure")

#2022
year2022 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2022 County Health Rankings Virginia Data.xlsx", sheet = 5)
colnames(year2022) <- year2022[1,]
year2022 <- year2022[-1,]
year2022.wide <- year2022 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`, schoolFundAdequacy = `School funding`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), schoolFundAdequacy = as.double(schoolFundAdequacy), year = 2022)
year2022.long <- pivot_longer(year2022.wide, cols = c("disconnectedYouth", "schoolFundAdequacy"), names_to = "measure")

#2021
year2021 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2022 County Health Rankings Virginia Data.xlsx", sheet = 5)
colnames(year2021) <- year2021[1,]
year2021 <- year2021[-1,]
year2021.wide <- year2021 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2021)
year2021.long <- pivot_longer(year2021.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2020
year2020 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2020 County Health Rankings Virginia Data.xlsx", sheet = 5)
colnames(year2020) <- year2020[1,]
year2020 <- year2020[-1,]
year2020.wide <- year2020 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2020)
year2020.long <- pivot_longer(year2020.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2019
year2019 <-read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2019 County Health Rankings Virginia Data.xls", sheet = 5)
colnames(year2019) <- year2019[1,]
year2019 <- year2019[-1,]
year2019.wide <- year2019 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2019)
year2019.long <- pivot_longer(year2019.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2018
year2018 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2018 County Health Rankings Virginia Data.xls", sheet = 5)
colnames(year2018) <- year2018[1,]
year2018 <- year2018[-1,]
year2018.wide <- year2018 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2018)
year2018.long <- pivot_longer(year2018.wide, cols = c("disconnectedYouth"), names_to = "measure")

#2017
year2017 <- read_excel("/Users/avagutshall/Desktop/folder/sdc.education_dev/Funding/data/2017 County Health Rankings Virginia Data.xls", sheet = 5)
colnames(year2017) <- year2017[1,]
year2017 <- year2017[-1,]
year2017.wide <- year2017 %>% select(FIPS, disconnectedYouth = `% Disconnected Youth`) %>% mutate(disconnectedYouth = as.double(disconnectedYouth), year = 2017)
year2017.long <- pivot_longer(year2017.wide, cols = c("disconnectedYouth"), names_to = "measure")

va_ct_20222023_fundingAdequacy <- as.data.frame(year2023, year2022, year2021, year2020, year2019, year2018, year2017)



va_ct_20172023_vote_youth <- fin %>% rename(geoid = FIPS) %>% filter(measure != "schoolFundAdequacy")
va_ct_20222023_fundingAdequacy <- fin %>% rename(geoid = FIPS) %>% 
  filter(measure == "schoolFundAdequacy") %>% mutate(moe='')



# write_csv(va_ct_20172023_vote_youth, xzfile("va_ct_20172023_vote_youth.csv.xz", compression = 9))
write.csv(va_ct_20222023_fundingAdequacy, "/Users/avagutshall/Downloads/va_ct_2022_2023_funding_adequacy.csv.xz", row.names = FALSE)

file.exists("/Users/avagutshall/Downloads/va_ct_2022_2023_funding_adequacy.csv.xz")  # Check if the file exists






