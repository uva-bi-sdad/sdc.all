#EPA EJScreen
#Download directly from a website
#Note the EJSCREEN zip file is too large for git so the
#data is downloaded and the NCR counties are separated out
#and this file is saved in original
library(utils)
library(readr)
library(dplyr)
library(stringr)

#code to read in block group data
 url <- "https://gaftp.epa.gov/EJSCREEN/2022/EJSCREEN_2022_with_AS_CNMI_GU_VI.csv.zip"
dest <- "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/us_bg_epa_2022_ejscreen.zip" 
download.file(url, dest)

 originalus <- "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/us_bg_epa_2022_ejscreen.zip"
   original <- "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original"
unzip(zipfile = originalus, exdir = original)

library(readr)
ejscreen_original <- read_csv("~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/EJSCREEN_2022_Full_with_AS_CNMI_GU_VI.csv")
dim(ejscreen_original); View(ejscreen_original)

#keep only counties/cities in the NCR
ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")
ejscreen_ncr <- ejscreen_original %>% dplyr::filter(str_detect(ID, ncr_counties)) 
dim(ejscreen_ncr)

write.csv(ejscreen_ncr, "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/EJSCREEN_Table_BlockGroups_NCR.csv", row.names=FALSE)

#code to read in census tract data
 url <- "https://gaftp.epa.gov/EJSCREEN/2022/EJSCREEN_2022_with_AS_CNMI_GU_VI_Tracts.csv.zip"
dest <- "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/us_tr_epa_2022_ejscreen.zip" 
download.file(url, dest)

originalus <- "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/us_tr_epa_2022_ejscreen.zip"
  original <- "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original"
unzip(zipfile = originalus, exdir = original)

library(readr)
ejscreen_original <- read_csv("~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/EJSCREEN_Full_with_AS_CNMI_GU_VI_Tracts.csv")
dim(ejscreen_original); View(ejscreen_original)

#keep only counties/cities in the NCR
ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")
ejscreen_ncr <- ejscreen_original %>% dplyr::filter(str_detect(ID, ncr_counties)) 
dim(ejscreen_ncr)

write.csv(ejscreen_ncr, "~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/EJSCREEN_Table_CensusTracts_NCR.csv", row.names=FALSE)


