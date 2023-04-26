#FEMA National Risk Inventory
#Download directly from a website
library(utils)
url1 <- "https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload//NRI_Table_CensusTracts/NRI_Table_CensusTracts_Virginia.zip"
url2 <- "https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload/NRI_Table_CensusTracts/NRI_Table_CensusTracts_Maryland.zip"
url3 <- "https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload/NRI_Table_CensusTracts/NRI_Table_CensusTracts_DistrictofColumbia.zip"

dest1 <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/va_tr_fema_2021_national_risk_index.zip"
dest2 <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/md_tr_fema_2021_national_risk_index.zip"
dest3 <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/dc_tr_fema_2021_national_risk_index.zip"
 
download.file(url1, dest1)
download.file(url2, dest2)
download.file(url3, dest3)

originalva <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/va_tr_fema_2021_national_risk_index.zip"
originalma <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/md_tr_fema_2021_national_risk_index.zip"
originaldc <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/dc_tr_fema_2021_national_risk_index.zip"
 
  original <- "~/RCode/Data Commons/Environment/FEMA_NRI/data/original/"

unzip(zipfile = originalva, exdir = original)
unzip(zipfile = originalma, exdir = original)
unzip(zipfile = originaldc, exdir = original)
