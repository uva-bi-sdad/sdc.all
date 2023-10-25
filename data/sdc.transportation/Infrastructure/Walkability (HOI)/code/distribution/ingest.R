# NATIONAL WALKABILITY INDEX
options(timeout = max(1000, getOption("timeout")))

#url <- "https://edg.epa.gov/EPADataCommons/public/OA/SLD/SmartLocationDatabaseV3.zip"
#dest <- "./Infrastructure/data/original/SmartLocationDatabaseV3.zip"
#download.file(url, dest)

url <-"https://edg.epa.gov/EPADataCommons/public/OA/WalkabilityIndex.zip"
dest <- "./Infrastructure/data/original/WalkabilityIndex.zip"
download.file(url, dest)
