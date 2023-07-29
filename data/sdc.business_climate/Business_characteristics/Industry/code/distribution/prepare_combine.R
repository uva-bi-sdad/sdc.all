# combine all data

# library -----------------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(reshape2)
library(crosstable)
library(tidyr)
library(scales)


# load all data
path = "Business_characteristics/Industry/data/distribution/"
files = list.files(path)
regions <- c('va059', 'ncr', 'rva')
metrics <- c('exit_by_industry', 'entry_by_industry', 'number_business_by_industry')

for (region in regions){
  temp0 <- files[str_detect(files,region)]
  for (metric in metrics){
    templist <- temp0[str_detect(temp0, metric)]
    data <- NULL
    for (file in templist){
      temp <- read_csv(paste0(path,file)) %>% select(geoid,year,measure,value,moe=MOE)
      data <- rbind(data,temp)
    }
    readr::write_csv(temp, xzfile(paste0(path,region,"_cttrbg_mi_",min(temp$year),max(temp$year),"_",metric,".csv.xz"), compression = 9))
  }
}


