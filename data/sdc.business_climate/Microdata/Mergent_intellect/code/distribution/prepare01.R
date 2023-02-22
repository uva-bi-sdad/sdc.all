# Concatenate together files downloaded from Mergent Intellect 

# packages
library(readxl)
library(tidyverse)
library(readr)
library(reshape2)

# working directory
path = "ownership_diversity/VA/fairfax/overall/data/original/mergent_intellect/"

# concatenate csv into one file
for (data in list.files(path)){
  print(data)
  # Create the first data if no data exist yet
  if (!exists("companies")){
    companies <- read_excel(paste0(path,data), sheet = 1)
    companies <- companies[!is.na(companies$`Company Name`),]
  }
  
  if (!exists("financial")){
    financial <- read_excel(paste0(path,data), sheet = 2)
    # reshape into long format beacause # of columns (finacial indicators over years) is unequal
    financial <- melt(financial,
                          id.vars=c("D-U-N-S@ Number", "Company Name"),
                          variable.name="measure",
                          value.name="value")
    # measure is in levels -> character
    financial$measure <- as.character(financial$measure)
    
    # get year
    financial['year'] =  str_sub(financial$measure,1,5)
    financial$year[financial$year %in% c("Sales", "Emplo")] <- "All"
    financial$measure[financial$year != "All"] <- str_sub(financial$measure[financial$year != "All"],6,-1)
    
    # drop NAs to decrese number of observations
    financial <- financial[!is.na(financial$value),]
    
  }
  if (!exists("executive")){
    executive <- read_excel(paste0(path,data), sheet = 3)
    executive <- executive[!is.na(executive$`Company Name`),]
  }
  
  # if data already exist, then append it together
  if (exists("companies")){
    tempory1 <-read_excel(paste0(path,data), sheet = 1)
    tempory1 <- tempory1[!is.na(tempory1$`Company Name`),]
    companies <-unique(rbind(companies, tempory1))
    rm(tempory1)
  }
  if (exists("financial")){
    tempory2 <-read_excel(paste0(path,data), sheet = 2)
    # reshape into long format beacause # of columns (finacial indicators over years) is unequal
    tempory2 <- melt(tempory2,
                      id.vars=c("D-U-N-S@ Number", "Company Name"),
                      variable.name="measure",
                      value.name="value")
    # measure is in levels -> character
    tempory2$measure <- as.character(tempory2$measure)
    # get year
    tempory2['year'] =  str_sub(tempory2$measure,1,5)
    tempory2$year[tempory2$year %in% c("Sales", "Emplo")] <- "All"
    tempory2$measure[tempory2$year != "All"] <- str_sub(tempory2$measure[financial$year != "All"],6,-1)
    
    # drop NAs to decrease number of observations
    tempory2 <- tempory2[!is.na(tempory2$value),]
    
    financial <-unique(rbind(financial, tempory2))
    rm(tempory2)
  }
  if (exists("executive")){
    tempory3 <-read_excel(paste0(path,data), sheet = 3)
    tempory3 <- tempory3[!is.na(tempory3$`Company Name`),]
    executive <-unique(rbind(executive, tempory3))
    rm(tempory3)
  }
}


# save working data
savepath = "ownership_diversity/VA/fairfax/overall/data/working/"
readr::write_csv(companies, xzfile(paste(savepath,"mi_companies_details.csv.xz"), compression = 9))
readr::write_csv(financial, xzfile(paste(savepath,"mi_financial_info.csv.xz"), compression = 9))
readr::write_csv(executive, xzfile(paste(savepath,"mi_executives.csv.xz"), compression = 9))
