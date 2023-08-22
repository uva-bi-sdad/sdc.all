# we estimate metrics for businesses entry by industry

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

uploadpath = "Microdata/Mergent_intellect/data/working/"
savepath = "Business_characteristics/Industry/data/distribution/"

# load function and parameters ---------------------------------------------------------------------
source('utils/distribution/functions.R')
geolevels <- c('tract','block group','county')
topic <- 'Industry'


# upload fairfax data  -----------------------------------------------------------------------------
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))
prefix <- 'va059'
temp_ffx <- business_dynamics(mi_fairfax_features,geolevels,topic,prefix,savepath,entry=TRUE,exit=TRUE,count=TRUE,size=TRUE)

# upload NCR data  --------------------------------------------------------------------
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))
prefix <- 'ncr'
temp_ncr <- business_dynamics(mi_ncr_features,geolevels,topic,prefix,savepath,entry=TRUE,exit=TRUE,count=TRUE,size=TRUE)

# upload Richmond city, Henrico county and Chesterfield county data  ------------------------------
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))
prefix <- 'rva'
temp_rva <- business_dynamics(mi_subva_features,geolevels,topic,prefix,savepath,entry=TRUE,exit=TRUE,count=TRUE,size=TRUE)

