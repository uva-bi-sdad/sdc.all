
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
savepath = "Employment/Industry/data/distribution/"

# load function and parameters ---------------------------------------------------------------------
source('function/distribution/functions.R')
geolevels <- c('tract','block group','county')
topic <- 'Industry'


# upload fairfax data  -----------------------------------------------------------------------------
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_bg.csv.xz"))
prefix <- 'va059'
temp_ffx <- employment_dynamics(mi_fairfax_features,geolevels,topic,prefix,savepath,job_creation=TRUE,job_destruction=TRUE,job_count=TRUE)
temp_ffx_hhi <- industry_concentration(mi_fairfax_features,prefix)
temp_ffx_lq <- location_quotient(mi_fairfax_features,geolevels)


# upload NCR data  --------------------------------------------------------------------
mi_ncr_features <-  read_csv(paste0(uploadpath,"mi_ncr_features_bg.csv.xz"))
prefix <- 'ncr'
temp_ncr <- employment_dynamics(mi_ncr_features,geolevels,topic,prefix,savepath,job_creation=TRUE,job_destruction=TRUE,job_count=TRUE)
temp_ncr_hhi <- industry_concentration(mi_ncr_features,prefix)
temp_ncr_lq <- location_quotient(mi_ncr_features,geolevels)


# upload Richmond city, Henrico county and Chesterfield county data  ------------------------------
mi_subva_features <-  read_csv(paste0(uploadpath,"mi_subva_features_bg.csv.xz"))
prefix <- 'rva'
temp_rva <- employment_dynamics(mi_subva_features,geolevels,topic,prefix,savepath,job_creation=TRUE,job_destruction=TRUE,job_count=TRUE)
temp_rva_hhi <- industry_concentration(mi_subva_features,prefix)
temp_rva_lq <- location_quotient(mi_subva_features,geolevels)


