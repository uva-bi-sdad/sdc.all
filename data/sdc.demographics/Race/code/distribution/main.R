# This file update all/specific data on the age topic

# load all functions
source('Race/code/distribution/functions.R')

# select the data that need to be updated. The default is true for all
run_data_prep('Race', ingest=TRUE, direct=TRUE, refine=TRUE)

# build function for updating demographics for each - idea is to just run this code every year (without touching ingest and model)

