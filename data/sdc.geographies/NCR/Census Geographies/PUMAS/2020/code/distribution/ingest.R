# gets census tract to puma crosswalk data -- 2020
# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html
# --> Relationship files identify the census tracts that are contained within a 
#     PUMA. Census tracts completely nest within PUMA boundaries and serve as 
#     the building block of PUMA geography.

# tract-puma crosswalk data
tr_puma_2020 <- read.csv('https://www2.census.gov/geo/docs/maps-data/data/rel2020/2020_Census_Tract_to_2020_PUMA.txt',
                         colClasses='character') 

path <- 'NCR/Census Geographies/PUMAS/2020/data/original/'
readr::write_csv(tr_puma_2020, 
                 xzfile(paste0(path, 'national_2020_tr_to_puma_crosswalk.csv.xz'), 
                        compression=9))