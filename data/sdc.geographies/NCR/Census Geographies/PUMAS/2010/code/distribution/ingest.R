# gets census tract to puma crosswalk -- 2010
# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html
# --> Relationship files identify the census tracts that are contained within a 
#     PUMA. Census tracts completely nest within PUMA boundaries and serve as 
#     the building block of PUMA geography.

# tract-puma crosswalk data
tr_puma_2010 <- read.csv('https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt', 
                         colClasses='character') 

path <- 'NCR/Census Geographies/PUMAS/2010/data/original/'
readr::write_csv(tr_puma_2010, 
                 xzfile(paste0(path, 'national_2010_tr_to_puma_crosswalk.csv.xz'), 
                        compression=9))
