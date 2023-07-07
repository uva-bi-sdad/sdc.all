"Source of Data: Childcare costs are obtained from Childcare Aware of Virginia's Child Care 
Supply Data by Locality. <https://vachildcare.com/data-2/child-care-supply-data-by-locality/>, 
specifically from the section detailing information on center-based care in each county's fact 
sheet. The report has values for five age ranges: infant(~0yrs.), toddler(~1yr.), 
pre-school(2-3yrs.), pre-school(4-5yrs.), and school-age(5-12yrs.). 

The raw data is weekly and at the county level. In order to determine monthly tract level data, 
all values were multiplied by a factor of 4 and each tract inherited the costs of the county in 
which it was located. 

Because the data was incomplete, values for counties without data were imputed by averaging the 
childcare costs for all counties surrounding them. This imputation was then applied iteratively 
until all counties had data."

library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
County_Shapes <- st_read("~/git/cost-living/data/County_Shapefile", int64_as_string = TRUE, stringsAsFactors = F)

VA_Counties <- County_Shapes %>%
  filter(STATEFP == 51)

VA_Childcare_Aware_Data <- read_excel("~/git/cost-living/Child care cost/data/Original/VA_Childcare_Aware_Data.xlsx")

VA_Childcare_Aware_Data$fips <- as.character(VA_Childcare_Aware_Data$fips)
VA_Counties$COUNTYFP <- as.character(VA_Counties$COUNTYFP)

VA_Childcare_Aware_Data <-
  merge(VA_Counties, VA_Childcare_Aware_Data, by.x = "COUNTYFP", by.y = "fips")

ggplot(VA_Childcare_Aware_Data, aes(fill = (VA_Childcare_Aware_Data$`ChildcareAware-2-3yr-avg` > 0))) + 
  geom_sf(data = VA_Childcare_Aware_Data$geometry) +
  labs(fill = 'Childcare Data')

VA_Childcare_Aware_Data[VA_Childcare_Aware_Data$`ChildcareAware-2-3yr-avg` == 0, "geometry"] <- 
  st_buffer(VA_Childcare_Aware_Data[VA_Childcare_Aware_Data$`ChildcareAware-2-3yr-avg` == 0, "geometry"], dist=1609.34*5)

ggplot(VA_Childcare_Aware_Data, aes(fill = (VA_Childcare_Aware_Data$`ChildcareAware-2-3yr-avg` > 0))) + 
  geom_sf(data = VA_Childcare_Aware_Data$geometry) +
  labs(fill = 'Childcare Data')

for (k in 10:24){
  while(0 %in% dplyr::select(as.data.frame(VA_Childcare_Aware_Data), -geometry)[,k]){
    for (i in 1:length(VA_Childcare_Aware_Data$`ChildcareAware-2-3yr-avg`)){
      count <- 0
      total <- 0
      if (dplyr::select(as.data.frame(VA_Childcare_Aware_Data), -geometry)[i,k] == 0){
        for (j in 1:length(VA_Childcare_Aware_Data$`ChildcareAware-2-3yr-avg`)){
          if (dplyr::select(as.data.frame(VA_Childcare_Aware_Data), -geometry)[j,k] != 0){
            if (st_intersects(VA_Childcare_Aware_Data$geometry[i],
                              VA_Childcare_Aware_Data$geometry[j], sparse = FALSE)){
              total <- total + dplyr::select(as.data.frame(VA_Childcare_Aware_Data), -geometry)[j,k]
              count <- count + 1}
          }
        }
      }
      if (count > 0){
        VA_Childcare_Aware_Data[i,k] <- total/count}
    }}}

VA_Childcare_Costs <- select(as.data.frame(VA_Childcare_Aware_Data), -geometry)
VA_Childcare_Costs <- VA_Childcare_Costs[,c(5:6, 10:24)]

va_tracts <- st_read("~/git/cost-living/data/Shapefile_VA_tract", int64_as_string = TRUE, stringsAsFactors = F)
va_tracts <- dplyr::select(as.data.frame(va_tracts), -geometry)[,c(1:2, 5)]
colnames(va_tracts) <- c("State", "County", "Tract")
va_tracts$GEOID <- paste0(va_tracts$State, va_tracts$County)
va_tracts <- va_tracts[,c(3, 4)]

VA_Childcare_Costs$GEOID <- as.character(VA_Childcare_Costs$GEOID)
VA_Childcare_Costs <- left_join(va_tracts, VA_Childcare_Costs, by="GEOID")
VA_Childcare_Costs <- VA_Childcare_Costs[,-3]

write.csv(VA_Childcare_Costs,"~/git/cost-living/Child care cost/data/Working/va_tr_ccava_2022_childcarecosts.csv")
