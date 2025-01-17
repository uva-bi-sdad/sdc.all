# Aggregate the number of graduates in health professions to AHEC geographies
# AHEC geographies are taken from Virginia Health Workworce Development Authority (VHWDA) https://www.vhwda.org/initiatives/ahec

# The Blue Ridge region includes the counties of Albemarle, Augusta, Bath, Clarke, Culpeper,
# Fauquier, Frederick, Greene, Highland, Loudoun, Louisa, Madison, Nelson, Orange, 
# Page, Rappahannock, Rockbridge, Shenandoah, Warren 
# and the cities of Buena Vista, Charlottesville, Harrisonburg, Lexington, Staunton, 
# Waynesboro and Winchester

# The Capital region includes Charles City, Chesterfield, Colonial Heights, 
# Goochland, Hanover, Henrico, New Kent, Powhatan and Richmond City

# The Northern Virginia region includes the counties of Arlington, Fairfax, 
# Loudoun and Prince William, and the cities of Alexandria, Fairfax, Falls Church, 
# Manassas and Manassas Park

# Eastern Virginia: The counties of Accomack, Isle of Wight, James City, Northampton, Southampton, and 
# York and the cities of Chesapeake, Franklin, Hampton, Newport News, 
# Norfolk, Poquoson, Portsmouth, Suffolk, Virginia Beach, and Williamsburg

# The counties and cities in the Rappahannock Region include Caroline, Essex, 
# Fredericksburg, Gloucester, King George, King and Queen, King William, 
# Lancaster, Mathews, Middlesex, Northumberland, Richmond County, Spotsylvania, 
# Stafford, and Westmoreland

# The South Central region of Virginia serves the counties of Amherst, Appomattox, 
# Bedford, Campbell, Franklin, Henry, Patrick, and Pittsylvania and the cities of 
# Bedford, Danville, Lynchburg and Martinsville

# The Southside region includes the counties of Amelia, Brunswick, Buckingham, Charlotte, 
# Cumberland, Dinwiddie, Greensville, Halifax, Lunenburg, Mecklenburg, Nottoway, 
# Prince Edward, Prince George, Surry, and Sussex, and the cities of Emporia, Hopewell, 
# and Petersburg

# The Southwest region includes the counties of Alleghany, Bland, Botetourt, Buchanan, 
# Carroll, Craig, Dickenson, Floyd, Giles, Grayson, Lee, Montgomery, Pulaski, 
# Roanoke, Russell, Scott, Smyth, Tazewell, Washington, Wise, and Wythe, 
# and the cities of Bristol, Covington, Galax, Norton, Radford, Roanoke, and Salem

# packages
library(tidyverse)
library(readr)
library(stringr)
library(readxl)
library(reshape2)


# working directory
setwd("~/git/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates")

###################
# LOAD COUNTY DATA
###################
under <- read_csv("/Users/avagutshall/Desktop/folder/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates/data/working/out_under.csv")
under$geoid <- as.character(under$geoid)
grad <- read_csv("/Users/avagutshall/Desktop/folder/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates/data/working/out_grad.csv")
grad$geoid <- as.character(grad$geoid)
profs <- read_csv("/Users/avagutshall/Desktop/folder/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates/data/working/out_profs.csv")
profs$geoid <- as.character(profs$geoid)
two <- read_csv("/Users/avagutshall/Desktop/folder/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates/data/working/out_two.csv")
two$geoid <- as.character(two$geoid)

######################
# AHEC REGIONS
######################

# load AHEC regions (shared by Rexford Anson-Dwamena)
ahec <- read_excel("/Users/avagutshall/Desktop/folder/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates/data/original/AHEC_RUCAs.xlsx")
# correct Blue Ridge AHEC name
ahec$`AHEC Name`[ahec$`AHEC Code` == 1] = "Blue Ridge AHEC"

###########################
# blue_ridge <- c("Albemarle", "Augusta", "Bath", "Clarke", "Culpeper", "Fauquier", 
#                 "Frederick", "Greene", "Highland", "Loudoun", "Louisa", "Madison", 
#                 "Nelson", "Orange", "Page", "Rappahannock", "Rockbridge", "Rockingham", 
#                 "Shenandoah", "Warren", "Fluvanna",
#                 "Buena Vista", "Charlottesville", "Harrisonburg", 
#                 "Lexington", "Staunton", "Waynesboro", "Winchester")
# 
# capital <- c("Charles", "Chesterfield", "Colonial Heights", "Goochland", "Hanover", 
#              "Henrico", "New Kent", "Powhatan", "Richmond city")
# 
# north_virginia <- c("Arlington", "Fairfax", "Loudoun", "Prince William", 
#                     "Alexandria", "Fairfax", "Falls Church", "Manassas", "Manassas Park")
# 
# eastern_virginia <- c("Accomack", "Isle of Wight", "James City", "Northampton", "Southampton",
#                       "York", "Chesapeake", "Franklin", "Hampton", "Newport News", 
#                       "Norfolk", "Poquoson", "Portsmouth", "Suffolk", "Virginia Beach", 
#                       "Williamsburg")
# 
# rappahannock <- c("Caroline", "Essex", "Fredericksburg", "Gloucester", "King George",
#                   "King and Queen", "King William", "Lancaster", "Mathews", "Middlesex", 
#                   "Northumberland", "Richmond County", "Spotsylvania", "Stafford", "Westmoreland")
# 
# south_central <- c("Amherst", "Appomattox", "Bedford", "Campbell", "Franklin", "Henry", 
#                    "Patrick", "Pittsylvania", "Bedford", "Danville", "Lynchburg", "Martinsville")
# 
# southside <- c("Amelia", "Brunswick", "Buckingham", "Charlotte", "Cumberland", "Dinwiddie",
#                "Greensville", "Halifax", "Lunenburg", "Mecklenburg", "Nottoway", 
#                "Prince Edward", "Prince George", "Surry", "Sussex", "Emporia", "Hopewell", "Petersburg")
# 
# southwest <- c("Alleghany", "Bland", "Botetourt", "Buchanan", "Carroll", "Craig", 
#                "Dickenson", "Floyd", "Giles", "Grayson", "Lee", "Montgomery", "Pulaski", 
#                "Roanoke", "Russell", "Scott", "Smyth", "Tazewell", "Washington", "Wise",
#                "Wythe", "Bristol", "Covington", "Galax", "Norton", "Radford", "Salem")
# 
# # Rokingham and Fluvanna counties are omitted. Blue Ridge?
# # Loudoun county shows up twice
#######################################

under$region_name[under$geoid %in% ahec$FIPS] <- ahec$`AHEC Name`
# Bedford City is missing in the list, add to South Central region
under$region_name[under$region_name == "Bedford city, Virginia"] <- "South Central AHEC"

grad$region_name[grad$geoid %in% ahec$FIPS] <- ahec$`AHEC Name`
# Bedford City is missing in the list, add to South Central region
grad$region_name[grad$region_name == "Bedford city, Virginia"] <- "South Central AHEC"

profs$region_name[profs$geoid %in% ahec$FIPS] <- ahec$`AHEC Name`
# Bedford City is missing in the list, add to South Central region
profs$region_name[profs$region_name == "Bedford city, Virginia"] <- "South Central AHEC"

two$region_name[two$geoid %in% ahec$FIPS] <- ahec$`AHEC Name`
# Bedford City is missing in the list, add to South Central region
two$region_name[two$region_name == "Bedford city, Virginia"] <- "South Central AHEC"


# under$region_name[grepl(paste(blue_ridge, collapse="|"),under$region_name) == T] <- "Blue Ridge"
# under$region_name[grepl(paste(capital, collapse="|"),under$region_name) == T] <- "Capital"
# under$region_name[grepl(paste(north_virginia, collapse="|"),under$region_name) == T] <- "Northern Virginia"
# under$region_name[grepl(paste(eastern_virginia, collapse="|"),under$region_name) == T] <- "Eastern Virginia"
# under$region_name[grepl(paste(rappahannock, collapse="|"),under$region_name) == T] <- "Rappahannock"
# under$region_name[grepl(paste(south_central, collapse="|"),under$region_name) == T] <- "South Central"
# under$region_name[grepl(paste(southside, collapse="|"),under$region_name) == T] <- "Southside"
# under$region_name[grepl(paste(southwest, collapse="|"),under$region_name) == T] <- "Southwest"
# 
# grad$region_name[grepl(paste(blue_ridge, collapse="|"),grad$region_name) == T] <- "Blue Ridge"
# grad$region_name[grepl(paste(capital, collapse="|"),grad$region_name) == T] <- "Capital"
# grad$region_name[grepl(paste(north_virginia, collapse="|"),grad$region_name) == T] <- "Northern Virginia"
# grad$region_name[grepl(paste(eastern_virginia, collapse="|"),grad$region_name) == T] <- "Eastern Virginia"
# grad$region_name[grepl(paste(rappahannock, collapse="|"),grad$region_name) == T] <- "Rappahannock"
# grad$region_name[grepl(paste(south_central, collapse="|"),grad$region_name) == T] <- "South Central"
# grad$region_name[grepl(paste(southside, collapse="|"),grad$region_name) == T] <- "Southside"
# grad$region_name[grepl(paste(southwest, collapse="|"),grad$region_name) == T] <- "Southwest"
# 
# profs$region_name[grepl(paste(blue_ridge, collapse="|"),profs$region_name) == T] <- "Blue Ridge"
# profs$region_name[grepl(paste(capital, collapse="|"),profs$region_name) == T] <- "Capital"
# profs$region_name[grepl(paste(north_virginia, collapse="|"),profs$region_name) == T] <- "Northern Virginia"
# profs$region_name[grepl(paste(eastern_virginia, collapse="|"),profs$region_name) == T] <- "Eastern Virginia"
# profs$region_name[grepl(paste(rappahannock, collapse="|"),profs$region_name) == T] <- "Rappahannock"
# profs$region_name[grepl(paste(south_central, collapse="|"),profs$region_name) == T] <- "South Central"
# profs$region_name[grepl(paste(southside, collapse="|"),profs$region_name) == T] <- "Southside"
# profs$region_name[grepl(paste(southwest, collapse="|"),profs$region_name) == T] <- "Southwest"
# 
# two$region_name[grepl(paste(blue_ridge, collapse="|"),two$region_name) == T] <- "Blue Ridge"
# two$region_name[grepl(paste(capital, collapse="|"),two$region_name) == T] <- "Capital"
# two$region_name[grepl(paste(north_virginia, collapse="|"),two$region_name) == T] <- "Northern Virginia"
# two$region_name[grepl(paste(eastern_virginia, collapse="|"),two$region_name) == T] <- "Eastern Virginia"
# two$region_name[grepl(paste(rappahannock, collapse="|"),two$region_name) == T] <- "Rappahannock"
# two$region_name[grepl(paste(south_central, collapse="|"),two$region_name) == T] <- "South Central"
# two$region_name[grepl(paste(southside, collapse="|"),two$region_name) == T] <- "Southside"
# two$region_name[grepl(paste(southwest, collapse="|"),two$region_name) == T] <- "Southwest"

under$region_type <- "ahec region"
grad$region_type <- "ahec region"
profs$region_type <- "ahec region"
two$region_type <- "ahec region"

under <- under %>% 
  mutate(geoid = case_when(
    region_name == "Blue Ridge AHEC" ~ "51_ahec_01",
    region_name == "Capital AHEC" ~ "51_ahec_02",
    region_name == "Northern Virginia AHEC" ~ "51_ahec_04",
    region_name == "Eastern Virginia AHEC" ~ "51_ahec_03",
    region_name == "Rappahannock AHEC" ~ "51_ahec_05",
    region_name == "South Central AHEC" ~ "51_ahec_07",
    region_name == "Southside AHEC" ~ "51_ahec_06",
    region_name == "Southwest Virginia AHEC" ~ "51_ahec_08"))

grad <- grad %>% 
  mutate(geoid = case_when(
    region_name == "Blue Ridge AHEC" ~ "51_ahec_01",
    region_name == "Capital AHEC" ~ "51_ahec_02",
    region_name == "Northern Virginia AHEC" ~ "51_ahec_04",
    region_name == "Eastern Virginia AHEC" ~ "51_ahec_03",
    region_name == "Rappahannock AHEC" ~ "51_ahec_05",
    region_name == "South Central AHEC" ~ "51_ahec_07",
    region_name == "Southside AHEC" ~ "51_ahec_06",
    region_name == "Southwest Virginia AHEC" ~ "51_ahec_08"))

profs <- profs %>% 
  mutate(geoid = case_when(
    region_name == "Blue Ridge AHEC" ~ "51_ahec_01",
    region_name == "Capital AHEC" ~ "51_ahec_02",
    region_name == "Northern Virginia AHEC" ~ "51_ahec_04",
    region_name == "Eastern Virginia AHEC" ~ "51_ahec_03",
    region_name == "Rappahannock AHEC" ~ "51_ahec_05",
    region_name == "South Central AHEC" ~ "51_ahec_07",
    region_name == "Southside AHEC" ~ "51_ahec_06",
    region_name == "Southwest Virginia AHEC" ~ "51_ahec_08"))

two <- two %>% 
  mutate(geoid = case_when(
    region_name == "Blue Ridge AHEC" ~ "51_ahec_01",
    region_name == "Capital AHEC" ~ "51_ahec_02",
    region_name == "Northern Virginia AHEC" ~ "51_ahec_04",
    region_name == "Eastern Virginia AHEC" ~ "51_ahec_03",
    region_name == "Rappahannock AHEC" ~ "51_ahec_05",
    region_name == "South Central AHEC" ~ "51_ahec_07",
    region_name == "Southside AHEC" ~ "51_ahec_06",
    region_name == "Southwest Virginia AHEC" ~ "51_ahec_08"))


under <- under  %>%
  group_by(geoid, region_name, region_type) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() 
write_csv(under, "/Users/avagutshall/Downloads/under_ahec.csv")

grad <- grad  %>%
  group_by(geoid, region_name, region_type) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() 
write_csv(grad, "/Users/avagutshall/Downloads/grad_ahec.csv")

profs <- profs  %>%
  group_by(geoid, region_name, region_type) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup()
write_csv(profs, "/Users/avagutshall/Downloads/profs_ahec.csv")

two <- two  %>%
  group_by(geoid, region_name, region_type) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() 
write_csv(two, "/Users/avagutshall/Downloads/two_ahec.csv")

# long format
out_under_long <- melt(under,
                       id.vars=c("geoid", "region_type", "region_name"),
                       variable.name="year",
                       value.name="value"
)

out_grad_long <- melt(grad,
                      id.vars=c("geoid", "region_type", "region_name"),
                      variable.name="year",
                      value.name="value"
)

out_prof_long <- melt(profs,
                      id.vars=c("geoid", "region_type", "region_name"),
                      variable.name="year",
                      value.name="value"
)

out_two_long <- melt(two,
                     id.vars=c("geoid", "region_type", "region_name"),
                     variable.name="year",
                     value.name="value"
)

out_under_long["measure"] <- "degrees_awarded_undergraduate"
out_under_long["measure_type"] <- "count"
# re-arrange columns
out_under_long <- out_under_long[, c(1, 2, 3, 4, 6, 5, 7)]

out_grad_long["measure"] <- "degrees_awarded_graduate"
out_grad_long["measure_type"] <- "count"
# re-arrange columns
out_grad_long <- out_grad_long[, c(1, 2, 3, 4, 6, 5, 7)]

out_prof_long["measure"] <- "degrees_awarded_first_professional"
out_prof_long["measure_type"] <- "count"
# re-arrange columns
out_prof_long <- out_prof_long[, c(1, 2, 3, 4, 6, 5, 7)]

out_two_long["measure"] <- "degrees_awarded_two_year"
out_two_long["measure_type"] <- "count"
# re-arrange columns
out_two_long <- out_two_long[, c(1, 2, 3, 4, 6, 5, 7)]

out_df <- rbind(out_under_long, out_grad_long, out_prof_long, out_two_long)

# save to distribution folder
# write_csv(out_df, "data/distribution/va_ahec_schev_2016_2019_health_degrees_awarded.csv")
readr::write_csv(out_df, xzfile("/Users/avagutshall/Downloads/va_ahec_schev_2016_2023_health_degrees_awarded.csv.xz", compression = 9))
