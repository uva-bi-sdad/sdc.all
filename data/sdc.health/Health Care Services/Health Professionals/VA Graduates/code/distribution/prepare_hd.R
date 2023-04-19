# Aggregate the number of graduates in health professions to health distrcit geographies

# packages
library(tidyverse)
library(readr)
library(RPostgreSQL)

# working directory
setwd("~/git/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates")

###################
# LOAD COUNTY DATA
###################
under <- read_csv("data/working/out_under.csv")
under$geoid <- as.character(under$geoid)
grad <- read_csv("data/working/out_grad.csv")
grad$geoid <- as.character(grad$geoid)
profs <- read_csv("data/working/out_profs.csv")
profs$geoid <- as.character(profs$geoid)
two <- read_csv("data/working/out_two.csv")
two$geoid <- as.character(two$geoid)
################### 
# HEALTH DISTRICTS
###################

# connect to database
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 user = "hc2cc",
                 password = "hc2cchc2cc")
# read in health districts
health_district <- dbGetQuery(con, "SELECT * FROM dc_common.va_hdct_sdad_2021_health_district_counties")
dbDisconnect(con)

under <- left_join(under[, c("2016", "2017", "2018", "2019", "geoid")], health_district[, c("geoid_county", "geoid")],
               by = c("geoid" = "geoid_county"))
grad <- left_join(grad[, c("2016", "2017", "2018", "2019", "geoid")], health_district[, c("geoid_county", "geoid")],
                   by = c("geoid" = "geoid_county"))
profs <- left_join(profs[, c("2016", "2017", "2018", "2019", "geoid")], health_district[, c("geoid_county", "geoid")],
                  by = c("geoid" = "geoid_county"))
two <- left_join(two[, c("2016", "2017", "2018", "2019", "geoid")], health_district[, c("geoid_county", "geoid")],
                   by = c("geoid" = "geoid_county"))

#####################################
# AGGREGATE TO HEALTH DISTRICT LEVEL
#####################################
under <- under  %>%
  group_by(geoid.y) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() %>% filter(!is.na(geoid.y))
names(under)[1] <- "short_geoid"

grad <- grad  %>%
  group_by(geoid.y) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() %>% filter(!is.na(geoid.y))
names(grad)[1] <- "short_geoid"

two <- two  %>%
  group_by(geoid.y) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() %>% filter(!is.na(geoid.y))
names(two)[1] <- "short_geoid"

profs <- profs  %>%
  group_by(geoid.y) %>%
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>%
  ungroup() %>% filter(!is.na(geoid.y))
names(profs)[1] <- "short_geoid"

# geographies
geos_data <- read_csv("~/git/dc.metadata/data/region_name.csv.xz")
va_hd <- geos_data %>% filter(region_type == "health district" & substr(geoid, 1,2) == "51")
va_hd["short_geoid"] <- substr(va_hd$geoid, 7,8)

#######################
# FORMAT
#######################

# add geographies names
out_under <- left_join(under, va_hd, by = c("short_geoid" = "short_geoid"))
out_under <- out_under %>% select(-c(short_geoid))
write_csv(out_under, "data/working/under_hd.csv")

out_grad <- left_join(grad, va_hd, by = c("short_geoid" = "short_geoid"))
out_grad <- out_grad %>% select(-c(short_geoid))
write_csv(out_grad, "data/working/grad_hd.csv")

out_profs <- left_join(profs, va_hd, by = c("short_geoid" = "short_geoid"))
out_profs <- out_profs %>% select(-c(short_geoid))
write_csv(out_profs, "data/working/profs_hd.csv")

out_two <- left_join(two, va_hd, by = c("short_geoid" = "short_geoid"))
out_two <- out_two %>% select(-c(short_geoid))
write_csv(out_two, "data/working/two_hd.csv")

# long format
out_under_long <- melt(out_under,
                       id.vars=c("geoid", "region_type", "region_name"),
                       variable.name="year",
                       value.name="value"
)

out_grad_long <- melt(out_grad,
                      id.vars=c("geoid", "region_type", "region_name"),
                      variable.name="year",
                      value.name="value"
)

out_prof_long <- melt(out_profs,
                      id.vars=c("geoid", "region_type", "region_name"),
                      variable.name="year",
                      value.name="value"
)

out_two_long <- melt(out_two,
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
write_csv(out_df, "data/distribution/va_hd_schev_2016_2019_health_degrees_awarded.csv")
