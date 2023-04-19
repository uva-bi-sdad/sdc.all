library(readr)
library(fuzzyjoin)

# working directory
setwd("~/git/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates")

# check how many institutions
#college_degrees <- read_csv("~/git/sdc.health_dev/Health Care Services/Health Professionals/VA Graduates/data/working/College Degrees.csv")
#length(table(college_degrees$Institution))

grads <- read_csv("data/original/all_institutions_grad_students.csv")
profs <- read_csv("data/original/all_institutions_first_prof.csv")
undergrads_pub <- read_csv("data/original/four_year_public_undergrad.csv")
undergrads_priv <- read_csv("data/original/four_year_private_undergrad.csv")
two_year <- read_csv("data/original/two_year_undergrad.csv")

colnames(grads) <- c("county_name", "2016", "2017", "2018", "2019", "2020")
colnames(profs) <- c("county_name", "2016", "2017", "2018", "2019", "2020")
colnames(two_year) <- c("county_name", "2016", "2017", "2018", "2019", "2020")
colnames(undergrads_priv) <- c("county_name", "2016", "2017", "2018", "2019", "2020")
colnames(undergrads_pub) <- c("county_name", "2016", "2017", "2018", "2019", "2020")

undergrads_pub <- undergrads_pub[1:(length(undergrads_pub$county_name)-3), ]
undergrads_priv <- undergrads_priv[1:(length(undergrads_priv$county_name)-2), ]
undergrads <- as.data.frame(undergrads_pub$county_name)
undergrads["2016"] <- undergrads_priv$`2016` + undergrads_pub$`2016`
undergrads["2017"] <- undergrads_priv$`2017` + undergrads_pub$`2017`
undergrads["2018"] <- undergrads_priv$`2018` + undergrads_pub$`2018`
undergrads["2019"] <- undergrads_priv$`2019` + undergrads_pub$`2019`
undergrads["2020"] <- undergrads_priv$`2020` + undergrads_pub$`2020`
names(undergrads)[1] <- "county_name"

# geographies
geos_data <- read_csv("~/git/dc.metadata/data/region_name.csv.xz")
va_counties <- geos_data %>% filter(region_type == "county" & substr(geoid, 1,2) == "51")

#######################
# FORMAT
#######################

# add geographies names (distance = "virginia" + space)
out_under <- stringdist_left_join(undergrads, va_counties, by = c("county_name" = "region_name"), max_dist = 11)
out_under <- out_under[!is.na(out_under$geoid),]
out_under <- out_under %>% select(-c(county_name, `2020`))
#write_csv(out_under, "data/working/out_under.csv")

out_grad <- stringdist_left_join(grads, va_counties, by = c("county_name" = "region_name"), max_dist = 11)
out_grad <- out_grad[!is.na(out_grad$geoid),]
out_grad <- out_grad %>% select(-c(county_name, `2020`))
#write_csv(out_grad, "data/working/out_grad.csv")

out_profs <- stringdist_left_join(profs, va_counties, by = c("county_name" = "region_name"), max_dist = 11)
out_profs <- out_profs[!is.na(out_profs$geoid),]
out_profs <- out_profs %>% select(-c(county_name, `2020`))
#write_csv(out_profs, "data/working/out_profs.csv")

out_two <- stringdist_left_join(two_year, va_counties, by = c("county_name" = "region_name"), max_dist = 11)
out_two <- out_two[!is.na(out_two$geoid),]
out_two <- out_two %>% select(-c(county_name, `2020`))
#write_csv(out_two, "data/working/out_two.csv")

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
write_csv(out_df, "data/distribution/va_ct_schev_2016_2019_health_degrees_awarded.csv")
