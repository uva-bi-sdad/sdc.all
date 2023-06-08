library(data.table)
library(micropan)

dat <- fread(input = "Years of Schooling/data/working/ncr_tr_acs5_2017_2021_years_of_schooling.csv")
dat20172019 <- dat[year %in% c(2017, 2018, 2019), .(geoid = as.character(geoid), year, measure, measure_type = "index", value)]
dat20202021 <- dat[year %in% c(2020, 2021), .(geoid = as.character(geoid), year, measure, measure_type = "index", value)]

# geographies
download.file("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/docs/distribution/geographies_metadata.csv.xz", "Years of Schooling/data/working/geographies_metadata.csv.xz")
xzuncompress("Years of Schooling/data/working/geographies_metadata.csv.xz")

geo_names <- fread("Years of Schooling/data/working/geographies_metadata.csv")
geo_names <- geo_names[!region_name %like% "District Of Columbia" & 
                         !region_name %like% "Manassas Park city" & 
                         !region_name %like% "Fairfax city" &
                         !region_name %like% "Alexandria city" &
                         !region_name %like% "Manassas city" &
                         !region_name %like% "Falls Church city"]

geo_names_10 <- geo_names[year == "2010", .(geoid, region_name, region_type)]
geo_names_20 <- geo_names[year == "2020", .(geoid, region_name, region_type)]

dat20172019_mrg <- unique(merge(dat20172019, geo_names_10, by = "geoid"))
dat20202021_mrg <- unique(merge(dat20202021, geo_names_20, by = "geoid"))

dat_mrg <- rbindlist(list(dat20172019_mrg, dat20202021_mrg))

dat_fin <- dat_mrg[,.(geoid, measure, measure_type, region_name, region_type, value, year, moe = "")]

fwrite(dat_fin, "Years of Schooling/data/distribution/ncr_tr_acs5_2017_2021_years_of_schooling.csv")
