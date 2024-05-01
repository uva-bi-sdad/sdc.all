library(data.table)
library(R.utils)
library(readr)

# read data
geometa <- setDT(read.csv(xzfile("docs/distribution/geographies_metadata.csv.xz", open = "rw")))
adcwreg <- setDT(read.csv("docs/distribution/adcw_regions_usa_states_counties.csv", colClasses = "character"))

# write copy of old geo metadata file
fwrite(geometa, "docs/distribution/geographies_metadata.csv.old")

# add states
states <- adcwreg[nchar(adcwreg$geoid)==2, .(region_name=state, geoid)][, region_type := "state"][, year := 2021]
geometa <- rbindlist(list(geometa, states), use.names = T)

# add adcw ids
join <- merge(x = geometa, y = adcwreg, by = "geoid", all.x = T)[, .(region_type, region_name, year, geoid, adcw_id)]

# write updated file
write.csv(join, xzfile("docs/distribution/geographies_metadata.csv.xz"))
