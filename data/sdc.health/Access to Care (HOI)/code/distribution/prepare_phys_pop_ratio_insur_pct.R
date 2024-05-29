library(data.table)
library(readr)

# Prepare Data
## primary care physicians with tract
phys_tract <- fread("Access to Care (HOI)/data/working/va_2017_2021_primary_care_phys_by_tract.csv")
phys_tract <- phys_tract[!is.na(Tract_FIPS)]

## distances between tract centroids
if (exists("tract_distances")) rm(tract_distances)
for (i in 1:6) {
  trdist <- readr::read_csv(paste0("../tract_distance_matrix/data/tract2tract_dist_time_2020/tract2tract_dist_time_", i,".zip"))
  if (!exists("tract_distances")) tract_distances <- trdist else tract_distances <- rbindlist(list(tract_distances, trdist))
}
tract_distances <- tract_distances[!is.na(tract_dest)]
tract_distances[, tract_orig := as.character(tract_orig)]
tract_distances[, tract_dest := as.character(tract_dest)]

## population and count of non-insured by tract 
pop_insur_tract <- fread("Access to Care (HOI)/data/working/va_tr_2017_2021_tot_pop_tot_insur.csv")
pop_insur_tract <- dcast(pop_insur_tract, geoid+year ~ measure, value.var = "value")
pop_insur_tract <- pop_insur_tract[, .(geoid = as.character(geoid), year, tot_pop_cnt = B01001_001, no_ins_cnt = B27010_033 + B27010_050)]


# Create Physician Count Within 30mi of Tract Centroid
## physician count per tract 
phys_per_tract <- phys_tract[, .(count = .N), by=.(Tract_FIPS, Year)][order(Tract_FIPS, Year)]
phys_per_tract <- phys_per_tract[, .(geoid = Tract_FIPS, year = Year, phys_cnt = count)]
phys_per_tract[, geoid := as.character(geoid)]

## all tracts that are within 30 miles of other tracts
tract_distances_lt30mi <- tract_distances[dist_meters <= 48280.3, .(geoid = tract_orig, geoid30 = tract_dest)]
## add row for origin and destination being same tract (removed from original dataset)
self_tract <- data.table::data.table(geoid = unique(tract_distances_lt30mi$geoid), geoid30 = unique(tract_distances_lt30mi$geoid))
tract_distances_lt30mi <- rbindlist(list(tract_distances_lt30mi, self_tract))

## join physician counts per tract with tracts within 30 miles
tract_distances_lt30mi_phys <- merge(tract_distances_lt30mi, phys_per_tract, by.x = "geoid30", by.y = "geoid", allow.cartesian = T)
tract_distances_lt30mi_phys <- tract_distances_lt30mi_phys[, .(geoid, geoid30, year, phys_cnt)]

## for each tract, sum up physician count for all tracts within 30 miles
phys_tract_30mi <- tract_distances_lt30mi_phys[, .(geoid, year, phys_cnt)][, phys_30_cnt := sum(phys_cnt), by = c("geoid", "year")]
phys_tract_30mi <- unique(phys_tract_30mi[, .(geoid, year, phys_30_cnt)])

## write file
fwrite(phys_tract_30mi, "Access to Care (HOI)/data/distribution/va_cnt_physician_lt_30mi_from_tract.csv")


# Join Physicians Within 30 Miles per Tract to Population and Uninsured Count
## just 2020 and 2021 until tract distances is updated
## pop_insur_tract_20_21 <- pop_insur_tract[year %in% c(2020, 2021)]
phy_pop_insur_tract <- merge(pop_insur_tract, phys_tract_30mi, by = c("geoid", "year"), all.x = T)
 # phy_pop_insur_tract <- phy_pop_insur_tract[, phys_cnt := as.double(phys_cnt)]
 # phy_pop_insur_tract[is.na(phys_cnt), phys_cnt := 0.0001]

# Calculate Index
## calculate z-scores of the population-to-physician ratio
phy_pop_insur_tract[, pop_phys := tot_pop_cnt/phys_30_cnt]
phy_pop_insur_tract[, pop_phys_z := scale(pop_phys)]
## calculate z-score of uninsured population
phy_pop_insur_tract[, no_ins_pct := (no_ins_cnt/tot_pop_cnt) * 100]
phy_pop_insur_tract[, no_ins_pct_z := scale(no_ins_pct)]
## sum up z-scores (the population to provider z score and the uninsured population z score)
## into a composite, and calculate z score of composite
phy_pop_insur_tract[, sum_z := scale(pop_phys_z + no_ins_pct_z)]
## reverse z-scores
phy_pop_insur_tract[, access_index := sum_z * -1]

## write calculation file
fwrite(phy_pop_insur_tract, "Access to Care (HOI)/data/working/va_tr_2020_2021_care_access_calculations.csv")

## transform to final data clearinghouse format
care_access_tracts <- phy_pop_insur_tract[, .(geoid, year, measure = "access_care_indicator", value = access_index,  moe = NA)]

# standardize to 2020 geographies
## get the tract conversion function
source("https://github.com/uva-bi-sdad/sdc.geographies/raw/main/utils/distribution/tract_conversions.R")
## convert
stnd <- standardize_all(care_access_tracts)

# save standardized file
write_csv(stnd, file = xzfile("Access to Care (HOI)/data/distribution/va_tr_2017_2021_care_access_indicator_std.csv.xz"))


