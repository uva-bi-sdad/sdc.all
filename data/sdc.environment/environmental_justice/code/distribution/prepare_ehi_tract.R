library(data.table)
library(readr)

for (yr in 2016:2021) {
  dt <- setDT(read_csv(xzfile(paste0("environmental_justice/data/working/va_bg_", yr, "_env_haz_inx.csv.xz"))))
  bg_pops <- unique(setDT(read_csv("../sdc.demographics_dev/Overall/data/distribution/census.csv.xz")))[
    year == yr & nchar(geoid) == 12 & substr(geoid, 1, 2) == "51" & measure == "population_direct", ]
  dt[, geoid := as.character(geoid)]
  
  tracts <- as.data.table(unique(substr(dt$geoid, 1, 11)))
  colnames(tracts) <- "geoid"
  tracts[, geoid := as.character(geoid)]
  
  msr <- dt$measure[1]
  
  if (exists("tracts_wm")) rm(tracts_wm)
  for (id in tracts$geoid) {
    tr_bgs <- dt[substr(geoid, 1, 11)==id]
    tr_bgs_pops <- merge(tr_bgs, bg_pops, by="geoid")
    wm <- weighted.mean(tr_bgs_pops$value.x, tr_bgs_pops$value.y)
    tract_wm <- data.table(geoid = id, year = yr, measure = msr, value = wm, moe = NA)
    
    if (!exists("tracts_wm")) tracts_wm <- tract_wm else tracts_wm <- rbindlist(list(tracts_wm, tract_wm))
  }
  
  write_csv(tracts_wm, xzfile(paste0("environmental_justice/data/distribution/va_tr_", yr, "_env_haz_inx.csv.xz")))
}












