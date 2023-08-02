# get gender distribution data from each model
uploadpath <- "Gender/data/working/model/"
files <- list.files(uploadpath)

# combine data into one file
combined <- NULL
for (file in files) {
  data <- read.csv(paste0(uploadpath, file))
  combined <- rbind(combined, data)
}

# save to distribution
yearlist <- unique(combined$year)
savepath <- "Gender/data/distribution/"
readr::write_csv(combined, xzfile(paste0(savepath, "va_hsrsdpdzccttrbg_sdad_",min(yearlist),'_',max(yearlist),"_gender_demographics.csv.xz"), compression = 9))