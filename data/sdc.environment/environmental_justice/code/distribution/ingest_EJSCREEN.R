library(data.table)
library(readr)
library(stringr)

get_file <- function(url) {
  filename <- basename(url)
  year <- str_extract(url, pattern = "\\d\\d\\d\\d")
  new_filename <- paste0("EJSCREEN_", year,"_USPR_BG_VA.csv")
  print(paste0("Downloading ", url))
  download.file(url, 
                paste0("environmental_justice/data/original/", filename), 
                method = "wget", extra = "--no-check-certificate")
  data <- setDT(read_csv(paste0("environmental_justice/data/original/", filename)))
  data_tr <- data[substr(as.character(ID), 1, 2)=="51" & nchar(as.character(ID))==12,]
  write_csv(data_tr, paste0("environmental_justice/data/original/", new_filename))
  unlink(paste0("environmental_justice/data/original/", filename))
}

urls <- c("https://gaftp.epa.gov/EJSCREEN/2021/EJSCREEN_2021_USPR.csv.zip",
          "https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_USPR.csv.zip",
          "https://gaftp.epa.gov/EJSCREEN/2019/EJSCREEN_2019_USPR.csv.zip",
          "https://gaftp.epa.gov/EJSCREEN/2018/EJSCREEN_2018_USPR_csv.zip",
          "https://gaftp.epa.gov/EJSCREEN/2017/EJSCREEN_2017_USPR_Public.csv",
          "https://gaftp.epa.gov/EJSCREEN/2016/EJSCREEN_V3_USPR_090216_CSV.zip"
          )

for (i in 1:length(urls)) {
  get_file(urls[i])
}
