library(httr)

# read in drug overdose data from VDH - new data available at https://www.vdh.virginia.gov/surveillance-and-investigation/syndromic-surveillance/drug-overdose-surveillance/

url <- "https://www.vdh.virginia.gov/content/uploads/sites/13/2021/10/Drug-Overdose-ED-Visits_Virginia-September-2021.xlsx"
GET(url, write_disk("Population Health/Substance Abuse/Drug Overdose ED Visits/data/original/Drug-Overdose-ED-Visits_Virginia-September-2021.xlsx", overwrite = TRUE))