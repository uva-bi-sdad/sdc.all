#we got the data for incarceration index from prison policy website
#The prison policy have a table for dataset in their website with values. Since there is no option to download, we need 
# to scrap the table alone from the website

#For tract

library(rvest)

url_tract <- "https://www.prisonpolicy.org/origin/va/2020/tract.html"


webpage <- read_html(url_tract)
table_data <- webpage %>% html_table()

write.csv(table_data[[1]], file = "~/git/sdc.public_safety_dev/Incarceration/data/original/incarceration_tract_data_2020.csv", row.names = FALSE)


#For county

url_county <- "https://www.prisonpolicy.org/origin/va/2020/county.html"

webpage <- read_html(url_county)
table_data <- webpage %>% html_table()

write.csv(table_data[[1]], file = "~/git/sdc.public_safety_dev/Incarceration/data/original/incarceration_county_data_2020.csv", row.names = FALSE)



