
#Code for getting Washington Self Sufficiency standard data.  

#The following link is for Virginia data
#Washington Self Sufficiency standard link "https://selfsufficiencystandard.org/virginia/"

#First step is getting the URL of the file
URL <- "https://selfsufficiencystandard.org/wp-content/uploads/2021/11/VA2021_all_families.xlsx"

#Set the File destination on your system
destination <- "~/output.csv"

#downloading the file with R
download.file(URL,destination)