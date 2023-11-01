# NATIONAL WALKABILITY INDEX
options(timeout = max(1000, getOption("timeout")))

# THIS IS A BIG FILE, Do not try to push it up to github
url <-"https://edg.epa.gov/EPADataCommons/public/OA/WalkabilityIndex.zip"
dest <- "./Infrastructure/data/original/Walkability (HOI)/data/original/WalkabilityIndex.zip"
download.file(url, dest)
