# NON WMATA BUS AND TRAINS

# Charles County VanGo
url <- "https://github.com/mobilityequity/maryland-local-gtfs/raw/master/Charles_GTFS.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_charles.zip"
download.file(url, destfile)

# Alexandria DASH
url <- "http://dashbus.com/google_transit.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_alexandria.zip"
download.file(url, destfile)

# Fairfax County Connector
url <- "https://www.fairfaxcounty.gov/connector/sites/connector/files/Assets/connector_gtfs.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_fairfax01.zip"
download.file(url, destfile)

# Fairfax CUE
url <- "https://www.fairfaxva.gov/home/showpublisheddocument/19191/637950354183670000"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_fairfax02.zip"
download.file(url, destfile)

# Arlington Transit
url <- "https://www.arlingtontransit.com/shared/content/gtfs/art/google_transit.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_arlington.zip"
download.file(url, destfile)

# Montgomery Ride On
url <- "https://www.montgomerycountymd.gov/DOT-Transit/Resources/Files/GTFS/RideOnGTFS.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_montgomery.zip"
download.file(url, destfile)

# Frederick TransIT
url <- "https://maps.frederickcountymd.gov/google/google_transit.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_bus_frederick.zip"
download.file(url, destfile)

# Maryland MARC Train
url <- "https://mdotmta-gtfs.s3.amazonaws.com/mdotmta_gtfs_marc.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_train_maryland.zip"
download.file(url, destfile)

# Virginia VRE
url <- "http://www.vre.org/gtfs/google_transit.zip"
destfile <- "./Infrastructure/data/original/NONWMATA_train_virginia.zip"
download.file(url, destfile)
