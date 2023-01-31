# INGESTING DATA FROM DC OPEN DATA PORTAL ~~~~~~~~~~
library(jsonlite)

# WMATA rail locations
write_json(fromJSON("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_WebMercator/MapServer/111/query?outFields=*&where=1%3D1&f=geojson"), "./Infrastructure/data/original/WMATA_rail_locations.geojson")

# WMATA bus locations
write_json(fromJSON("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_WebMercator/MapServer/53/query?outFields=*&where=1%3D1&f=geojson"), "./Infrastructure/data/original/WMATA_bus_locations.geojson")

# Carshare locations (Zipcar and Enterprise)
write_json(fromJSON("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_WebMercator/MapServer/95/query?outFields=*&where=1%3D1&f=geojson"), "./Infrastructure/data/original/carshare_locations.geojson")

# Capital Bikeshare Locations
write_json(fromJSON("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_WebMercator/MapServer/5/query?outFields=*&where=1%3D1&f=geojson"), "./Infrastructure/data/original/capital_bikeshare_locations.geojson")
