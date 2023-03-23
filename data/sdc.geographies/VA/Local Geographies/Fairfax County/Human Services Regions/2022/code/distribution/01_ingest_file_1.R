# dataset creation code - data source ingest - file 1
# source file: https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_S2/FeatureServer/12/query?outFields=*&where=1%3D1&f=geojson

# Import source file and save to original for backup
source_file <- "https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_S2/FeatureServer/12/query?outFields=*&where=1%3D1&f=geojson"
download.file(source_file, "data/va059_geo_ffxct_gis_2022_human_services_regions/original/va059_geo_ffxct_gis_2022_human_services_regions.geojson")
