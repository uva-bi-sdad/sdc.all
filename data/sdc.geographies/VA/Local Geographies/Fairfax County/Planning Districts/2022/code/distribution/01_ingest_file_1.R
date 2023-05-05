# dataset creation code - data source ingest - file 1
# source file: https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_A1/FeatureServer/2/query?outFields=*&where=1%3D1&f=geojson

# Import source file and save to original for backup
source_file <- "https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_A1/FeatureServer/2/query?outFields=*&where=1%3D1&f=geojson"
download.file(source_file, "VA/Local Geographies/Fairfax County/Planning Districts/2022/data/original/va059_geo_ffxct_gis_2022_planning_districts.geojson")
