# dataset creation code - data source ingest - file 2
# source file: https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_S1/FeatureServer/21?f=pjson

# Import source file and save to original for backup
source_file <- "https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_S1/FeatureServer/21?f=pjson"
download.file(source_file, "data/va059_geo_ffxct_gis_2022_zip_codes/original/va059_geo_ffxct_gis_2022_zip_codes.json")
