# dataset creation code - data source ingest - file 1
# source file: https://gis2.arlingtonva.us/arlgis/rest/services/Open_Data/od_Civic_Association_Polygons/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson

# Import source file and save to original for backup
source_file <- "https://gis2.arlingtonva.us/arlgis/rest/services/Open_Data/od_Civic_Association_Polygons/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
download.file(source_file, "data/va013_geo_arl_2021_civic_associations/original/va013_geo_arl_2021_civic_associations.geojson")
