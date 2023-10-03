# High School Pyramid Areas - FFX GIS
# 
# Link to get to API: https://data-fairfaxcountygis.opendata.arcgis.com/datasets/Fairfaxcountygis::high-school-attendance-areas/about

# Import source file and save to original
source_file <- "https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/OpenData_S1/FeatureServer/12/query?outFields=*&where=1%3D1&f=geojson"
download.file(source_file, "VA/Local Geographies/Fairfax County/High School Attendance Areas/2023/data/original/va059_geo_ffxct_gis_2023_high_school_attendance_areas.geojson")
