# get 2010 puma geometry data for dc, md, va
library(sf)

pumas_2010 <- NULL
path <- 'NCR/Census Geographies/PUMAS/2010/data/original/'
for (state_code in c('11', '24', '51')) {
  temp_shp <- read_sf(paste0(path, 'tl_2019_', state_code, '_puma10.shp'))
  
  pumas_2010 <- rbind(pumas_2010, temp_shp)
}

sf::st_write(pumas_2010, 
             paste0(path, "dc_md_va_geo_2010_pumas.geojson"),
             delete_dsn=TRUE)
