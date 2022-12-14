library(sf)
library(data.table)
library(ggplot2)
library(ggrepel)
library(viridis)

civ_assoc <- st_read("../../projects_data/mc/data_commons/arl_civic_association_polygons/Civic_Association_Polygons.shp")
plot(st_geometry(civ_assoc))

civ_assoc_wgs84 <-st_transform(civ_assoc, 4326)
plot(st_geometry(civ_assoc_wgs84))

civ_assoc_wgs84$LABEL <- gsub("  ", ", ", gsub("[\r\n]", " ", civ_assoc_wgs84$LABEL))

con <- get_db_conn()
dc_dbWriteTable(con, "dc_geographies", "va_013_arl_2020_civic_associations", civ_assoc_wgs84)
DBI::dbDisconnect(con)


# intersect with parcels to assign parcels to civic associations
int <- st_intersects(civ_assoc_wgs84, va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf)

plot(st_geometry(va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf[int[[1]],]))
plot(st_geometry(va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf[int[[2]],]))

i = 1

setDT(va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf)
for (i in 1:length(int)) {
  cv <- civ_assoc_wgs84[i,]$CIVIC
  va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf[int[[i]], civ_assoc := cv]
}
va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf <- st_as_sf(va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf)

con <- get_db_conn()
dc_dbWriteTable(con, "dc_working", "va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf", va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf)
DBI::dbDisconnect(con)

plot(va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf[va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf$civ_assoc == "Williamsburg", c("afr_amer_alone_pct")])
plot(va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf[va_arl_block_parcels_cnts_dmgs_dt_wide_geo_sf$civ_assoc == "Williamsburg", c("wht_alone_pct")])

library(data.table)

con <- get_db_conn()
va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf <- st_read(con, c("dc_working", "va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf"))
DBI::dbDisconnect(con)

setDT(va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf)
civ_dmg <- va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf[, .(wht_alone_pct = mean(wht_alone_pct), afr_amer_alone_pct = mean(afr_amer_alone_pct) ), c("civ_assoc")]
setDT(civ_assoc_wgs84)
civ_dmg_geo <- merge(civ_dmg, civ_assoc_wgs84, by.x = "civ_assoc", by.y = "CIVIC")
civ_dmg_geo_sf <- st_as_sf(civ_dmg_geo)
# Add centroids for label placement
civ_dmg_geo_sf <- cbind(civ_dmg_geo_sf, st_coordinates(st_centroid(civ_dmg_geo_sf)))

plot(civ_dmg_geo_sf[, c("wht_alone_pct")])
plot(civ_dmg_geo_sf[, c("afr_amer_alone_pct")])



# Arlington County Geo
con <- get_db_conn()
us_counties <- st_read(con, c("gis_census_cb", "cb_2016_us_county_500k"))
DBI::dbDisconnect(con)

va_arl_county <- us_counties[us_counties$GEOID=="51013",]
va_arl_county <- st_transform(va_arl_county, 4326)
plot(st_geometry(va_arl_county))


civ_names <- cbind(civ_dmg_geo_sf, st_coordinates(st_centroid(civ_dmg_geo_sf)))

ggplot(data = va_arl_county) +
  geom_sf() +
  geom_sf(data = civ_dmg_geo_sf, aes(fill = wht_alone_pct)) + 
  geom_label_repel(data = civ_dmg_geo_sf, aes(X, Y, label = civ_assoc), size = 1.35, fontface = "bold") +
  scale_fill_distiller(palette = "GnBu", 
                       limits = c(min(civ_dmg_geo_sf$afr_amer_alone_pct), max(civ_dmg_geo_sf$wht_alone_pct))) +
  labs(title = "Arlington Civic Association Demographics\nPercent White 2019 [ACS Redistribution]", x = "", y = "")

ggplot(data = va_arl_county) +
  geom_sf() +
  geom_sf(data = civ_dmg_geo_sf, aes(fill = afr_amer_alone_pct)) + 
  geom_label_repel(data = civ_dmg_geo_sf, aes(X, Y, label = civ_assoc), size = 1.5, fontface = "bold") +
  scale_fill_distiller(palette = "GnBu",
                       limits = c(min(civ_dmg_geo_sf$afr_amer_alone_pct), max(civ_dmg_geo_sf$wht_alone_pct)))

ggplot(data = va_arl_county) +
  geom_sf() +
  geom_sf(data = civ_dmg_geo_sf, aes(fill = afr_amer_alone_pct)) + 
  geom_label_repel(data = civ_dmg_geo_sf, aes(X, Y, label = civ_assoc), size = 1.5, fontface = "bold") +
  scale_fill_distiller(palette = "GnBu",
                       limits = c(min(civ_dmg_geo_sf$afr_amer_alone_pct), max(civ_dmg_geo_sf$wht_alone_pct)))


rpc_ctr <- st_centroid(va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf[va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf$rpc_master=="32024002" | va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf$rpc_master=="03036126",])

ggplot(data = va_arl_county) +
  geom_sf() +
  geom_sf(data = civ_dmg_geo_sf, aes(fill = wht_alone_pct)) +
  geom_sf(data = rpc_ctr)


# melt to long
va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf$geometry <- NULL

prcl_lng <- melt(va_arl_block_parcels_cnts_dmgs_civ_dt_wide_geo_sf, c("rpc_master", "geoid", "civ_assoc"), variable.name = "measure")
prcl_lng <- prcl_lng[!measure %like% "pct$"]
civ_assoc_dmgs <- prcl_lng[, .(value = sum(value)), list(civ_assoc, measure)]
civ_assoc_dmgs <- civ_assoc_dmgs[!is.na(civ_assoc)]

civ_assoc_dmgs[, year := 2019]
civ_assoc_dmgs[, measure_type := "count"]
civ_assoc_dmgs[, region_type := "civic association"]
civ_assoc_dmgs[, region_name := civ_assoc]
civ_assoc_dmgs[, geoid := paste0("va013cc_", gsub(" +", "_", gsub("-", "", tolower(region_name))))]

civ_assoc_dmgs$year <- as.integer(civ_assoc_dmgs$year)

# write civic assoc demographics to db 
con <- get_db_conn()
dc_dbWriteTable(con, "dc_population_characteristics", "va_013_arl_2020_civic_association_demographics", civ_assoc_dmgs[, .(geoid, region_type, region_name, year, measure, value, measure_type)])
DBI::dbDisconnect(con)
