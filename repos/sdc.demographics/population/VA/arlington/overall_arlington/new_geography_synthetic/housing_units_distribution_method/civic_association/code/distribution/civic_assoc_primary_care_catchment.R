library(sf)
library(ggplot2)

con <- get_db_conn()
va013_nbr_sdad_2021_primary_care_acccess_scores <- sf::st_read(dsn = con, 
                                                               layer = c("dc_health_behavior_diet", "va013_nbr_sdad_2021_primary_care_acccess_scores_updated"))
DBI::dbDisconnect(con)

civ_assoc <- st_read("../../projects_data/mc/data_commons/arl_civic_association_polygons/Civic_Association_Polygons.shp")
civ_assoc_wgs84 <-st_transform(civ_assoc, 4326)

va013_nbr_sdad_2021_primary_care_acccess_scores$geometry <- NULL

va013_nbr_sdad_2021_primary_care_acccess_scores_sf <- merge(civ_assoc_wgs84, va013_nbr_sdad_2021_primary_care_acccess_scores, by="CIVIC")

sca3 <- va013_nbr_sdad_2021_primary_care_acccess_scores_sf[va013_nbr_sdad_2021_primary_care_acccess_scores_sf$measure=="3sfca_e2s_weigths",c("value")]

sca2e <- va013_nbr_sdad_2021_primary_care_acccess_scores_sf[va013_nbr_sdad_2021_primary_care_acccess_scores_sf$measure=="e2sfca",c("value")] 

sca2 <- va013_nbr_sdad_2021_primary_care_acccess_scores_sf[va013_nbr_sdad_2021_primary_care_acccess_scores_sf$measure=="2sfca",c("value")] 

ggplot() +
  geom_sf() +
  geom_sf(data = sca3, aes(fill = value)) +
  labs(title = "Primary Care Physician Access by Civic Association\n2019 [3-Step Catchment Area Calculation]", x = "", y = "")

ggplot() +
  geom_sf() +
  geom_sf(data = sca2e, aes(fill = value)) +
  labs(title = "Primary Care Physician Access by Civic Association\n[2-Step Enhanced Catchment Area Calculation]", x = "", y = "")

ggplot() +
  geom_sf() +
  geom_sf(data = sca2, aes(fill = value)) +
  labs(title = "Primary Care Physician Access by Civic Association\n[2-Step Catchment Area Calculation]", x = "", y = "")
