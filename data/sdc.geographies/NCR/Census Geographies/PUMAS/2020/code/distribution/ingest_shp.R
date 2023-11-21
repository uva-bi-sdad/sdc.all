# get pumas shape files for dc, md, va -----------------------------------------

library(tigris)

ncr_states <- c(51, 11, 24)
pumas_list_2020 <- lapply(ncr_states, function(x) {pumas(state=x, year=2020)})

pumas_2020 <- rbind_tigris(pumas_list_2020)

sf::st_write(pumas_2020, "NCR/Census Geographies/PUMAS/2020/data/original/dc_md_va_geo_2020_pumas.geojson",
             delete_dsn=TRUE)
