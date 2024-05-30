library(data.table)
library(tidyverse)
library(dplyr)
library(readr)
library(GPArotation)
library(psych)

# get undergraound storage tank from 2021 - first year collected
ej21 <- setDT(read_csv("environmental_justice/data/original/EJSCREEN_2021_USPR_BG_VA.csv"))
ust21 <- ej21[,.(ID, UST)]
ust21[, ID := as.character(ID)]

yr <- 2016

dtt <- setDT(read_csv(paste0("environmental_justice/data/original/EJSCREEN_", yr,"_USPR_BG_VA.csv")))
setnames(dtt, "FIPS", "ID")

sel <- dtt[, .(ID, DSLPM, CANCER, RESP, PTRAF, PWDIS, PNPL, PRMP, PTSDF, OZONE, PM25, PRE1960PCT)] 

sel <- sel %>% mutate_at(1, as.character)
sel <- sel %>% mutate_at(2:12, as.numeric)
sel_mrg <- merge(sel, ust21, by="ID", all.x=T)

sel_0 <- sel %>% mutate(across(everything(), ~replace_na(., 0)))

selected_columns <- sel_0 %>%
  select(-c(ID))

pca_result <- principal(r = selected_columns,  rotate = "oblimin")
pca_values <- as.data.frame(pca_result$scores)
df_with_pca <- cbind(sel_0, pca_values)
df_with_pca$PC_z <- scale(df_with_pca$PC1)

df_with_pca$year <- yr
df_with_pca$region_type <- 'tract'
df_with_pca$measure <- 'environmental_hazard_index'
df_with_pca$value <- df_with_pca$PC_z
df_with_pca <- df_with_pca %>%  mutate(value = as.character(value))
df_with_pca$geoid <- df_with_pca$ID

data_dist <- df_with_pca %>% select(geoid, measure, year, region_type, value)

write_csv(data_dist, xzfile(paste0("environmental_justice/data/distribution/va_bg_", yr,"_env_haz_inx.csv.xz")))

