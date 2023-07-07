library(dplyr)
library(stringr)
library(tidyr)

#keep only those census tracts in the NCR for select variables

#bring in from working folder and create a ncr file by combining the data from va, md, and dc
originalva <- read.csv("~/RCode/Data Commons/Environment/FEMA_NRI/data/original/NRI_Table_CensusTracts_Virginia.csv")
originalmd <- read.csv("~/RCode/Data Commons/Environment/FEMA_NRI/data/original/NRI_Table_CensusTracts_Maryland.csv")
originaldc <- read.csv("~/RCode/Data Commons/Environment/FEMA_NRI/data/original/NRI_Table_CensusTracts_DistrictofColumbia.csv")
dim(originalva); dim(originalmd); dim(originaldc) 

#keep only the variables STATE, COUNTY, TRACTFIPS, RISK_SCORE, AVLN_RISKS, CFLD_RISKS, CWAV_RISKS,  
#DRGT_RISKS, ERQK_RISKS, HAIL_RISKS, HWAV_RISKS, HRCN_RISKS, ISTM_RISKS, LNDS_RISKS, LTNG_RISKS,  
#RFLD_RISKS, SWND_RISKS, TRND_RISKS, TSUN_RISKS, VLCN_RISKS, WFIR_RISKS, WNTW_RISKS

reducedva <- data.frame(originalva[, c("STATE","COUNTY","TRACTFIPS","RISK_SCORE","AVLN_RISKS","CFLD_RISKS","CWAV_RISKS", 
                                       "DRGT_RISKS","ERQK_RISKS","HAIL_RISKS","HWAV_RISKS","HRCN_RISKS",
                                       "ISTM_RISKS","LNDS_RISKS","LTNG_RISKS","RFLD_RISKS","SWND_RISKS",
                                       "TRND_RISKS","TSUN_RISKS","VLCN_RISKS","WFIR_RISKS","WNTW_RISKS")])

reducedmd <- data.frame(originalmd[, c("STATE","COUNTY","TRACTFIPS","RISK_SCORE", "AVLN_RISKS","CFLD_RISKS","CWAV_RISKS", 
                                       "DRGT_RISKS","ERQK_RISKS","HAIL_RISKS","HWAV_RISKS","HRCN_RISKS",
                                       "ISTM_RISKS","LNDS_RISKS","LTNG_RISKS","RFLD_RISKS","SWND_RISKS",
                                       "TRND_RISKS","TSUN_RISKS","VLCN_RISKS","WFIR_RISKS","WNTW_RISKS")])

reduceddc <- data.frame(originaldc[, c("STATE","COUNTY","TRACTFIPS","RISK_SCORE","AVLN_RISKS","CFLD_RISKS","CWAV_RISKS", 
                                       "DRGT_RISKS","ERQK_RISKS","HAIL_RISKS","HWAV_RISKS","HRCN_RISKS",
                                       "ISTM_RISKS","LNDS_RISKS","LTNG_RISKS","RFLD_RISKS","SWND_RISKS",
                                       "TRND_RISKS","TSUN_RISKS","VLCN_RISKS","WFIR_RISKS","WNTW_RISKS")])
dim(reducedva);dim(reducedmd);dim(reduceddc)

combined <- data.frame(rbind(reduceddc,reducedva,reducedmd))
dim(combined)

#keep only counties/cities iin the NCR
          ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")
             fema_wide <- combined %>% dplyr::filter(str_detect(TRACTFIPS, ncr_counties)) 
 fema_wide$region_name <- factor(paste(paste0(fema_wide$COUNTY,","),fema_wide$STATE))
       fema_wide$geoid <- factor(fema_wide$TRACTFIPS)
dim(fema_wide)       
                  drop <- c("STATE", "COUNTY", "TRACTFIPS") 
             fema_wide <- fema_wide[,!(names(fema_wide) %in% drop)]
dim(fema_wide)  
View(fema_wide)
write.csv(fema_wide, "~/RCode/Data Commons/Environment/FEMA_NRI/data/working/fema_wide", row.names=FALSE)

#transpose to the long form
             fema_long <- gather(fema_wide, measure, value, RISK_SCORE:WNTW_RISKS, factor_key=TRUE)
                   nrw <- dim(fema_long)[1]
        fema_long$year <- as.numeric(rep(2019, nrw))
 fema_long$region_type <- as.factor(rep("tract", nrw)) 
fema_long$measure_type <- as.factor(rep("index", nrw)) 
         fema_long$moe <- rep(NA, nrw)

ncr_tr_fema_2019_natural_hazards_risk_scores <- data.frame(geoid = fema_long$geoid,
                                                           region_type = fema_long$region_type,
                                                           region_name = fema_long$region_name,
                                                           year = fema_long$year,
                                                           measure = fema_long$measure,
                                                           value = fema_long$value,
                                                           measure_type = fema_long$measure_type,
                                                           moe = fema_long$moe)
dim(ncr_tr_fema_2019_natural_hazards_risk_scores)
View(ncr_tr_fema_2019_natural_hazards_risk_scores)
# write.csv(ncr_tr_fema_2019_natural_hazards_risk_scores, 
#          "~/RCode/Data Commons/Environment/FEMA_NRI/data/distribution/ncr_tr_fema_2019_natural_hazards_risk_scores.csv",
#         row.names=FALSE)
readr::write_csv(ncr_tr_fema_2019_natural_hazards_risk_scores, xzfile("climate_change/data/distribution/ncr_tr_fema_2019_natural_hazards_risk_scores.csv.xz", compression = 9))
