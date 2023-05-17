library(dplyr)
library(stringr)
library(tidyr)

#census tract
#transpose the census tract into the long form
ejscreen_wide_ct <- read_csv("~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/EJSCREEN_Table_CensusTracts_NCR.csv")

dim(ejscreen_wide_ct); View(ejscreen_wide_ct)

#keep only the variables ID, STATE_NAME, CNTY_NAME, PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PRE1960, 
#PRE1960PCT, PNPL, PRMP, PTSDF, UST, PWDIS, D_PM25_2, D_OZONE_2, D_DSLPM_2, D_CANCR_2, D_RESP_2, 
#D_PTRAF_2, D_LDPNT_2, D_PNPL_2, D_PRMP_2, D_PTSDF_2, D_UST_2, D_PWDIS_2

ejscreen_wide_ct <- data.frame(ejscreen_wide_ct[, c("ID", "STATE_NAME", "CNTY_NAME", "PM25", "OZONE", "DSLPM", "CANCER", "RESP", 
                                                    "PTRAF", "PRE1960", "PRE1960PCT", "PNPL", "PRMP", "PTSDF", "UST", "PWDIS", 
                                                    "D_PM25_2", "D_OZONE_2", "D_DSLPM_2", "D_CANCR_2", "D_RESP_2", "D_PTRAF_2", 
                                                    "D_LDPNT_2", "D_PNPL_2", "D_PRMP_2", "D_PTSDF_2", "D_UST_2", "D_PWDIS_2")])
ejscreen_wide_ct$CNTY_NAME <- ifelse(ejscreen_wide_ct$CNTY_NAME=="District of Columbia", "Washington", ejscreen_wide_ct$CNTY_NAME)
dim(ejscreen_wide_ct); View(ejscreen_wide_ct)

ejscreen_wide_ct$region_name <- factor(paste(paste0(ejscreen_wide_ct$CNTY_NAME,","), ejscreen_wide_ct$STATE_NAME))
      ejscreen_wide_ct$geoid <- factor(ejscreen_wide_ct$ID)
ejscreen_wide_ct$region_type <- as.factor(rep("census tract", dim(ejscreen_wide_ct)[1]))  
       ejscreen_wide_ct$year <- as.numeric(rep(2022, dim(ejscreen_wide_ct)[1]))  
dim(ejscreen_wide_ct)       
            drop <- c("STATE_NAME", "CNTY_NAME", "ID") 
ejscreen_wide_ct <- ejscreen_wide_ct[,!(names(ejscreen_wide_ct) %in% drop)]
ejscreen_wide_ct <- ejscreen_wide_ct[, c(27, 28, 26, 29, 1:25)]
dim(ejscreen_wide_ct); View(ejscreen_wide_ct)

#place the group the analytic variables into quartiles
                   quartiles <- quantile(ejscreen_wide_ct$PM25, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
   ejscreen_wide_ct$PM25_QRT <- as.numeric(cut(ejscreen_wide_ct$PM25, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
   ejscreen_wide_ct$PM25_QRT <- ifelse(ejscreen_wide_ct$PM25_QRT==1, "first", ifelse(ejscreen_wide_ct$PM25_QRT==2, "second", ifelse(ejscreen_wide_ct$PM25_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$OZONE, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_ct$OZONE_QRT <- as.numeric(cut(ejscreen_wide_ct$OZONE, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
  ejscreen_wide_ct$OZONE_QRT <- ifelse(ejscreen_wide_ct$OZONE_QRT==1, "first", ifelse(ejscreen_wide_ct$OZONE_QRT==2, "second", ifelse(ejscreen_wide_ct$OZONE_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$DSLPM, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_ct$DSLPM_QRT <- as.numeric(cut(ejscreen_wide_ct$DSLPM, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
  ejscreen_wide_ct$DSLPM_QRT <- ifelse(ejscreen_wide_ct$DSLPM_QRT==1, "first", ifelse(ejscreen_wide_ct$DSLPM_QRT==2, "second", ifelse(ejscreen_wide_ct$DSLPM_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$PTRAF, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_ct$PTRAF_QRT <- as.numeric(cut(ejscreen_wide_ct$PTRAF, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
  ejscreen_wide_ct$PTRAF_QRT <- ifelse(ejscreen_wide_ct$PTRAF_QRT==1, "first", ifelse(ejscreen_wide_ct$PTRAF_QRT==2, "second", ifelse(ejscreen_wide_ct$PTRAF_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$PRE1960, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
ejscreen_wide_ct$PRE1960_QRT <- as.numeric(cut(ejscreen_wide_ct$PRE1960, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
ejscreen_wide_ct$PRE1960_QRT <- ifelse(ejscreen_wide_ct$PRE1960_QRT==1, "first", ifelse(ejscreen_wide_ct$PRE1960_QRT==2, "second", ifelse(ejscreen_wide_ct$PRE1960_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$PRMP, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
   ejscreen_wide_ct$PRMP_QRT <- as.numeric(cut(ejscreen_wide_ct$PRMP, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
   ejscreen_wide_ct$PRMP_QRT <- ifelse(ejscreen_wide_ct$PRMP_QRT==1, "first", ifelse(ejscreen_wide_ct$PRMP_QRT==2, "second", ifelse(ejscreen_wide_ct$PRMP_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$PNPL, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
   ejscreen_wide_ct$PNPL_QRT <- as.numeric(cut(ejscreen_wide_ct$PNPL, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
   ejscreen_wide_ct$PNPL_QRT <- ifelse(ejscreen_wide_ct$PNPL_QRT==1, "first", ifelse(ejscreen_wide_ct$PNPL_QRT==2, "second", ifelse(ejscreen_wide_ct$PNPL_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$PTSDF, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_ct$PTSDF_QRT <- as.numeric(cut(ejscreen_wide_ct$PTSDF, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
  ejscreen_wide_ct$PTSDF_QRT <- ifelse(ejscreen_wide_ct$PTSDF_QRT==1, "first", ifelse(ejscreen_wide_ct$PTSDF_QRT==2, "second", ifelse(ejscreen_wide_ct$PTSDF_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$UST, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
    ejscreen_wide_ct$UST_QRT <- as.numeric(cut(ejscreen_wide_ct$UST, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
    ejscreen_wide_ct$UST_QRT <- ifelse(ejscreen_wide_ct$UST_QRT==1, "first", ifelse(ejscreen_wide_ct$UST_QRT==2, "second", ifelse(ejscreen_wide_ct$UST_QRT==3, "third", "fourth")))
                   quartiles <- quantile(ejscreen_wide_ct$PWDIS, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_ct$PWDIS_QRT <- as.numeric(cut(ejscreen_wide_ct$PWDIS, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
  ejscreen_wide_ct$PWDIS_QRT <- ifelse(ejscreen_wide_ct$PWDIS_QRT==1, "first", ifelse(ejscreen_wide_ct$PWDIS_QRT==2, "second", ifelse(ejscreen_wide_ct$PWDIS_QRT==3, "third", "fourth")))
dim(ejscreen_wide_ct); View(ejscreen_wide_ct)

#save the wide form to the working folder
write.csv(ejscreen_wide_ct, "~/RCode/Data Commons/Environment/EPA_EJScreen/data/working/ejscreen_wide_ct.csv",
          row.names=FALSE)

#transpose to the long format
             ejscreen_long_ct <- gather(ejscreen_wide_ct, measure, value, PM25:PWDIS_QRT, factor_key=TRUE)
                       nrw_ct <- dim(ejscreen_long_ct)[1]
ejscreen_long_ct$measure_type <- as.factor(rep(c("concentration","risk index","exposure index","proximity index","count","percent","proximity index","risk index","environmental justice index","quartile"), 
                                               c(3993,1331,1331,1331,1331,1331,3993,2662,15972,13310))) 
         ejscreen_long_ct$moe <- rep(NA, nrw_ct)

ncr_tr_epa_2022_environmental_justice_screen <- data.frame(geoid = ejscreen_long_ct$geoid,
                                                           region_type = ejscreen_long_ct$region_type,
                                                           region_name = ejscreen_long_ct$region_name,
                                                           year = ejscreen_long_ct$year,
                                                           measure = ejscreen_long_ct$measure,
                                                           value = ejscreen_long_ct$value,
                                                           measure_type = ejscreen_long_ct$measure_type,
                                                           moe = ejscreen_long_ct$moe)
dim(ncr_tr_epa_2022_environmental_justice_screen)

#save the wide long to the distribution folder
write.csv(ncr_tr_epa_2022_environmental_justice_screen, "~/RCode/Data Commons/Environment/EPA_EJScreen/data/distribution/ncr_tr_epa_2022_environmental_justice_screen.csv",
          row.names=FALSE)

#####################################
#block group
#transpose the census tract into the long form
ejscreen_wide_bg <- read_csv("~/RCode/Data Commons/Environment/EPA_EJScreen/data/original/EJSCREEN_Table_BlockGroups_NCR.csv")
dim(ejscreen_wide_bg); View(ejscreen_wide_bg)

#keep only the variables ID, STATE_NAME, CNTY_NAME, PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PRE1960, 
#PRE1960PCT, PNPL, PRMP, PTSDF, UST, PWDIS, D_PM25_2, D_OZONE_2, D_DSLPM_2, D_CANCR_2, D_RESP_2, 
#D_PTRAF_2, D_LDPNT_2, D_PNPL_2, D_PRMP_2, D_PTSDF_2, D_UST_2, D_PWDIS_2

ejscreen_wide_bg <- data.frame(ejscreen_wide_bg[, c("ID", "STATE_NAME", "CNTY_NAME", "PM25", "OZONE", "DSLPM", "CANCER", "RESP", 
                                                    "PTRAF", "PRE1960", "PRE1960PCT", "PNPL", "PRMP", "PTSDF", "UST", "PWDIS", 
                                                    "D_PM25_2", "D_OZONE_2", "D_DSLPM_2", "D_CANCR_2", "D_RESP_2", "D_PTRAF_2", 
                                                    "D_LDPNT_2", "D_PNPL_2", "D_PRMP_2", "D_PTSDF_2", "D_UST_2", "D_PWDIS_2")])
ejscreen_wide_bg$CNTY_NAME <- ifelse(ejscreen_wide_bg$CNTY_NAME=="District of Columbia", "Washington", ejscreen_wide_bg$CNTY_NAME)
dim(ejscreen_wide_bg); View(ejscreen_wide_bg)

ejscreen_wide_bg$region_name <- factor(paste(paste0(ejscreen_wide_bg$CNTY_NAME,","), ejscreen_wide_bg$STATE_NAME))
      ejscreen_wide_bg$geoid <- factor(ejscreen_wide_bg$ID)
ejscreen_wide_bg$region_type <- as.factor(rep("block group", dim(ejscreen_wide_bg)[1]))  
       ejscreen_wide_bg$year <- as.numeric(rep(2022, dim(ejscreen_wide_bg)[1]))  
dim(ejscreen_wide_bg)       
drop <- c("STATE_NAME", "CNTY_NAME", "ID") 
            ejscreen_wide_bg <- ejscreen_wide_bg[,!(names(ejscreen_wide_bg) %in% drop)]
            ejscreen_wide_bg <- ejscreen_wide_bg[, c(27, 28, 26, 29, 1:25)]
dim(ejscreen_wide_bg); View(ejscreen_wide_bg)

#place the group the analytic variables into quartiles
                   quartiles <- quantile(ejscreen_wide_bg$PM25, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
   ejscreen_wide_bg$PM25_QRT <- as.numeric(cut(ejscreen_wide_bg$PM25, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$OZONE, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_bg$OZONE_QRT <- as.numeric(cut(ejscreen_wide_bg$OZONE, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$DSLPM, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_bg$DSLPM_QRT <- as.numeric(cut(ejscreen_wide_bg$DSLPM, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$PTRAF, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_bg$PTRAF_QRT <- as.numeric(cut(ejscreen_wide_bg$PTRAF, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$PRE1960, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
ejscreen_wide_bg$PRE1960_QRT <- as.numeric(cut(ejscreen_wide_bg$PRE1960, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$PRMP, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
   ejscreen_wide_bg$PRMP_QRT <- as.numeric(cut(ejscreen_wide_bg$PRMP, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$PNPL, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
   ejscreen_wide_bg$PNPL_QRT <- as.numeric(cut(ejscreen_wide_bg$PNPL, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$PTSDF, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_bg$PTSDF_QRT <- as.numeric(cut(ejscreen_wide_bg$PTSDF, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$UST, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
    ejscreen_wide_bg$UST_QRT <- as.numeric(cut(ejscreen_wide_bg$UST, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
                   quartiles <- quantile(ejscreen_wide_bg$PWDIS, probs = c(0.00, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)
  ejscreen_wide_bg$PWDIS_QRT <- as.numeric(cut(ejscreen_wide_bg$PWDIS, quartiles, include.lowest = TRUE, labels = c(round(unname(quartiles)[-1], 4)))); rm(quartiles)
dim(ejscreen_wide_bg); View(ejscreen_wide_bg)

#save the wide form to the working folder
write.csv(ejscreen_wide_bg, "~/RCode/Data Commons/Environment/EPA_EJScreen/data/working/ejscreen_wide_bg.csv", row.names=FALSE)

#transpose to the long form
             ejscreen_long_bg <- gather(ejscreen_wide_bg, measure, value, PM25:PWDIS_QRT, factor_key=TRUE)
                       nrw_bg <- dim(ejscreen_long_bg)[1]
ejscreen_long_bg$measure_type <- as.factor(rep(c("concentration","risk index","exposure index","proximity index","count","percent","proximity index","risk index","environmental justice index","quartile"), 
                                               c(10878,3626,3626,3626,3626,3626,10878,7252,43512,36260))) 
         ejscreen_long_bg$moe <- rep(NA, nrw_bg)   

ncr_bg_epa_2022_environmental_justice_screen <- data.frame(geoid = ejscreen_long_bg$geoid,
                                                           region_type = ejscreen_long_bg$region_type,
                                                           region_name = ejscreen_long_bg$region_name,
                                                           year = ejscreen_long_bg$year,
                                                           measure = ejscreen_long_bg$measure,
                                                           value = ejscreen_long_bg$value,
                                                           measure_type = ejscreen_long_bg$measure_type,
                                                           moe = ejscreen_long_bg$moe)
dim(ncr_bg_epa_2022_environmental_justice_screen)

#save the wide long to the distribution folder
write.csv(ncr_bg_epa_2022_environmental_justice_screen, "~/RCode/Data Commons/Environment/EPA_EJScreen/data/distribution/ncr_bg_epa_2022_environmental_justice_screen.csv",
          row.names=FALSE)





