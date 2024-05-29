library(data.table)
library(readr)


cms2017 <- setDT(read_csv(xzfile("Health Care Services/Physicians/data/original/Medicare Physician & Other Practitioners - by Provider/2017/MUP_PHY_R19_P04_V10_D17_Prov.csv.xz")))
cms2017va_prim <- cms2017[Rndrng_Prvdr_State_FIPS=="51" & 
                            Rndrng_Prvdr_Type %in% c("Internal Medicine","Pediatric Medicine","Family Practice","Obstetrics & Gynecology") & 
                            !is.na(Rndrng_Prvdr_Crdntls),
                          .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_St2, Rndrng_Prvdr_City, Rndrng_Prvdr_Type, Rndrng_Prvdr_RUCA)]
cms2017va_prim[, year := 2017]
rm(cms2017)

cms2018 <- setDT(read_csv(xzfile("Health Care Services/Physicians/data/original/Medicare Physician & Other Practitioners - by Provider/2018/MUP_PHY_R20_P04_V10_D18_Prov.csv.xz")))
cms2018va_prim <- cms2018[Rndrng_Prvdr_State_FIPS=="51" & 
                            Rndrng_Prvdr_Type %in% c("Internal Medicine","Pediatric Medicine","Family Practice","Obstetrics & Gynecology") & 
                            !is.na(Rndrng_Prvdr_Crdntls),
                          .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_St2, Rndrng_Prvdr_City, Rndrng_Prvdr_Type, Rndrng_Prvdr_RUCA)]
cms2018va_prim[, year := 2018]
rm(cms2018)

cms2019 <- setDT(read_csv(xzfile("Health Care Services/Physicians/data/original/Medicare Physician & Other Practitioners - by Provider/2019/MUP_PHY_R21_P04_V10_D19_Prov.csv.xz")))
cms2019va_prim <- cms2019[Rndrng_Prvdr_State_FIPS=="51" & 
                            Rndrng_Prvdr_Type %in% c("Internal Medicine","Pediatric Medicine","Family Practice","Obstetrics & Gynecology") & 
                            !is.na(Rndrng_Prvdr_Crdntls),
                          .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_St2, Rndrng_Prvdr_City, Rndrng_Prvdr_Type, Rndrng_Prvdr_RUCA)]
cms2019va_prim[, year := 2019]
rm(cms2019)

cms2020 <- setDT(read_csv(xzfile("Health Care Services/Physicians/data/original/Medicare Physician & Other Practitioners - by Provider/2020/MUP_PHY_R22_P05_V10_D20_Prov.csv.xz")))
cms2020va_prim <- cms2020[Rndrng_Prvdr_State_FIPS=="51" & 
                            Rndrng_Prvdr_Type %in% c("Internal Medicine","Pediatric Medicine","Family Practice","Obstetrics & Gynecology") & 
                            !is.na(Rndrng_Prvdr_Crdntls),
                          .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_St2, Rndrng_Prvdr_City, Rndrng_Prvdr_Type, Rndrng_Prvdr_RUCA)]
cms2020va_prim[, year := 2020]
rm(cms2020)

cms2021 <- setDT(read_csv(xzfile("Health Care Services/Physicians/data/original/Medicare Physician & Other Practitioners - by Provider/2021/MUP_PHY_R23_P05_V10_D21_Prov.csv.xz")))
cms2021va_prim <- cms2021[Rndrng_Prvdr_State_FIPS=="51" & 
                            Rndrng_Prvdr_Type %in% c("Internal Medicine","Pediatric Medicine","Family Practice","Obstetrics & Gynecology") & 
                            !is.na(Rndrng_Prvdr_Crdntls),
                          .(Rndrng_NPI, Rndrng_Prvdr_Crdntls, Rndrng_Prvdr_St1, Rndrng_Prvdr_St2, Rndrng_Prvdr_City, Rndrng_Prvdr_Type, Rndrng_Prvdr_RUCA)]
cms2021va_prim[, year := 2021]
rm(cms2021)

cms20172021va_prim <- rbindlist(list(cms2017va_prim, cms2018va_prim, cms2019va_prim, cms2020va_prim, cms2021va_prim))

write_csv(cms20172021va_prim, "Access to Care (HOI)/data/working/va_2017_2021_primary_care_phys.csv")
