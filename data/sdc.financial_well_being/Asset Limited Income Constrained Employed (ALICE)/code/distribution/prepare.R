library(tidyverse)
library(readxl)
library(xlsx)

alice.county <- openxlsx::read.xlsx("https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_VA.xlsx", sheet = 2)

alice.county.formatted <- alice.county %>% select(geoid = GEO.id2, year = Year, households = Households, alice_households = ALICE.Households, poverty_households = Poverty.Households) %>% mutate(alice_pct = round(alice_households / households, 4) * 100, poverty_pct = round(poverty_households / households, 4) * 100)

va_ct_2010_2021_alice <- alice.county.formatted %>% select(-c("households", "alice_households", "poverty_households")) %>% pivot_longer(cols = c("alice_pct", "poverty_pct"), names_to = "measure", values_to = "value") %>%
  mutate(measure_type = "percentage", moe="")

write_csv(va_ct_2010_2021_alice, xzfile("./Asset Limited Income Constrained Employed (ALICE)/data/distribution/va_ct_2010_2021_alice.csv.xz", compression = 9))



