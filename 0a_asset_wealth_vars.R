## 0a - Household asset and wealth variable preparation

# Inconsistent measurement of assets across 4 IHS waves (main difference in IHS2)
# Harmonize assets and their values 
  # Impute values for assets in IHS2 that do not have MWK vals from IHS2 -------
  # Use inflation adjusted MWK (2010 is index)
# Adjust for inflation for all waves 

## Adjust for CPI based on World Bank data [accessed January 14, 2025]
## https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2023&locations=MW&start=1981&view=chart
## CPI 2010 = 100
## IHS2, 2004: 55.6247640619101
## IHS3, 2010: 100.00
## IHS4, 2016: 305.0311745
## IHS5, 2019: 418.3443255

library(tidyverse)
library(readstata13)
library(foreign)

  
## 1. Calculate median MWK val in 2010 for assets not valued in IHS2 --------
## "Hand sprayer","Panga","Sickle", "Ox-cart","Axe","Hoe"
  
## Explore values over time -- looks approximately even with CPI vals
m2v <- read.dta("data/ihs2/sec_m1.dta") %>%
  mutate(wave="IHS2") %>%
  select(case_id,wave,good=m0a,m03a,m05a) %>%
  na.omit() %>%
  group_by(good) %>%
  summarise(med = median(m05a),
            mean = mean(m05a))

m3v <- read.dta("data/ihs3/hh_mod_l.dta") %>%
  select(case_id,good=hh_l02,hh_l03,hh_l05) %>% na.omit() %>%
  group_by(good) %>%
  summarise(med = median(hh_l05),
            mean = mean(hh_l05))

m4v <- read.dta13("data/ihs4/hh_mod_l.dta") %>%
  select(case_id,good=hh_l02,hh_l03,hh_l05) %>% na.omit() %>%
  group_by(good) %>%
  summarise(med = median(hh_l05),
            mean = mean(hh_l05))

m3v <- read.dta("data/ihs3/hh_mod_m.dta") %>%
              select(case_id,good=hh_m0a,hh_l03 = hh_m01, hh_l05=hh_m03) %>% 
              filter(good %in% c("axe","OX CART","sprayer","PANGA KNIFE",
                                 "HAND HOE","sickle")) %>% na.omit() %>%
  group_by(good) %>%
  summarise(med = median(hh_l05),
            mean = mean(hh_l05))

m4v <- read.dta13("data/ihs4/hh_mod_m.dta") %>% 
  select(case_id,good=hh_m0b,hh_l03 = hh_m01, hh_l05=hh_m03) %>%
  filter(good %in% c("axe","OX CART","sprayer","PANGA KNIFE",
                     "HAND HOE","sickle")) %>% na.omit() %>%
  group_by(good) %>%
  summarise(med = median(hh_l05),
            mean = mean(hh_l05))

m5v <- read.dta13("data/ihs5/HH_MOD_M.dta") %>% 
  select(case_id,good=hh_m0b,hh_l03 = hh_m01, hh_l05=hh_m03) %>% 
  filter(good %in% c("AXE","OX CART","SPRAYER","PANGA KNIFE",
                     "HAND HOE","SICKLE")) %>% na.omit() %>%
  group_by(good) %>%
  summarise(med = median(hh_l05),
            mean = mean(hh_l05))


## Impute median vals, adjusted from 2010 vals to 2004 vals
m2 <- read.dta("data/ihs2/sec_m1.dta") %>%
  select(case_id,good=m0a,m03a,m05a) %>% 
  bind_rows(.,read.dta("data/ihs2/sec_m2.dta") %>%
              select(case_id,good=m0b,m03a=m03b) %>% na.omit()) %>%
  filter(!(good %in% c("Boat or canoe","Fishing net","Wheelbarrow"))) %>%
  mutate(m05a = case_when(good == "Ox-cart" ~ 30000*0.556247640619101,
                          good == "Axe" ~ 300*0.556247640619101,
                          good == "Hand sprayer" ~ 3000*0.556247640619101,
                          good == "Panga" ~ 200*0.556247640619101,
                          good == "Sickle" ~ 150*0.556247640619101,
                          good == "Hoe" ~ 300*0.556247640619101,
                          TRUE ~ m05a)) %>%
  rowwise() %>%
  mutate(totval = m03a*m05a) %>%
  group_by(case_id) %>%
  summarise(assetval_mwk = sum(totval,na.rm=T),
            assets = sum(m03a,na.rm=T)) %>% 
  ungroup() %>%
  mutate(assetval_mwk_2010 = assetval_mwk/0.556247640619101,
         wave= "IHS2") 


m3 <- read.dta("data/ihs3/hh_mod_l.dta") %>%
  select(case_id,good=hh_l02,hh_l03,hh_l05) %>% 
  bind_rows(.,read.dta("data/ihs3/hh_mod_m.dta") %>%
              select(case_id,good=hh_m0a,hh_l03 = hh_m01, hh_l05=hh_m03) %>% 
              filter(good %in% c("axe","OX-CART","sprayer","PANGA KNIFE",
                                 "HAND HOE","sickle"))) %>%
  rowwise() %>%
  mutate(totval = hh_l03*hh_l05) %>%
  group_by(case_id) %>%
  summarise(assetval_mwk = sum(totval,na.rm=T),
            assets = sum(hh_l03,na.rm=T)) %>% 
  ungroup() %>%
  mutate(assetval_mwk_2010 = assetval_mwk/1,
         wave = "IHS3")
  
m4 <- read.dta13("data/ihs4/hh_mod_l.dta") %>%
  select(case_id,good=hh_l02,hh_l03,hh_l05) %>%
  bind_rows(.,read.dta13("data/ihs4/hh_mod_m.dta") %>% 
              select(case_id,good=hh_m0b,hh_l03 = hh_m01, hh_l05=hh_m03) %>% 
              filter(good %in% c("axe","OX CART","sprayer","PANGA KNIFE",
                                 "HAND HOE","sickle"))) %>%
  rowwise() %>%
  mutate(totval = hh_l03*hh_l05) %>%
  group_by(case_id) %>%
  summarise(assetval_mwk = sum(totval,na.rm=T),
            assets = sum(hh_l03,na.rm=T)) %>% 
  ungroup() %>%
  mutate(assetval_mwk_2010 = assetval_mwk/3.050311745,
         wave="IHS4") 


m5 <- read.dta13("data/ihs5/HH_MOD_L.dta") %>%
  select(case_id,good=hh_l02,hh_l03,hh_l05) %>%
  bind_rows(.,read.dta13("data/ihs5/HH_MOD_M.dta") %>% 
              select(case_id,good=hh_m0b,hh_l03 = hh_m01, hh_l05=hh_m03) %>% 
              filter(good %in% c("AXE","OX CART","SPRAYER","PANGA KNIFE",
                                 "HAND HOE","SICKLE"))) %>%
  rowwise() %>%
  mutate(totval = hh_l03*hh_l05) %>%
  group_by(case_id) %>%
  summarise(assetval_mwk = sum(totval,na.rm=T),
            assets = sum(hh_l03,na.rm=T)) %>% 
  ungroup() %>%
  mutate(assetval_mwk_2010 = assetval_mwk/4.183443255,
         wave="IHS5") 


a <- bind_rows(m2,m3,m4,m5) %>%
  mutate(wave = factor(wave,ordered = T,
                       levels=c("IHS2","IHS3","IHS4","IHS5")))

tapply(a$assetval_mwk_2010, a$wave, summary)
tapply(a$assets, a$wave, summary)

write_rds(a,"processed/assets_infladj.rds")


# mortar/pestle (mtondo), bed, table, chair, fan, air conditioner, radio ('wireless'), tape, CD/DVD player, HiFi     
# television, VCR, sewing machine, kerosene/paraffin stove, electric or gas stove, hot plate, refrigerator                    
# washing machine, bicycle, motorcycle/scooter, car, mini-bus, lorry                           
# beer-brewing drum, upholstered chair, sofa set, coffee table, cupboard, drawers, bureau       
# paraffin lantern, desk, clock, clothing iron, computer/computer accessories, sattelite dish                  
# solar panel, generator, radio with flash drive/micro CD










