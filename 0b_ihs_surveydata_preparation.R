# Replication package for: 
  # Beach et al., 2026 - "Do coupled population, economic, and land use dynamics explain household energy transitions in Malawi?" [World Development]

# 0b - Extract variables from household and community EA data; harmonize across waves

# DATA: 
# Malawi's IHS on the World Bank's Microdata Library:
# https://microdata.worldbank.org/catalog/2307
# https://microdata.worldbank.org/catalog/1003
# https://microdata.worldbank.org/catalog/2936
# https://microdata.worldbank.org/catalog/3818

library(tidyverse)
library(readstata13)
library(foreign)
library(lubridate)
library(skimr)
library(sf)
library(table1)

select <- dplyr::select

# Step 1 - IHS Household Variables ------------------
## (1a) IHS 2 - 2004-2005 data -------
i <- read.dta("data/ihs2/sec_a.dta") %>%
  select(case_id,region,dist,ta,ea=psu,reside,a14b,a14c,hhwght) 

ihs2 <- read.dta("data/ihs2/sec_g.dta") %>%
  select(case_id,g12,g13,g14,g15) %>%
  left_join(i,.) %>%
  rename(month=a14b,year=a14c,fuel_cook=g13,firewood_use=g14,firewood_coll=g15) %>%
  mutate(region = factor(region, ordered = T),
         reside = factor(reside, ordered=T, levels=c("Rural","Urban"),labels=c("Rural","Urban")),
         ea = as.character(ea),
         ta = as.character(ta),
         district = factor(case_when(str_detect(dist,"Chitipa") ~ "Chitipa",
                                     str_detect(dist,"Karonga") ~ "Karonga",
                                     str_detect(dist,"Likoma") ~ "Likoma",
                                     str_detect(dist,"Mzimba") ~ "Mzimba",
                                     str_detect(dist,"Mzuzu") ~ "Mzimba",
                                     str_detect(dist,"Nkhata") ~ "Nkhata Bay",
                                     str_detect(dist,"Rumphi") ~ "Rumphi",
                                     str_detect(dist,"Dedza") ~ "Dedza",
                                     str_detect(dist,"Dowa") ~ "Dowa",
                                     str_detect(dist,"Kasungu") ~ "Kasungu",
                                     str_detect(dist,"Lilongwe") ~ "Lilongwe",
                                     str_detect(dist,"Mchinji") ~ "Mchinji",
                                     str_detect(dist,"Nkhota") ~ "Nkhotakota",
                                     str_detect(dist,"Ntcheu") ~ "Ntcheu",
                                     str_detect(dist,"Ntchisi") ~ "Ntchisi",
                                     str_detect(dist,"Salima") ~ "Salima",
                                     str_detect(dist,"Balaka") ~ "Balaka",
                                     str_detect(dist,"Blantyre") ~ "Blantyre",
                                     str_detect(dist,"Blanytyre") ~ "Blantyre",
                                     str_detect(dist,"Chikwawa") ~ "Chikwawa",
                                     str_detect(dist,"Chiradzulu") ~ "Chiradzulu",
                                     str_detect(dist,"Machinga") ~ "Machinga",
                                     str_detect(dist,"Mangochi") ~ "Mangochi",
                                     str_detect(dist,"Mulanje") ~ "Mulanje",
                                     str_detect(dist,"Mwanza") ~ "Mwanza",
                                     str_detect(dist,"Neno") ~ "Neno",
                                     str_detect(dist,"Nsanje") ~ "Nsanje",
                                     str_detect(dist,"Phalombe") ~ "Phalombe",
                                     str_detect(dist,"Thyolo") ~ "Thyolo",
                                     str_detect(dist,"Zomba") ~ "Zomba"),
                           levels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           labels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           ordered=T),
         dist = as.character(dist),
         g12 = ifelse(as.numeric(g12)==10,9,as.numeric(g12))) %>%
  mutate(fuel_light = factor(g12, levels=c(1:9), 
                             labels = c("Collected firewood","Purchased firewood","Grass","Paraffin","Electricity",
                                        "Gas","Battery/dry cell (torch)","Candles","Other")),
         firewood_use = factor(firewood_use,ordered = T,levels=c("No","Yes"),labels=c("No","Yes")),
         firewood_coll = factor(firewood_coll,ordered = T,levels=c("No","Yes"),labels=c("No","Yes")),
         wave = "IHS2") %>%
  mutate(wave = factor(wave,levels = c("IHS2","IHS3","IHS4","IHS5"), labels = c("IHS2","IHS3","IHS4","IHS5"),
                       ordered=T)) %>%
  select(wave,case_id,region,district,ta,ea,reside,month,year,hhwght,fuel_light,fuel_cook,firewood_use,firewood_coll,dist)

e <- read.dta("data/ihs2/sec_e.dta") %>%
  filter(e02!="X" & e07 <= 24) %>%
  group_by(case_id) %>%
  summarise(time_collfw = sum(e07))

j <- read.dta("data/ihs2/sec_j1.dta") %>%
  filter(j0a %in% c("Charcoal","Paraffin or kerosene")) %>%
  select(case_id,j0a,j03a) %>%
  pivot_wider(names_from = j0a, values_from = j03a) %>%
  rename(charc_1wk = Charcoal, paraffin_1wk = `Paraffin or kerosene`) 

b1 <- read.dta("data/ihs2/sec_b.dta") %>%
  filter(b04=="Head") %>%
  select(case_id, head_sex=b03, head_yrs = b05a)

b3 <-  read.dta("data/ihs2/sec_b.dta") %>%
  filter(b08=="Yes") %>%
  group_by(case_id) %>% 
  summarize(hhmem_tot = n(),
         hhmem_wom = sum(b03=="Female" & b05a >=15 & b05a <65,na.rm=T),
         hhmem_grl = sum(b03=="Female" & b05a <15 & b05a,na.rm=T),
         hhmem_eldwom = sum(b03=="Female" &  b05a >=65,na.rm=T),
         hhmem_boy = sum(b03=="Male" & b05a <15,na.rm=T),
         hhmem_men = sum(b03=="Male" & b05a >=15 & b05a <65,na.rm=T),
         hhmem_eldman = sum(b03=="Male" &  b05a >=65,na.rm=T)) %>%
  rowwise() %>%
  mutate(hhmem_gather_n = sum(c_across(hhmem_wom:hhmem_eldwom)),
         hhmem_gather_prop = hhmem_gather_n/hhmem_tot)

b4 <-  read.dta("data/ihs2/sec_b.dta") %>%
  #filter(b08=="Yes") %>%
  mutate(hhmem_grp = case_when(b03=="Female" & b05a >=65 ~ "Elder, female",
                               b03=="Male" & b05a >=65 ~ "Elder, male",
                               b03=="Female" & b05a <65 & b05a >=15 ~ "Adult, female",
                               b03=="Male" & b05a <65 & b05a >=15 ~ "Adult, male",
                               b03=="Female" & b05a <15 & b05a >=5 ~ "Child, female",
                               b03=="Male" & b05a <15 & b05a >=5 ~ "Child, male",
                               TRUE ~ "Baby")) %>%
  filter(hhmem_grp != "Baby") %>%
  select(case_id,hhid,memid,hhmem_grp,mem_age=b05a)

e4 <- read.dta("data/ihs2/sec_e.dta") %>%
  select(case_id,hhid,memid,e02,fw_collect_time = e07) %>%
  right_join(.,b4) %>% 
  mutate(fw_collect_time = ifelse(fw_collect_time > 24, 24, fw_collect_time)) %>%
  group_by(hhmem_grp) %>%
  summarise(fw_time = mean(fw_collect_time,na.rm=T))

 ag <- read.dta("data/ihs2/sec_o.dta") %>%
    select(case_id, num = o05a, unit = o05b) %>% 
   mutate(rainy = case_when(
     unit == "Acre" ~ num,
     unit == "Hectare" ~ num*2.471054,
     unit == "Sq. meters" ~ num*0.0001,
     TRUE ~ NA
   )) %>% na.omit() %>%
   group_by(case_id) %>%
   summarise(rainy = sum(rainy)) 


ihs2 <-left_join(ihs2,e) %>%
  left_join(.,j) %>%
  left_join(.,b1) %>%
  left_join(.,b3) %>%
  mutate(lat = NA, lon = NA) %>% 
  left_join(.,ag) 


## (1b) IHS 3 - 2010-2011 data ----
i <- read.dta("data/ihs3/hh_mod_a_filt.dta") %>%
  select(case_id,dist=hh_a01,ta=hh_a02,ea=ea_id,reside,a14b=hh_a23b_1,a14c=hh_a23c_1,hhwght=hh_wgt) %>%
  mutate(region = case_when(dist %in% c("Dedza","Dowa","Kasungu","Lilongwe","Mchinji",
                                        "Nkhotakota","Ntcheu","Ntchisi","Salima") ~ "Centre",
                            dist %in% c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay",
                                        "Rumphi") ~ "North",
                            dist %in% c("Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                        "Mulanje","Mwanza","Nsanje","Thyolo",
                                        "Phalombe","Zomba","Neno") ~ "South")) %>%
  select(case_id,region,everything())


ihs3 <- read.dta("data/ihs3/hh_mod_f.dta") %>%
  select(case_id,g12=hh_f11,g13=hh_f12,g14=hh_f13,g15=hh_f14) %>%
  left_join(i,.) %>%
  rename(month=a14b,year=a14c,firewood_use=g14,firewood_coll=g15) %>%
  mutate(reside = factor(reside, ordered=T, levels=c("rural","urban"),labels=c("Rural","Urban")),
         fuel_cook = factor(g13,levels=c(1:10),labels=base::levels(ihs2$fuel_cook)),
         ta = as.character(ta),
         district = factor(case_when(str_detect(dist,"Chitipa") ~ "Chitipa",
                                     str_detect(dist,"Karonga") ~ "Karonga",
                                     str_detect(dist,"Likoma") ~ "Likoma",
                                     str_detect(dist,"Mzimba") ~ "Mzimba",
                                     str_detect(dist,"Mzuzu") ~ "Mzimba",
                                     str_detect(dist,"Nkhata") ~ "Nkhata Bay",
                                     str_detect(dist,"Rumphi") ~ "Rumphi",
                                     str_detect(dist,"Dedza") ~ "Dedza",
                                     str_detect(dist,"Dowa") ~ "Dowa",
                                     str_detect(dist,"Kasungu") ~ "Kasungu",
                                     str_detect(dist,"Lilongwe") ~ "Lilongwe",
                                     str_detect(dist,"Mchinji") ~ "Mchinji",
                                     str_detect(dist,"Nkhota") ~ "Nkhotakota",
                                     str_detect(dist,"Ntcheu") ~ "Ntcheu",
                                     str_detect(dist,"Ntchisi") ~ "Ntchisi",
                                     str_detect(dist,"Salima") ~ "Salima",
                                     str_detect(dist,"Balaka") ~ "Balaka",
                                     str_detect(dist,"Blantyre") ~ "Blantyre",
                                     str_detect(dist,"Blanytyre") ~ "Blantyre",
                                     str_detect(dist,"Chikwawa") ~ "Chikwawa",
                                     str_detect(dist,"Chiradzulu") ~ "Chiradzulu",
                                     str_detect(dist,"Machinga") ~ "Machinga",
                                     str_detect(dist,"Mangochi") ~ "Mangochi",
                                     str_detect(dist,"Mulanje") ~ "Mulanje",
                                     str_detect(dist,"Mwanza") ~ "Mwanza",
                                     str_detect(dist,"Neno") ~ "Neno",
                                     str_detect(dist,"Nsanje") ~ "Nsanje",
                                     str_detect(dist,"Phalombe") ~ "Phalombe",
                                     str_detect(dist,"Thyolo") ~ "Thyolo",
                                     str_detect(dist,"Zomba") ~ "Zomba"),
                           levels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           labels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           ordered=T),
         dist = as.character(dist),
         fuel_light = factor(as.numeric(g12), levels=c(1:9), 
                             labels = c("Collected firewood","Purchased firewood","Grass","Paraffin","Electricity",
                                        "Gas","Battery/dry cell (torch)","Candles","Other")),
         firewood_use = factor(firewood_use,ordered = T,levels=c("No","Yes"),labels=c("No","Yes")),
         firewood_coll = factor(firewood_coll,ordered = T,levels=c("No","Yes"),labels=c("No","Yes")),
         wave = "IHS3",
         region = factor(case_when(district %in% c("Dedza","Dowa","Kasungu","Lilongwe","Mchinji",
                                                   "Nkhotakota","Ntcheu","Ntchisi","Salima") ~ "Centre",
                                   district %in% c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay",
                                                   "Rumphi") ~ "North",
                                   district %in% c("Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                                   "Mulanje","Mwanza","Nsanje","Thyolo",
                                                   "Phalombe","Zomba","Neno") ~ "South"),
                         levels = c("North","Centre","South"), labels = c("North","Centre","South"),ordered=T)) %>%
  mutate(wave = factor(wave,levels = c("IHS2","IHS3","IHS4","IHS5"), labels = c("IHS2","IHS3","IHS4","IHS5"),
                       ordered=T)) %>%
  select(wave,case_id,region,district,ta,ea,reside,month,year,hhwght,fuel_light,fuel_cook,firewood_use,firewood_coll,dist)

e <- read.dta("data/ihs3/hh_mod_e.dta") %>%
  filter(hh_e02!="X" & hh_e06 <= 24) %>%
  group_by(case_id) %>%
  summarise(time_collfw = sum(hh_e06))

j <- read.dta("data/ihs3/hh_mod_i1.dta") %>%
  filter(hh_i02 %in% c("Charcoal","Paraffin or kerosene")) %>%
  select(case_id,hh_i02,hh_i03) %>%
  pivot_wider(names_from = hh_i02, values_from = hh_i03) %>%
  rename(charc_1wk = Charcoal, paraffin_1wk = `Paraffin or kerosene`) 

b1 <- read.dta("data/ihs3/hh_mod_b.dta") %>%
  filter(hh_b04=="Head") %>%
  select(case_id, head_sex=hh_b03, head_yrs = hh_b05a) %>%
  filter(!(case_id=="103040120125" & head_yrs < 69))


b3 <-  read.dta("data/ihs3/hh_mod_b.dta") %>%
  group_by(case_id) %>% 
  summarize(hhmem_tot = n(),
            hhmem_wom = sum(hh_b03=="Female" & hh_b05a >=15 & hh_b05a <65,na.rm=T),
            hhmem_grl = sum(hh_b03=="Female" & hh_b05a <15 & hh_b05a,na.rm=T),
            hhmem_eldwom = sum(hh_b03=="Female" &  hh_b05a >=65,na.rm=T),
            hhmem_boy = sum(hh_b03=="Male" & hh_b05a <15,na.rm=T),
            hhmem_men = sum(hh_b03=="Male" & hh_b05a >=15 & hh_b05a <65,na.rm=T),
            hhmem_eldman = sum(hh_b03=="Male" & hh_b05a >=65,na.rm=T)) %>%
  rowwise() %>%
  mutate(hhmem_gather_n = sum(c_across(hhmem_wom:hhmem_eldwom)),
         hhmem_gather_prop = hhmem_gather_n/hhmem_tot)

b4 <-  read.dta("data/ihs3/hh_mod_b.dta") %>%
  mutate(hhmem_grp = case_when(hh_b03=="Female" & hh_b05a >=65 ~ "Elder, female",
                               hh_b03=="Male" & hh_b05a >=65 ~ "Elder, male",
                               hh_b03=="Female" & hh_b05a <65 & hh_b05a >=15 ~ "Adult, female",
                               hh_b03=="Male" & hh_b05a <65 & hh_b05a >=15 ~ "Adult, male",
                               hh_b03=="Female" & hh_b05a <15 & hh_b05a >=5 ~ "Child, female",
                               hh_b03=="Male" & hh_b05a <15 & hh_b05a >=5 ~ "Child, male",
                               TRUE ~ "Baby")) %>%
  filter(hhmem_grp != "Baby") %>%
  select(case_id,memid=id_code,hhmem_grp,mem_age=hh_b05a)

e4 <- read.dta("data/ihs3/hh_mod_e.dta") %>%
  filter(hh_e02!="X") %>%
  select(case_id,memid=id_code,fw_collect_time = hh_e06) %>%
  right_join(.,b4) %>% 
  mutate(fw_collect_time = ifelse(fw_collect_time > 24, 24, fw_collect_time)) %>%
  group_by(hhmem_grp) %>%
  summarise(fw_time = mean(fw_collect_time,na.rm=T))

ag <- read.dta("data/ihs3/ag_mod_c.dta") %>%
  select(case_id, num = ag_c04a, unit = ag_c04b) %>% na.omit() %>%
  mutate(unit = factor(case_when(unit == 1 ~ "acre",
                          unit == 2 ~ "hectare",
                          unit == 3 ~ "SQUARE METERS",
                          TRUE ~ NA)),
         ag_type = "rainy") %>% 
  mutate(ag_acre = case_when(
    unit == "acre" ~ num,
    unit == "hectare" ~ num*2.471054,
    unit == "SQUARE METERS" ~ num*0.0001,
    TRUE ~ NA
  )) %>% group_by(case_id) %>%
  summarise(rainy = sum(ag_acre)) 


ihs3 <-left_join(ihs3,e) %>%
  left_join(.,j) %>%
  left_join(.,b1) %>%
  left_join(.,b3) %>%
  left_join(.,read.dta("data/ihs3/householdgeovariables.dta") %>%
              select(case_id,lat=lat_modified,lon=lon_modified)) %>%
  left_join(.,ag)

## (1c) IHS 4 - 2016-2017 data -----

i <- read.dta13("data/ihs4/hh_mod_a_filt.dta") %>%
  select(case_id,region,dist=district,ta=hh_a02a,ea=ea_id,reside,a14b=interviewDate,a14c=interviewDate,hhwght=hh_wgt) %>%
  mutate(a14b=month(a14b),
         a14c=year(a14c))

ihs4 <- read.dta13("data/ihs4/hh_mod_f.dta") %>%
  select(case_id,g12=hh_f11,g13=hh_f12,g14=hh_f13,g15=hh_f14) %>%
  left_join(i,.) %>%
  rename(month=a14b,year=a14c,firewood_use=g14,firewood_coll=g15) %>%
  mutate(region = factor(as.numeric(region), ordered = T,levels=c(1:3),labels=c("North","Centre","South")),
         reside = factor(as.numeric(reside), ordered=T, levels=c(2,1),labels=c("Rural","Urban")),
         fuel_cook = factor(as.numeric(g13),levels=c(1:10),labels=base::levels(ihs2$fuel_cook)),
         ta = as.character(ta),
         district = factor(case_when(str_detect(dist,"Chitipa") ~ "Chitipa",
                                     str_detect(dist,"Karonga") ~ "Karonga",
                                     str_detect(dist,"Likoma") ~ "Likoma",
                                     str_detect(dist,"Mzimba") ~ "Mzimba",
                                     str_detect(dist,"Mzuzu") ~ "Mzimba",
                                     str_detect(dist,"Nkhata") ~ "Nkhata Bay",
                                     str_detect(dist,"Rumphi") ~ "Rumphi",
                                     str_detect(dist,"Dedza") ~ "Dedza",
                                     str_detect(dist,"Dowa") ~ "Dowa",
                                     str_detect(dist,"Kasungu") ~ "Kasungu",
                                     str_detect(dist,"Lilongwe") ~ "Lilongwe",
                                     str_detect(dist,"Mchinji") ~ "Mchinji",
                                     str_detect(dist,"Nkhota") ~ "Nkhotakota",
                                     str_detect(dist,"Ntcheu") ~ "Ntcheu",
                                     str_detect(dist,"Ntchisi") ~ "Ntchisi",
                                     str_detect(dist,"Salima") ~ "Salima",
                                     str_detect(dist,"Balaka") ~ "Balaka",
                                     str_detect(dist,"Blantyre") ~ "Blantyre",
                                     str_detect(dist,"Blanytyre") ~ "Blantyre",
                                     str_detect(dist,"Chikwawa") ~ "Chikwawa",
                                     str_detect(dist,"Chiradzulu") ~ "Chiradzulu",
                                     str_detect(dist,"Machinga") ~ "Machinga",
                                     str_detect(dist,"Mangochi") ~ "Mangochi",
                                     str_detect(dist,"Mulanje") ~ "Mulanje",
                                     str_detect(dist,"Mwanza") ~ "Mwanza",
                                     str_detect(dist,"Neno") ~ "Neno",
                                     str_detect(dist,"Nsanje") ~ "Nsanje",
                                     str_detect(dist,"Phalombe") ~ "Phalombe",
                                     str_detect(dist,"Thyolo") ~ "Thyolo",
                                     str_detect(dist,"Zomba") ~ "Zomba"),
                           levels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           labels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           ordered=T),
         dist = as.character(dist),
         fuel_light = factor(as.numeric(g12), levels=c(1:9), 
                             labels = c("Collected firewood","Purchased firewood","Grass","Paraffin","Electricity",
                                        "Gas","Battery/dry cell (torch)","Candles","Other")),
         firewood_use = factor(as.numeric(firewood_use),ordered = T,levels=c(2,1),labels=c("No","Yes")),
         firewood_coll = factor(as.numeric(firewood_coll),ordered = T,levels=c(2,1),labels=c("No","Yes")),
         wave = "IHS4") %>%
  mutate(wave = factor(wave,levels = c("IHS2","IHS3","IHS4","IHS5"), labels = c("IHS2","IHS3","IHS4","IHS5"),
                       ordered=T)) %>%
  select(wave,case_id,region,district,ta,ea,reside,month,year,hhwght,fuel_light,fuel_cook,firewood_use,firewood_coll,dist)

e <- read.dta13("data/ihs4/hh_mod_e.dta") %>%
  filter(!is.na(hh_e06)) %>%
  group_by(case_id) %>%
  summarise(time_collfw = sum(hh_e06))

j <- read.dta13("data/ihs4/hh_mod_i1.dta") %>%
  filter(hh_i02 %in% c("Charcoal","Paraffin or kerosene")) %>%
  select(case_id,hh_i02,hh_i03) %>%
  pivot_wider(names_from = hh_i02, values_from = hh_i03) %>%
  rename(charc_1wk = Charcoal, paraffin_1wk = `Paraffin or kerosene`) 

b1 <- read.dta13("data/ihs4/hh_mod_b.dta") %>%
  filter(hh_b04=="head") %>%
  mutate(head_sex = factor(ifelse(hh_b03 == "male", "Male","Female"),
                           levels = c("Male","Female"),
                           labels = c("Male","Female"))) %>%
  select(case_id, head_sex, head_yrs = hh_b05a) 

b3 <-  read.dta13("data/ihs4/hh_mod_b.dta") %>%
  group_by(case_id) %>% 
  summarize(hhmem_tot = n(),
            hhmem_wom = sum(hh_b03=="female" & hh_b05a >=15 & hh_b05a <65,na.rm=T),
            hhmem_grl = sum(hh_b03=="female" & hh_b05a <15 & hh_b05a,na.rm=T),
            hhmem_eldwom = sum(hh_b03=="female" &  hh_b05a >=65,na.rm=T),
            hhmem_boy = sum(hh_b03=="male" & hh_b05a <15,na.rm=T),
            hhmem_men = sum(hh_b03=="male" & hh_b05a >=15 & hh_b05a <65,na.rm=T),
            hhmem_eldman = sum(hh_b03=="male" & hh_b05a >=65,na.rm=T)) %>%
  rowwise() %>%
  mutate(hhmem_gather_n = sum(c_across(hhmem_wom:hhmem_eldwom)),
         hhmem_gather_prop = hhmem_gather_n/hhmem_tot)

b4 <-  read.dta13("data/ihs4/hh_mod_b.dta") %>%
  mutate(hhmem_grp = case_when(hh_b03=="female" & hh_b05a >=65 ~ "Elder, female",
                               hh_b03=="male" & hh_b05a >=65 ~ "Elder, male",
                               hh_b03=="female" & hh_b05a <65 & hh_b05a >=15 ~ "Adult, female",
                               hh_b03=="male" & hh_b05a <65 & hh_b05a >=15 ~ "Adult, male",
                               hh_b03=="female" & hh_b05a <15 & hh_b05a >=5 ~ "Child, female",
                               hh_b03=="male" & hh_b05a <15 & hh_b05a >=5 ~ "Child, male",
                               TRUE ~ "Baby")) %>%
  filter(hhmem_grp != "Baby") %>%
  select(case_id,memid=pid,hhmem_grp,mem_age=hh_b05a)

e4 <- read.dta13("data/ihs4/hh_mod_e.dta") %>%
  select(case_id,memid=pid,fw_collect_time = hh_e06) %>%
  right_join(.,b4) %>% 
  mutate(fw_collect_time = ifelse(fw_collect_time > 24, 24, fw_collect_time)) %>%
  group_by(hhmem_grp) %>%
  summarise(fw_time = mean(fw_collect_time,na.rm=T))

ag <- read.dta13("data/ihs4/ag_mod_b1.dta") %>%
  select(case_id, num = ag_b104a, unit = ag_b104b) %>% na.omit() %>%
  mutate(ag_acre = case_when(
    unit == "acre" ~ num,
    unit == "hectare" ~ num*2.471054,
    unit == "SQUARE METERS" ~ num*0.0001,
    TRUE ~ NA
  )) %>% group_by(case_id) %>%
    summarise(rainy = sum(ag_acre)) %>% ungroup() 

ihs4 <- left_join(ihs4,e) %>%
  left_join(.,j) %>%
  left_join(.,b1) %>%
  left_join(.,b3) %>%
  left_join(., read.dta13("data/ihs4/householdgeovariablesihs4.dta") %>%
              select(case_id,lat=lat_modified,lon=lon_modified)) %>%
  left_join(.,ag)


## (1d) IHS 5 - 2019-2020 data ------

i <- read.dta13("data/ihs5/hh_mod_a_filt.dta") %>%
  select(case_id,region,dist=district,ta=hh_a02a,ea=ea_id,reside,a14b=interviewDate,a14c=interviewDate,hhwght=hh_wgt) %>%
  mutate(a14b=month(a14b),
         a14c=year(a14c))

ihs5 <- read.dta13("data/ihs5/HH_MOD_F.dta") %>%
  select(case_id,g12=hh_f11,g13=hh_f12,g14=hh_f13,g15=hh_f14) %>%
  left_join(i,.) %>%
  rename(month=a14b,year=a14c,firewood_use=g14,firewood_coll=g15) %>%
  mutate(region = factor(as.numeric(region), ordered = T,levels=c(1:3),labels=c("North","Centre","South")),
         reside = factor(reside, ordered=T, levels=c(2,1),labels=c("Rural","Urban")),
         fuel_cook = factor(as.numeric(g13),levels=c(1:10),labels=base::levels(ihs2$fuel_cook)),
         as.character(ta),
         district = factor(case_when(str_detect(dist,"Chitipa") ~ "Chitipa",
                                     str_detect(dist,"Karonga") ~ "Karonga",
                                     str_detect(dist,"Likoma") ~ "Likoma",
                                     str_detect(dist,"Mzimba") ~ "Mzimba",
                                     str_detect(dist,"Mzuzu") ~ "Mzimba",
                                     str_detect(dist,"Nkhata") ~ "Nkhata Bay",
                                     str_detect(dist,"Rumphi") ~ "Rumphi",
                                     str_detect(dist,"Dedza") ~ "Dedza",
                                     str_detect(dist,"Dowa") ~ "Dowa",
                                     str_detect(dist,"Kasungu") ~ "Kasungu",
                                     str_detect(dist,"Lilongwe") ~ "Lilongwe",
                                     str_detect(dist,"Mchinji") ~ "Mchinji",
                                     str_detect(dist,"Nkhota") ~ "Nkhotakota",
                                     str_detect(dist,"Ntcheu") ~ "Ntcheu",
                                     str_detect(dist,"Ntchisi") ~ "Ntchisi",
                                     str_detect(dist,"Salima") ~ "Salima",
                                     str_detect(dist,"Balaka") ~ "Balaka",
                                     str_detect(dist,"Blantyre") ~ "Blantyre",
                                     str_detect(dist,"Blanytyre") ~ "Blantyre",
                                     str_detect(dist,"Chikwawa") ~ "Chikwawa",
                                     str_detect(dist,"Chiradzulu") ~ "Chiradzulu",
                                     str_detect(dist,"Machinga") ~ "Machinga",
                                     str_detect(dist,"Mangochi") ~ "Mangochi",
                                     str_detect(dist,"Mulanje") ~ "Mulanje",
                                     str_detect(dist,"Mwanza") ~ "Mwanza",
                                     str_detect(dist,"Neno") ~ "Neno",
                                     str_detect(dist,"Nsanje") ~ "Nsanje",
                                     str_detect(dist,"Phalombe") ~ "Phalombe",
                                     str_detect(dist,"Thyolo") ~ "Thyolo",
                                     str_detect(dist,"Zomba") ~ "Zomba"),
                           levels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           labels = c("Chitipa","Karonga","Likoma","Mzimba","Nkhata Bay","Rumphi",
                                      "Dedza","Dowa","Kasungu","Lilongwe","Mchinji","Nkhotakota","Ntcheu","Ntchisi",
                                      "Salima","Balaka","Blantyre","Chikwawa","Chiradzulu","Machinga","Mangochi",
                                      "Mulanje","Mwanza","Neno","Nsanje","Phalombe","Thyolo","Zomba"),
                           ordered=T),
         dist = as.character(dist),
         ta = as.character(ta),
         fuel_light = factor(as.numeric(g12), levels=c(1:9), 
                             labels = c("Collected firewood","Purchased firewood","Grass","Paraffin","Electricity",
                                        "Gas","Battery/dry cell (torch)","Candles","Other")),
         firewood_use = factor(as.numeric(firewood_use),ordered = T,levels=c(2,1),labels=c("No","Yes")),
         firewood_coll = factor(as.numeric(firewood_coll),ordered = T,levels=c(2,1),labels=c("No","Yes")),
         wave = "IHS5") %>%
  mutate(wave = factor(wave,levels = c("IHS2","IHS3","IHS4","IHS5"), labels = c("IHS2","IHS3","IHS4","IHS5"),
                       ordered=T)) %>%
  select(wave,case_id,region,district,ta,ea,reside,month,year,hhwght,fuel_light,fuel_cook,firewood_use,firewood_coll,dist)

e <- read.dta13("data/ihs5/HH_MOD_E.dta") %>%
  filter(!is.na(hh_e06)) %>%
  group_by(case_id) %>%
  summarise(time_collfw = sum(hh_e06))

j <- read.dta13("data/ihs5/HH_MOD_I1.dta") %>%
  filter(hh_i02 %in% c("Charcoal","Paraffin or kerosene")) %>%
  select(case_id,hh_i02,hh_i03) %>%
  pivot_wider(names_from = hh_i02, values_from = hh_i03) %>%
  rename(charc_1wk = Charcoal, paraffin_1wk = `Paraffin or kerosene`) 

b1 <- read.dta13("data/ihs5/HH_MOD_B.dta") %>%
  filter(hh_b04=="HEAD") %>%
  mutate(head_sex = factor(ifelse(hh_b03 == "MALE", "Male","Female"),
                           levels = c("Male","Female"),
                           labels = c("Male","Female"))) %>%
  select(case_id, head_sex, head_yrs = hh_b05a) 

b3 <- read.dta13("data/ihs5/HH_MOD_B.dta")  %>%
  group_by(case_id) %>% 
  summarize(hhmem_tot = n(),
            hhmem_wom = sum(hh_b03=="FEMALE" & hh_b05a >=15 & hh_b05a <65,na.rm=T),
            hhmem_grl = sum(hh_b03=="FEMALE" & hh_b05a <15 & hh_b05a,na.rm=T),
            hhmem_eldwom = sum(hh_b03=="FEMALE" &  hh_b05a >=65,na.rm=T),
            hhmem_boy = sum(hh_b03=="MALE" & hh_b05a <15,na.rm=T),
            hhmem_men = sum(hh_b03=="MALE" & hh_b05a >=15 & hh_b05a <65,na.rm=T),
            hhmem_eldman = sum(hh_b03=="MALE" & hh_b05a >=65,na.rm=T)) %>%
  rowwise() %>%
  mutate(hhmem_gather_n = sum(c_across(hhmem_wom:hhmem_eldwom)),
         hhmem_gather_prop = hhmem_gather_n/hhmem_tot)

b4 <-  read.dta13("data/ihs5/HH_MOD_B.dta") %>%
  mutate(hhmem_grp = case_when(hh_b03=="FEMALE" & hh_b05a >=65 ~ "Elder, female",
                               hh_b03=="MALE" & hh_b05a >=65 ~ "Elder, male",
                               hh_b03=="FEMALE" & hh_b05a <65 & hh_b05a >=15 ~ "Adult, female",
                               hh_b03=="MALE" & hh_b05a <65 & hh_b05a >=15 ~ "Adult, male",
                               hh_b03=="FEMALE" & hh_b05a <15 & hh_b05a >=5 ~ "Child, female",
                               hh_b03=="MALE" & hh_b05a <15 & hh_b05a >=5 ~ "Child, male",
                               TRUE ~ "Baby")) %>%
  filter(hhmem_grp != "Baby") %>%
  select(case_id,memid=PID,hhmem_grp,mem_age=hh_b05a)

e4 <- read.dta13("data/ihs5/HH_MOD_E.dta") %>%
  select(case_id,memid=PID,fw_collect_time = hh_e06) %>%
  right_join(.,b4) %>% 
  mutate(fw_collect_time = ifelse(fw_collect_time > 24, 24, fw_collect_time)) %>%
  group_by(hhmem_grp) %>%
  summarise(fw_time = mean(fw_collect_time,na.rm=T))

ag <- read.dta13("data/ihs5/ag_mod_c.dta") %>%
  select(case_id, num = ag_c04a, unit = ag_c04b) %>% na.omit() %>%
  mutate(ag_acre = case_when(
    unit == "ACRE" ~ num,
    unit == "HECTARE" ~ num*2.471054,
    unit == "SQUARE METERS" ~ num*0.0001,
    TRUE ~ NA
  )) %>% group_by(case_id) %>%
  summarise(rainy = sum(ag_acre)) %>% ungroup() 

ihs5 <-left_join(ihs5,e) %>%
  left_join(.,j) %>%
  left_join(.,b1) %>%
  left_join(.,b3) %>%
  left_join(., read.dta13("data/ihs5/householdgeovariables_ihs5.dta") %>%
              select(case_id,lat=ea_lat_mod,lon=ea_lon_mod)) %>%
  left_join(.,ag)

# (1e) Bind IHS Waves 2-5 data to one dataset --------
t1 <- bind_rows(ihs2,ihs3,ihs4,ihs5) %>%
  mutate(instanceid = c(1:47432)) %>%
  filter(district != "Likoma") %>%
  mutate(fw_use_updated = 
           case_when(fuel_cook %in% 
                       c("Collected firewood","Puchased firewood") ~ 1,
                     firewood_use=="Yes" ~ 1,
                     TRUE ~ 0),
         fw_coll_updated = case_when(fuel_cook == "Collected firewood" ~ 1,
                                     firewood_coll=="Yes" ~ 1,
                                     TRUE ~ 0),
         wave_yr = factor(wave,levels=c("IHS2","IHS3","IHS4","IHS5"),
                          labels=c("2004-05","2010-11","2016-17",
                                   "2019-20"),ordered=T),
         fuel_grouped = factor(
           case_when(fuel_cook %in% c("Crop residue","Saw dust", "Animal waste") ~ "Other biomass",
                     fuel_cook == "Collected firewood" ~ "Collected firewood",
                     fuel_cook == "Electricity" ~ "Electricity",
                     fuel_cook == "Charcoal" ~ "Charcoal",
                     fuel_cook == "Puchased firewood" ~ "Purchased firewood"), 
           levels = c("Collected firewood","Purchased firewood","Charcoal",
                      "Electricity","Other biomass"), 
           labels = c("Collected firewood","Purchased firewood","Charcoal",
                      "Electricity","Other biomass"),
           ordered=T),
         charc_purc = ifelse(is.na(charc_1wk),0,1)) %>%
  left_join(., read_rds("analytical_datasets/assets_infladj.rds"))


write_rds(t1,"processed/ihs2345_hh_analytical.rds")

rm(ihs2);rm(ihs3);rm(ihs4);rm(ihs5);rm(i);rm(e);rm(j);rm(b1);rm(b3);rm(b4);rm(e4);rm(ag)

# Step 2 - IHS EA Level Survey Variables -------------

## (2a) IHS 2 - Community variables ------
com <- read.dta("data/ihs2/mod_c.dta") %>%
  select(dist,ta,ea,cc14) %>%
  mutate(ea = paste0(dist,str_pad(ta,2,"0",side="left"),str_pad(ea,3,"0",side="left")),
         c_landforest = case_when(cc14 <= 10 ~ "ALMOST NONE",
                                  cc14 >10 & cc14 <= 35 ~ "1/4",
                                  cc14 >35 & cc14 <= 60 ~ "1/2",
                                  cc14 > 60 & cc14 <= 85 ~ "3/4",
                                  cc14 > 85 ~ "ALMOST ALL")) %>%
  select(ea,c_landforest)

d <- read.dta("data/ihs2/mod_d.dta") %>%
  select(dist,ta,ea,cd2aa,cd2bb,cd12,cd13a,cd13b,c_dailymarket = cd15,cd16a,cd16b, c_weeklymarket = cd17,cd18a,cd18b,c_admarcmarket = cd19,cd20a,cd20b) %>%
  mutate(c_dist_road_km = case_when(cd2bb==1 ~ cd2aa/1000,
                                    cd2bb==2 ~ cd2aa,
                                    cd2bb==3 ~ cd2aa/0.62137,
                                    is.na(cd2bb) ~ cd2aa),
         c_dist_urbcen_km = case_when(cd12=="Yes" ~ 0,
                                      cd13b=="meter" ~ cd13a/1000,
                                      cd13b=="km" ~ cd13a,
                                      cd13b=="mile" ~ cd13a/0.62137),
         c_dist_dailymarket_km = case_when(c_dailymarket== "Yes" ~ 0,
                                           cd16a==1 ~ cd16a/1000,
                                           cd16b==2 ~ cd16a,
                                           cd16b==3 ~ cd16a/0.62137),
         c_dist_weekmarket_km = case_when(c_weeklymarket =="Yes" ~ 0,
                                          cd18b=="meter" ~ cd18a/1000,
                                          cd18b=="km" ~ cd18a,
                                          cd18b=="mile" ~ cd18a/0.62137),
         c_dist_admarc_km = case_when(c_admarcmarket =="Yes" ~ 0,
                                      cd20b=="meter" ~ cd20a/1000,
                                      cd20b=="km" ~ cd20a,
                                      cd20b=="mile" ~ cd20a/0.62137),
         ea = paste0(dist,str_pad(ta,2,"0",side="left"),str_pad(ea,3,"0",side="left"))) %>%
  select(ea, starts_with("c_"))

e <- read.dta("data/ihs2/mod_e.dta") %>%
  select(dist,ta,ea,ce1a,ce1b,ce1c,ce2a,ce3,ce4,ce5a,ce5b) %>%
  mutate(c_fwcharc_livelihood = case_when(ce1a=="Firewood, Charcoal" ~ 1,
                                          ce1b==3 ~ 1, 
                                          ce1c==3 ~ 1, 
                                          TRUE~0),
         c_migrate_work_perc = factor(case_when(ce2a=="No" ~ "NONE",
                                                ce3 <= 10 ~ "ALMOST NONE",
                                                ce3 >10 & ce3 <= 35 ~ "1/4",
                                                ce3 >35 & ce3 <= 60 ~ "1/2",
                                                ce3 > 60 & ce3 <= 85 ~ "3/4",
                                                ce3 > 85 ~ "ALMOST ALL"), ordered=T,
                                      levels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL"),
                                      labels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL")),
         c_migrate_work_dest = factor(case_when(ce2a=="No" ~ "No migration",
                                                TRUE ~ as.character(ce4)), 
                                      levels = c("No migration","Rural areas", "Urban centres","Outside Malawi"), labels = c("No migration","Rural areas", "Urban centres","Outside Malawi")),
         c_migrate_work_livelihood = ifelse(ce5a==3 | ce5b==3,1,0),
         ea = paste0(dist,str_pad(ta,2,"0",side="left"),str_pad(ea,3,"0",side="left"))) %>%
  select(ea,starts_with("c_"))

com2f <- com %>%
  mutate(wave="IHS2") %>%
  mutate(wave=factor(wave,levels=c("IHS2","IHS3","IHS4","IHS5"),
                     labels = c("IHS2","IHS3","IHS4","IHS5"), 
                     ordered=T)) %>%
  left_join(.,t1 %>% filter(wave=="IHS2") %>%
              select(ea,region,district,ta,reside) %>%
              unique(),by=c("ea")) %>%
  left_join(.,d) %>% left_join(.,e) %>% 
  select(wave,ea,region,district,ta,reside,everything())


## (2b) IHS 3 - Community variables ------

com <- read.dta("data/ihs3/com_cc.dta") %>%
  select(ea=ea_id,c_landforest = com_cc13)

d <- read.dta("data/ihs3/com_cd.dta") %>%
  select(ea=ea_id,com_cd02a,com_cd02b,
         com_cd12,com_cd13a,com_cd13b,com_cd15,com_cd16a,com_cd16b,
         com_cd17,com_cd18a,com_cd18b,com_cd19,com_cd20a,com_cd20b) %>%
  mutate(c_dist_road_km = case_when(com_cd02a==0 ~ 0, 
                                    com_cd02b==1 ~ com_cd02a/1000,
                                    com_cd02b==2 ~ com_cd02a/1,
                                    com_cd02b==3 ~ com_cd02a/0.62137),
         c_dist_urbcen_km = case_when(com_cd12=="yes" ~ 0,
                                      com_cd13b=="meter" ~ com_cd13a/1000,
                                      com_cd13b=="km" ~ com_cd13a/1,
                                      com_cd13b=="mile" ~ com_cd13a/0.62137),
         c_dailymarket = factor(case_when(com_cd15=="yes" ~ "Yes",
                                          com_cd15=="no" ~ "No"),
                                levels=levels(com2f$c_dailymarket),
                                labels=levels(com2f$c_dailymarket)),
         c_dist_dailymarket_km = case_when(com_cd15=="yes" ~ 0,
                                           com_cd16a=="meter" ~ com_cd16a/1000,
                                           com_cd16b=="km" ~ com_cd16a/1,
                                           com_cd16b=="mile" ~ com_cd16a/0.62137),
         c_weeklymarket = factor(case_when(com_cd17=="yes" ~ "Yes",
                                           com_cd17=="no" ~ "No"),
                                 levels=levels(com2f$c_weeklymarket),
                                 labels=levels(com2f$c_weeklymarket)),
         c_dist_weekmarket_km = case_when(com_cd17 =="yes" ~ 0,
                                          com_cd18b=="meter" ~ com_cd18a/1000,
                                          com_cd18b=="km" ~ com_cd18a/1,
                                          com_cd18b=="mile" ~ com_cd18a/0.62137),
         c_admarcmarket = factor(case_when(com_cd19=="yes" ~ "Yes",
                                           com_cd19=="no" ~ "No"),
                                 levels=levels(com2f$c_admarcmarket),
                                 labels=levels(com2f$c_admarcmarket)),
         c_dist_admarc_km = case_when(com_cd19 =="yes" ~ 0,
                                      com_cd20b=="meter" ~ com_cd20a/1000,
                                      com_cd20b=="km" ~ com_cd20a/1,
                                      com_cd20b=="mile" ~ com_cd20a/0.62137)) %>%
  select(ea, starts_with("c_"))

e <- read.dta("data/ihs3/com_ce.dta") %>%
  select(ea=ea_id,com_ce01a,com_ce01b,com_ce01c,com_ce03,com_ce02,com_ce04,com_ce05a,com_ce05b) %>%
  mutate(c_fwcharc_livelihood = case_when(com_ce01a=="FIREWOOD, CHARCOAL SELLING" ~ 1,
                                          com_ce01b=="FIREWOOD, CHARCOAL SELLING" ~ 1, 
                                          com_ce01c=="FIREWOOD, CHARCOAL SELLING" ~ 1, 
                                          TRUE~0),
         c_migrate_work_perc= factor(case_when(com_ce02=="no" ~ "NONE",
                                               com_ce02=="yes" ~ as.character(com_ce03)), ordered=T,
                                     levels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL"),
                                     labels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL")),
         c_migrate_work_dest = factor(case_when(com_ce02=="no" ~ "No migration",
                                                com_ce04=="RURAL AREAS" ~ "Rural areas",
                                                com_ce04=="URBAN CENTRES" ~ "Urban centres",
                                                com_ce04=="OUTSIDE MALAWI" ~ "Outside Malawi"), 
                                      levels = c("No migration","Rural areas", "Urban centres","Outside Malawi"), labels = c("No migration","Rural areas", "Urban centres","Outside Malawi")),
         c_migrate_work_livelihood = ifelse(com_ce05a=="FIREWOOD, CHARCOAL SELLING" | 
                                              com_ce05b=="FIREWOOD, CHARCOAL SELLING",1,0)) %>%
  select(ea,starts_with("c_"))

com3f <- com %>%
  mutate(wave="IHS3") %>%
  mutate(wave=factor(wave,levels=c("IHS2","IHS3","IHS4","IHS5"),
                     labels = c("IHS2","IHS3","IHS4","IHS5"), 
                     ordered=T)) %>%
  left_join(.,t1 %>% filter(wave=="IHS3") %>%
              select(ea,region,district,ta,reside) %>%
              unique(),by=c("ea")) %>%
  left_join(.,d) %>% left_join(.,e) %>% 
  select(wave,ea,region,district,ta,reside,everything())

## (2c) IHS 4 - Community variables ------

com <- read.dta13("data/ihs4/com_cc.dta") %>%
  select(ea=ea_id,c_landforest = com_cc13)

d <- read.dta13("data/ihs4/com_cd.dta") %>%
  select(ea=ea_id,com_cd02a,com_cd02b,
         com_cd12,com_cd13a,com_cd13b,com_cd15,com_cd16,com_cd16b,
         com_cd17,com_cd18a,com_cd18b,com_cd19,com_cd20a,com_cd20b) %>%
  mutate(c_dist_road_km = case_when(com_cd02a==0 ~ 0, 
                                    com_cd02b=="meter" ~ com_cd02a/1000,
                                    com_cd02b=="km" ~ com_cd02a/1,
                                    com_cd02b=="mile" ~ com_cd02a/0.62137),
         c_dist_urbcen_km = case_when(com_cd12=="yes" ~ 0,
                                      com_cd13b=="meter" ~ com_cd13a/1000,
                                      com_cd13b=="km" ~ com_cd13a/1,
                                      com_cd13b=="mile" ~ com_cd13a/0.62137),
         c_dailymarket = factor(case_when(com_cd15=="yes" ~ "Yes",
                                          com_cd15=="no" ~ "No"),
                                levels=levels(com2f$c_dailymarket),
                                labels=levels(com2f$c_dailymarket)),
         c_dist_dailymarket_km = case_when(com_cd15=="yes" ~ 0,
                                           com_cd16b=="meter" ~ com_cd16/1000,
                                           com_cd16b=="km" ~ com_cd16/1,
                                           com_cd16b=="mile" ~ com_cd16/0.62137),
         c_weeklymarket = factor(case_when(com_cd17=="yes" ~ "Yes",
                                           com_cd17=="no" ~ "No"),
                                 levels=levels(com2f$c_weeklymarket),
                                 labels=levels(com2f$c_weeklymarket)),
         c_dist_weekmarket_km = case_when(com_cd17 =="yes" ~ 0,
                                          com_cd18b=="meter" ~ com_cd18a/1000,
                                          com_cd18b=="km" ~ com_cd18a/1,
                                          com_cd18b=="mile" ~ com_cd18a/0.62137),
         c_admarcmarket = factor(case_when(com_cd19=="yes" ~ "Yes",
                                           com_cd19=="no" ~ "No"),
                                 levels=levels(com2f$c_admarcmarket),
                                 labels=levels(com2f$c_admarcmarket)),
         c_dist_admarc_km = case_when(com_cd19 =="yes" ~ 0,
                                      com_cd20b=="meter" ~ com_cd20a/1000,
                                      com_cd20b=="km" ~ com_cd20a/1,
                                      com_cd20b=="mile" ~ com_cd20a/0.62137)) %>%
  select(ea, starts_with("c_"))

e <- read.dta13("data/ihs4/com_ce.dta") %>%
  select(ea=ea_id,com_ce01a,com_ce01b,com_ce01c,com_ce03,com_ce02,com_ce04,com_ce05a,com_ce05b) %>%
  mutate(c_fwcharc_livelihood = case_when(com_ce01a=="FIREWOOD,CHARCOAL SELLING" ~ 1,
                                          com_ce01b=="FIREWOOD,CHARCOAL SELLING" ~ 1, 
                                          com_ce01c=="FIREWOOD,CHARCOAL SELLING" ~ 1, 
                                          TRUE~0),
         c_migrate_work_perc= factor(case_when(com_ce02=="no" ~ "NONE",
                                               com_ce02=="yes" ~ as.character(com_ce03)), ordered=T,
                                     levels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL"),
                                     labels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL")),
         c_migrate_work_dest = factor(case_when(com_ce02=="no" ~ "No migration",
                                                com_ce04=="RURAL AREAS" ~ "Rural areas",
                                                com_ce04=="URBAN CENTRES" ~ "Urban centres",
                                                com_ce04=="OUTSIDE MALAWI" ~ "Outside Malawi"), 
                                      levels = c("No migration","Rural areas", "Urban centres","Outside Malawi"), labels = c("No migration","Rural areas", "Urban centres","Outside Malawi")),
         c_migrate_work_livelihood = case_when(com_ce05a=="FIREWOOD,CHARCOAL SELLING" | 
                                                 com_ce05b=="FIREWOOD,CHARCOAL SELLING" ~ 1,
                                               TRUE ~ 0)) %>%
  select(ea,starts_with("c_"))


com4f <- com %>%
  mutate(wave="IHS4") %>%
  mutate(wave=factor(wave,levels=c("IHS2","IHS3","IHS4","IHS5"),
                     labels = c("IHS2","IHS3","IHS4","IHS5"), 
                     ordered=T)) %>%
  left_join(.,t1 %>% filter(wave=="IHS4") %>%
              select(ea,region,district,ta,reside) %>%
              unique(),by=c("ea")) %>%
  left_join(.,d) %>% left_join(.,e) %>% 
  select(wave,ea,region,district,ta,reside,everything())

## (2d) IHS 5 - Community variables ------

com <- read.dta13("data/ihs5/com_cc.dta") %>%
  select(ea=ea_id,c_landforest = com_cc13)

d <- read.dta13("data/ihs5/com_cd.dta") %>%
  select(ea=ea_id,com_cd02a,com_cd02b,
         com_cd12,com_cd13a,com_cd13b,com_cd15,com_cd16,com_cd16b,
         com_cd17,com_cd18a,com_cd18b,com_cd19,com_cd20a,com_cd20b) %>%
  mutate(c_dist_road_km = case_when(com_cd02a==0 ~ 0, 
                                    com_cd02b=="METER" ~ com_cd02a/1000,
                                    com_cd02b=="KM" ~ com_cd02a/1,
                                    com_cd02b=="MILE" ~ com_cd02a/0.62137),
         c_dist_urbcen_km = case_when(com_cd12=="YES" ~ 0,
                                      com_cd13b=="METER" ~ com_cd13a/1000,
                                      com_cd13b=="KM" ~ com_cd13a/1,
                                      com_cd13b=="MILE" ~ com_cd13a/0.62137),
         c_dailymarket = factor(case_when(com_cd15=="YES" ~ "Yes",
                                          com_cd15=="NO" ~ "No"),
                                levels=levels(com2f$c_dailymarket),
                                labels=levels(com2f$c_dailymarket)),
         c_dist_dailymarket_km = case_when(com_cd15=="YES" ~ 0,
                                           com_cd16b=="METER" ~ com_cd16/1000,
                                           com_cd16b=="KM" ~ com_cd16/1,
                                           com_cd16b=="MILE" ~ com_cd16/0.62137),
         c_weeklymarket = factor(case_when(com_cd17=="YES" ~ "Yes",
                                           com_cd17=="NO" ~ "No"),
                                 levels=levels(com2f$c_weeklymarket),
                                 labels=levels(com2f$c_weeklymarket)),
         c_dist_weekmarket_km = case_when(com_cd17 =="YES" ~ 0,
                                          com_cd18b=="METER" ~ com_cd18a/1000,
                                          com_cd18b=="KM" ~ com_cd18a/1,
                                          com_cd18b=="MILE" ~ com_cd18a/0.62137),
         c_admarcmarket = factor(case_when(com_cd19=="YES" ~ "Yes",
                                           com_cd19=="NO" ~ "No"),
                                 levels=levels(com2f$c_admarcmarket),
                                 labels=levels(com2f$c_admarcmarket)),
         c_dist_admarc_km = case_when(com_cd19 =="YES" ~ 0,
                                      com_cd20b=="METER" ~ com_cd20a/1000,
                                      com_cd20b=="KM" ~ com_cd20a/1,
                                      com_cd20b=="MILE" ~ com_cd20a/0.62137)) %>%
  select(ea, starts_with("c_"))

e <- read.dta13("data/ihs5/com_ce.dta") %>%
  select(ea=ea_id,com_ce01a,com_ce01b,com_ce01c,com_ce03,com_ce02,com_ce04,com_ce05a,com_ce05b) %>%
  mutate(c_fwcharc_livelihood = case_when(com_ce01a=="FIREWOOD,CHARCOAL SELLING" ~ 1,
                                          com_ce01b=="FIREWOOD,CHARCOAL SELLING" ~ 1, 
                                          com_ce01c=="FIREWOOD,CHARCOAL SELLING" ~ 1, 
                                          TRUE~0),
         c_migrate_work_perc= factor(case_when(com_ce02=="NO" ~ "NONE",
                                               com_ce02=="YES" ~ as.character(com_ce03)), ordered=T,
                                     levels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL"),
                                     labels=c("NONE","ALMOST NONE","1/4","1/2","3/4","ALMOST ALL")),
         c_migrate_work_dest = factor(case_when(com_ce02=="NO" ~ "No migration",
                                                com_ce04=="RURAL AREAS" ~ "Rural areas",
                                                com_ce04=="URBAN CENTRES" ~ "Urban centres",
                                                com_ce04=="OUTSIDE MALAWI" ~ "Outside Malawi"), 
                                      levels = c("No migration","Rural areas", "Urban centres","Outside Malawi"), labels = c("No migration","Rural areas", "Urban centres","Outside Malawi")),
         c_migrate_work_livelihood = case_when(com_ce05a=="FIREWOOD,CHARCOAL SELLING" | 
                                                 com_ce05b=="FIREWOOD,CHARCOAL SELLING" ~ 1,
                                               TRUE ~ 0)) %>%
  select(ea,starts_with("c_"))


com5f <- com %>%
  mutate(wave="IHS5") %>%
  mutate(wave=factor(wave,levels=c("IHS2","IHS3","IHS4","IHS5"),
                     labels = c("IHS2","IHS3","IHS4","IHS5"), 
                     ordered=T)) %>%
  left_join(.,t1 %>% filter(wave=="IHS5") %>%
              select(ea,region,district,ta,reside) %>%
              unique(),by=c("ea")) %>%
  left_join(.,d) %>% left_join(.,e) %>% 
  select(wave,ea,region,district,ta,reside,everything())

## (2e) Bind all community level variables together --------

com <- bind_rows(com2f,com3f,com4f,com5f)

write_rds(com,"processed/ihs2345_com_analytical.rds")

rm(d);rm(e);rm(com2f);rm(com3f);rm(com4f);rm(com5f)

# Step 3 - IHS EA Level GPS Coordinate Key -----------------------------------------------------------------
  # Summary:
    # From all IHS Waves 2-5, create a key for all valid EAs with Lat/Lon coordinates for creating geovars
    # GPS KEY: ea, gps_id, wave, lat, lon

t1.ea <- t1 %>%
  dplyr::select(ea, wave, lat,lon) %>% unique() %>%
  filter(!is.na(lat) & !is.na(lon) & lat!=0 & lon !=0) %>%
  mutate(gps_id = 1:n()) %>%
  left_join(.,t1 %>% select(ea,wave,lat,lon) %>% unique())

p <- st_as_sf(t1.ea, coords = c("lon","lat"), crs=4326) %>%
  st_transform(.,32736) %>%
  left_join(.,t1.ea)

## IHS2 geopoints from PJ & CPH [not publicly available currently on IHS site]
p2 <- st_read("data/ihs2/ihsptsj.shp") %>% 
  filter(Year == 2004) %>%
  dplyr::select(ea = CLUSTID) %>%
  mutate(gps_id = c(3001:3564),
         wave = factor("IHS2", levels = c("IHS2","IHS3","IHS4","IHS5"), labels = c("IHS2","IHS3","IHS4","IHS5"),
                              ordered=T)) %>%
  dplyr::select(gps_id,ea,wave)

p2coords <- st_transform(p2,4326) %>% st_coordinates(.) %>%
  data.frame() %>% rename(lat=Y,lon=X) %>%
  bind_cols(.,p2) %>%
  select(names(p))

gps <- bind_rows(p,p2coords)

write_rds(gps,"processed/dataset_ihs_wave2345_gps.rds")



