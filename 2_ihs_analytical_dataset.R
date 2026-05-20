# 2 - Finalize analytical dataset for IHS: 
  # Geovariables: tree cover, forest loss, population density
    # 10 kilometer buffer areas 
  # Community level variables from step 0b
  # Proximity to PA's 
  # Transform relevant variables for modeling

library(tidyverse)
library(readstata13)
library(sf)

select <- dplyr::select

# Step 1 - Load household data and reduce to lat/lon + wave values; Fix issues with missing lat/lon ------
## Fix lat/lon issues --
  ## 1. IHS2 households -- merge lat/lon from community vars [FIXED]
  ## 2. 9 EAs from IHS5 that have 0 as coordinates -- can match from IHS5? [FIXED]
  ## 3. 11 households from IHS4 & IHS5 without coords [FIXED]
  ## 4. 112 households from IHS5 with 0 as coords, across 7 EAs [unable to fix, exclude from analysis]

hh0 <- read_rds("processed/ihs2345_hh_analytical.rds") %>%
  select(case_id,wave,lat,lon,ea,id=instanceid) 

hh1 <- hh0 %>% filter(!is.na(lat) & lat !=0)

## Fix 1.1 IHS2 points missing lat/lon ----
p2 <- st_read("data/ihs2/ihsptsj.shp") %>% 
  filter(Year == 2004) %>%
  dplyr::select(ea = CLUSTID) %>%
  mutate(wave = factor("IHS2", levels = c("IHS2","IHS3","IHS4","IHS5"), labels = c("IHS2","IHS3","IHS4","IHS5"),
                       ordered=T)) %>%
  dplyr::select(ea,wave) 

p2coords <- st_transform(p2,4326) %>% st_coordinates(.) %>%
  data.frame() %>% rename(lat=Y,lon=X) %>%
  bind_cols(.,p2) %>% select(-geometry)

hh2 <- hh0 %>% filter(wave=="IHS2") %>% select(-lat,-lon) %>%
  left_join(.,p2coords)

## Fix 1.2 - Households with 0 as coords? -- saved 2 EAs (32 / 144 households) -----
hh3 <- hh0 %>% filter(lat==0 & ea %in% hh1$ea) %>% select(-lat,-lon) %>%
  left_join(.,hh1 %>% select(ea,lat,lon) %>% unique())

## Fix 1.3 - Households without any coords?  - matched to remaining EAs -----
hh4 <- hh0 %>% filter(is.na(lat) & wave != "IHS2" & ea %in% hh1$ea) %>%
  select(-lat,-lon) %>% left_join(.,hh1 %>% select(ea,lat,lon) %>% unique()) 


## Fix 1.4 - MERGE all fixed hosuehold coordinates together ----
hh <- bind_rows(hh1,hh2,hh3,hh4) %>%
  mutate(coords = paste0("(",lon,", ",lat,")"))

## 112 households from IHS5 without known coords -- try to fix from community modules?
hh.x <- hh0 %>% filter(!(id %in% hh$id))
hh.xg <- read.dta13("data/ihs5/householdgeovariables_ihs5.dta") %>% 
  select(case_id,lat = ea_lat_mod, lon = ea_lon_mod) %>%
  filter(case_id %in% hh.x$case_id)

##### STARTING HH DATASET N = 47096
rm(hh.x);rm(hh.xg);rm(hh0);rm(hh1);rm(hh2);rm(hh3);rm(hh4);rm(p2);rm(p2coords)


gps <- hh %>% select(lat,lon) %>%
  mutate(coords = paste0("(",lon,", ",lat,")")) %>%
  unique()

# Step 2 - Geovariables ---------------------------------------------------------

hh2 <- left_join(hh,read_rds("processed/geovars.rds"))

## FC loss and rate for respective wave
g2 <- hh2 %>%
  select(-starts_with("popden"),-area_km2) %>%
  pivot_longer(.,starts_with("fc_lossrate_"),names_to = "ihs",
               names_prefix = "fc_lossrate_",
               values_to = "lossfc_km2") %>%
  filter(tolower(wave)==ihs) %>% select(-ihs) 

## Population density
g3 <- hh2 %>%
  select(-starts_with("fc_lossrate"),-area_km2) %>%
  pivot_longer(.,starts_with("popden_"),names_to = "ihs",
               names_prefix = "popden_",
               values_to = "popden") %>%
  filter(tolower(wave)==ihs) %>% select(-ihs) 


hh <- left_join(g2,g3)


rm(list=setdiff(ls(), "hh"))


# Step 3 - Integrate other household/community variables --------------------------------------------------

hh.vars <- read_rds("processed/ihs2345_hh_analytical.rds") %>% 
  rename(id = instanceid) %>%
  filter(id %in% hh$id) %>% 
  dplyr::select(-lat,-lon,-case_id,-ea)

hh.vars <- left_join(hh,hh.vars)

com.vars <- read_rds("processed/ihs2345_com_analytical.rds") %>%
  select(ea, wave, district, region, reside, c_dailymarket, c_weeklymarket, 
         c_dist_road_km, c_fwcharc_livelihood)

hh.full <- left_join(hh.vars %>% select(-region,-district,-reside),com.vars)

    # ## Examine those that did not merge [452 hh across 30 EAs]
    # hh.x <- hh.full %>% filter(is.na(c_fwcharc_livelihood)) 
    # ## 19 EAs did not merge to any household information
    # com.x <- com.vars %>% filter(!(ea %in% hh$ea))

hh <- hh.full %>% filter(!is.na(c_fwcharc_livelihood)) %>% 
  mutate(ea_id = paste0(wave," ",ea)) 

ea <- hh %>% select(wave,ea,ea_id) %>% unique()


# Step 4 - add indicators for protected area proximity -------

m <- st_read("data/gadm40_MWI_1.shp")
p <- st_read("data/MWI_protectedareas_clean_mar1.shp") %>% st_transform(.,st_crs(m)) 

## Key PAs: >10% loss and not known to have timber plantations
p2 <- p %>%
  filter(NAME %in% c("Chimaliro Forest Reserve",
                     "Dzalanyama Forest Reserv",
                     "Liwonde Forest Reserve",
                     "Michese Forest Reserve",
                     "Lengwe  National Park",
                     "Musisi"))

# Get unique EAs for each IHS wave
hp <- hh %>% 
  select(lon,lat,reside) %>% unique() %>%
  mutate(coords = paste0("(",lat,", ",lon,")"))

ep <- read_rds("processed/dataset_ihs_wave2345_gps.rds") %>% st_transform(.,st_crs(m)) %>%
  mutate(coords = paste0("(",lat,", ",lon,")")) %>%
  filter(coords %in% hp$coords) %>% 
  select(lat,lon,coords) %>% unique() %>%
  left_join(.,hp)

## Intersect EAs with PAs at 10 km
p10 <- st_buffer(p,10000)
pk10 <- st_buffer(p2,10000)

# 658 rural EAs within 10 km of any PA (838 total, with urban EAs)
x <- st_intersects(ep,p10,sparse=F) %>% data.frame() %>%
  rowwise() %>%
  mutate(pa10km = sum(c_across(X1:X26))) %>% ungroup %>%
  mutate(pa10km = ifelse(pa10km >0,1,0)) %>% select(pa10km)

# 164 EAs within 10 km of KEY PAs (169 total, with urban EAs)
x2 <- st_intersects(ep,pk10,sparse=F) %>% data.frame() %>%
  rowwise() %>%
  mutate(pa10km_key = sum(c_across(X1:X6))) %>% ungroup %>%
  mutate(pa10km_key = ifelse(pa10km_key >0,1,0)) %>% select(pa10km_key)

ep <- bind_cols(ep,x,x2)

hh <- hh %>%
  mutate(coords = paste0("(",lat,", ",lon,")")) %>%
  left_join(.,ep %>% st_set_geometry(NULL))

rm(x);rm(x2);rm(pk10);rm(p10);rm(ep);rm(hp);rm(p);rm(p2);rm(m)

# Step 5 - Prep for analysis (Scripts 3 and 4) ------------

## Scripts 3a and 3b - Dataset for tables and plots -------
hh2 <- hh %>% 
  mutate(primary_cooking = factor(fuel_grouped, ordered = F),
         market = ifelse(c_dailymarket=="Yes" | 
                           (!is.na(c_weeklymarket) & c_weeklymarket =="Yes"), 1, 0),
         roaddist = c_dist_road_km,
         assetval_usd10 = assetval_mwk_2010/147.63,
         fc_lossrate = lossfc_km2,
         c_wflivelihood = c_fwcharc_livelihood,
         assetval_mwk10_percap = assetval_mwk_2010/hhmem_tot,
         purch_charc_cat = case_when(primary_cooking == "Charcoal" ~ "Primary fuel",
                                     primary_cooking != "Charcoal" & charc_purc == 1 ~ "Supplementary fuel",
                                     primary_cooking != "Charcoal" & charc_purc == 0 ~ "No charcoal"),
         supp = ifelse(purch_charc_cat == "Supplementary fuel",1,0),
         nocharc = ifelse(purch_charc_cat == "No charcoal",1,0),
         wave = factor(wave,ordered=F),
         pf_charcoal = ifelse(primary_cooking == "Charcoal",1,0),
         pf_purchfw = ifelse(primary_cooking == "Purchased firewood",1,0),
         pf_collfw = ifelse(primary_cooking == "Collected firewood",1,0),
         elect = ifelse(primary_cooking=="Electrcity",1,0),
         biomass = ifelse(primary_cooking=="Other biomass",1,0),
         y = case_when(wave=="IHS2"~2004,
                       wave=="IHS3"~2010,
                       wave=="IHS4"~2016,
                       wave=="IHS5"~2019),
         rainy = ifelse(is.na(rainy),0,rainy),
         avg_time_collfw_hr = time_collfw/hhmem_tot) %>%
  rowwise() %>%
  mutate(hhmem_percfem = hhmem_gather_prop*100) %>% ungroup() %>%
  select(case_id:coords,
         hhwght,wave,district,region,reside,
         primary_cooking,pf_charcoal,pf_purchfw,pf_collfw,elect,biomass,supp,nocharc,
         purch_charc_cat,
         avg_time_collfw_hr,
         rainy,
         head_yrs,
         head_sex,
         assetval_mwk10_percap,
         hhmem_tot,
         hhmem_percfem,
         market,
         roaddist,
         c_wflivelihood,
         fc_baseline,
         fc_lossrate,
         popden,
         fuel_cook,
         pa10km,
         pa10km_key
         )

write_rds(hh2,"analytical_datasets/ihs_hh_full.rds")


## Script 4 - Modeling data subset ------
hh3 <- hh %>% filter(reside == "Rural") %>%
  mutate(primary_cooking = factor(fuel_grouped, ordered = F),
         wave = factor(wave,ordered=F),
         region = factor(region, ordered=F),
         assetval_mwk10_percap = assetval_mwk_2010/hhmem_tot,
         log_assetval = log10(assetval_mwk10_percap+0.001),
         rainy = ifelse(is.na(rainy),0,rainy),
         head_sex = ifelse(head_sex=="Female",1,0),
         hhsize_mc = hhmem_tot - mean(hhmem_tot),
         market = ifelse(c_dailymarket=="Yes" | 
                           (!is.na(c_weeklymarket) & c_weeklymarket =="Yes"), 1, 0),
         log_roaddist = log2(c_dist_road_km+0.001),
         fc_baseline = fc_baseline,
         fc_lossrate = lossfc_km2,
         c_wflivelihood = c_fwcharc_livelihood,
         log_popden = log2(popden+0.001),
         ea_id = paste0(wave,"_",ea),
         purch_charc_cat = case_when(primary_cooking == "Charcoal" ~ "Primary fuel",
                                     primary_cooking != "Charcoal" & charc_purc == 1 ~ "Supplementary fuel",
                                     primary_cooking != "Charcoal" & charc_purc == 0 ~ "No charcoal")) %>%
  rowwise() %>%
  mutate(hhmem_percfem = hhmem_gather_prop*10) %>% ungroup() %>%
  mutate(hhmem_percfem_mc = hhmem_percfem - 5)

hh3$head_age <- as.vector(scale(hh3$head_yrs))

hh3 <- hh3 %>% 
  select(case_id:coords,ea_id,
         hhwght,wave,district,region,reside,
         primary_cooking,
         purch_charc_cat,
         log_assetval,
         rainy,
         head_age,
         head_sex,
         hhmem_percfem_mc,
         hhsize_mc,
         market,
         log_roaddist,
         fc_baseline,
         fc_lossrate,
         c_wflivelihood,
         log_popden,
         region,
         pa10km,
         pa10km_key,
         wave)
  
    # ## Check: missing data?
    # ea <- hh3 %>% select(wave,ea_id) %>% unique()
    # ## 6 EAs in IHS2 without road distance data; 1 in IHS3
    # x1 <- hh3 %>% filter(is.na(log_roaddist))
    # length(unique(x1$ea))
  
write_rds(hh3, "analytical_datasets/household_fuelchoice_ihs2345.rds")



