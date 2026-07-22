# Replication package for: 
  # Beach et al., 2026 - "Do coupled population, economic, and land use dynamics explain household energy transitions in Malawi?" [World Development]

## 1b - Extract geographic variables (forest cover, forest loss, pop density) for EAs

library(sf)
library(terra)
library(tidyverse)
library(exactextractr)

# Step 1 - Load data -----------------------------------

r <- c(rast("processed/fc_baseline2000.tif"),
       rast("processed/fc_loss_ihs2.tif"),
       rast("processed/fc_loss_ihs3.tif"),
       rast("processed/fc_loss_ihs4.tif"),
       rast("processed/fc_loss_ihs5.tif"))

dens <- c(rast("data/mwi_ppp_2004_UNadj.tif"),
          rast("data/mwi_ppp_2010_UNadj.tif"),
          rast("data/mwi_ppp_2016_UNadj.tif"),
          rast("data/mwi_ppp_2019_UNadj.tif"))

# EA points
p <- read_rds("processed/dataset_ihs_wave2345_gps.rds") %>%
  st_transform(.,st_crs(r[[1]])) %>%
  dplyr::select(lat,lon) %>% unique() %>%
  st_buffer(.,10000)

# Baseline forest cover in 10km radius area
fc_baseline <- exact_extract(r[[1]],p,'weighted_sum',weights='area')*0.000001

# Annual forest cover loss rate in 10km radius area
fc_lossrate_ihs2 <- exact_extract(r[[2]],p,'weighted_sum',weights='area')*0.000001/5
fc_lossrate_ihs3 <- exact_extract(r[[3]],p,'weighted_sum',weights='area')*0.000001/6
fc_lossrate_ihs4 <- exact_extract(r[[4]],p,'weighted_sum',weights='area')*0.000001/6
fc_lossrate_ihs5 <- exact_extract(r[[5]],p,'weighted_sum',weights='area')*0.000001/3

q <- bind_cols(p %>% st_set_geometry(NULL),
               fc_baseline=fc_baseline,
               fc_lossrate_ihs2=fc_lossrate_ihs2,
               fc_lossrate_ihs3=fc_lossrate_ihs3,
               fc_lossrate_ihs4=fc_lossrate_ihs4,
               fc_lossrate_ihs5=fc_lossrate_ihs5) 

# Population density in 10km-radius area
popden_ihs2 <- exact_extract(dens[[1]],p,'sum')
popden_ihs3 <- exact_extract(dens[[2]],p,'sum')
popden_ihs4 <- exact_extract(dens[[3]],p,'sum')
popden_ihs5 <- exact_extract(dens[[4]],p,'sum')

p$area_km2 <- as.vector(st_area(p))*0.000001

d <- bind_cols(p %>% st_set_geometry(NULL),
               popden_ihs2=popden_ihs2,
               popden_ihs3=popden_ihs3,
               popden_ihs4=popden_ihs4,
               popden_ihs5=popden_ihs5)

d$popden_ihs2 <- d$popden_ihs2/d$area_km2
d$popden_ihs3 <- d$popden_ihs3/d$area_km2
d$popden_ihs4 <- d$popden_ihs4/d$area_km2
d$popden_ihs5 <- d$popden_ihs5/d$area_km2

x <- left_join(q,d)

write_rds(x,"processed/geovars.rds")

