# Replication package for: 
  # Beach et al., 2026 - "Do coupled population, economic, and land use dynamics explain household energy transitions in Malawi?" [World Development]

## 3c - calculation of forest area in protected areas
  ## [Table 3]

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)

# Step 1 - Load data -------------------------------------------------------------------
r <- c(rast("processed/fc_baseline2000.tif"),
       rast("processed/fc_loss_ihs2.tif"),
       rast("processed/fc_loss_ihs3.tif"),
       rast("processed/fc_loss_ihs4.tif"),
       rast("processed/fc_loss_ihs5.tif"))

f <- st_read("data/MWI_protectedareas_clean_mar1.shp") %>% st_transform(.,st_crs(r[[1]]))

# Baseline forest cover extent (km2)
fc_baseline <- exact_extract(r[[1]],f,'weighted_sum',weights='area')*0.000001

# Forest cover loss rate (km2)
x2 <- exact_extract(r[[2]],f,'weighted_sum',weights='area')*0.000001
x3 <- exact_extract(r[[3]],f,'weighted_sum',weights='area')*0.000001
x4 <- exact_extract(r[[4]],f,'weighted_sum',weights='area')*0.000001
x5 <- exact_extract(r[[5]],f,'weighted_sum',weights='area')*0.000001

f$area_km2 <- as.vector(st_area(f))*0.000001

f <- bind_cols(f %>% st_set_geometry(NULL),
               fc_baseline=fc_baseline,
               fc_loss_ihs2=x2,
               fc_loss_ihs3=x3,
               fc_loss_ihs4=x4,
               fc_loss_ihs5=x5) %>% 
  rowwise() %>%
  mutate(fc_loss_tot = sum(c_across(fc_loss_ihs2:fc_loss_ihs5))) %>%
  ungroup() %>%
  mutate(perc_fcloss = fc_loss_tot/fc_baseline*100) %>%
  arrange(desc(fc_loss_tot))


write_rds(f,"outputs/forestareas_loss_ihs_complete.rds")


g <- f %>% mutate_if(is.numeric, ~round(.x,2))

write_csv(g,"outputs/table3.csv")


