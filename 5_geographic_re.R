## 5 - Geographic anlaysis of random effects 
  ## [Create Figure A1]

library(tidyverse)
library(ggplot2)
library(sf)
library(ggthemes)
library(sp)
library(spdep)

p <- read_rds("analytical_datasets/raneffects_pfmod.rds") %>%
  mutate(wave = substr(ea_id,1,4),
         coord = paste0(lat,", ",lon)) %>%
  filter(duplicated(coord) == FALSE) %>%
  st_as_sf(., coords = c("lon","lat"), crs=4326)
  

mwi0 <- st_read("data/gadm40_MWI_0.shp") %>% st_transform(.,st_crs(p))
mwi1 <- st_read("data/gadm40_MWI_1.shp") %>% st_transform(.,st_crs(p))

# Step 1 - Calculate Global Moran's I for each wave ------

## IHS 2 -----
pd <- p %>% st_transform(.,32736)

d2 <- pd %>% filter(wave=="IHS2")
d3 <- pd %>% filter(wave=="IHS3")
d4 <- pd %>% filter(wave=="IHS4")
d5 <- pd %>% filter(wave=="IHS5")


## convert to an SP object
i <- as(d2, "Spatial")
coords <- coordinates(i)
rn <- i$rowid

# Find minimum distance for at least 1 neighbor
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)


# Moran's I for raneff
mor2 <- moran.mc(x = i$c1, listw = dnnw,
                 zero.policy = FALSE, nsim = 5000, adjust.n = T)
mor2

## IHS 3  ------
## convert to an SP object
i <- as(d3, "Spatial")
coords <- coordinates(i)
rn <- i$rowid

# Find minimum distance for at least 1 neighbor
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)


# Moran's I for raneff
mor3 <- moran.mc(x = i$c1, listw = dnnw,
                 zero.policy = FALSE, nsim = 5000, adjust.n = T)
mor3

## IHS 4  ------
## convert to an SP object
i <- as(d4, "Spatial")
coords <- coordinates(i)
rn <- i$rowid

# Find minimum distance for at least 1 neighbor
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)


# Moran's I for raneff
mor4 <- moran.mc(x = i$c1, listw = dnnw,
                 zero.policy = FALSE, nsim = 5000, adjust.n = T)
mor4

## IHS 5 ------
## convert to an SP object
i <- as(d5, "Spatial")
coords <- coordinates(i)
rn <- i$rowid

# Find minimum distance for at least 1 neighbor
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)


# Moran's I for raneff
mor5 <- moran.mc(x = i$c1, listw = dnnw,
                 zero.policy = FALSE, nsim = 5000, adjust.n = T)
mor5

# Local Clustering Analysis: Ran Effects -------
## IHS 2 - LISA, Dist Neighbors ------
i <- as(d2,"Spatial")
coords <- coordinates(i)
rn <- i$rowid
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)

## LISA
lisa <- as.data.frame(localmoran(i$c1, dnnw, zero.policy = FALSE))

lisa$Cat <- rep("0", nrow(lisa))
# Scale the input data to mean
cDV <- i$c1 - mean(i$c1) 
# Get spatial lag values for each observation
lagDV <- lag.listw(dnnw, i$c1)

# Scale the lag values
clagDV <- lagDV - mean(lagDV, na.rm = TRUE)

# This simply adds a label based on the values
lisa$Cat[which(cDV > 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "HH" 
lisa$Cat[which(cDV < 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "LL"      
lisa$Cat[which(cDV < 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "LH"
lisa$Cat[which(cDV > 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "HL"

## Quick summary of LISA output
table(lisa$Cat)
lisa$rowid <- as.integer(rownames(lisa))
d2 <- bind_cols(d2,lisa)

## IHS 3 - LISA, Dist Neighbors ------
i <- as(d3,"Spatial")
coords <- coordinates(i)
rn <- i$rowid
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)

## LISA
lisa <- as.data.frame(localmoran(i$c1, dnnw, zero.policy = FALSE))

lisa$Cat <- rep("0", nrow(lisa))
# Scale the input data to mean
cDV <- i$c1 - mean(i$c1) 
# Get spatial lag values for each observation
lagDV <- lag.listw(dnnw, i$c1)

# Scale the lag values
clagDV <- lagDV - mean(lagDV, na.rm = TRUE)

# This simply adds a label based on the values
lisa$Cat[which(cDV > 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "HH" 
lisa$Cat[which(cDV < 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "LL"      
lisa$Cat[which(cDV < 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "LH"
lisa$Cat[which(cDV > 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "HL"

## Quick summary of LISA output
table(lisa$Cat)
lisa$rowid <- as.integer(rownames(lisa))
d3 <- bind_cols(d3,lisa)


## IHS 4 - LISA, Dist Neighbors ------
i <- as(d4,"Spatial")
coords <- coordinates(i)
rn <- i$rowid
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)

## LISA
lisa <- as.data.frame(localmoran(i$c1, dnnw, zero.policy = FALSE))

lisa$Cat <- rep("0", nrow(lisa))
# Scale the input data to mean
cDV <- i$c1 - mean(i$c1) 
# Get spatial lag values for each observation
lagDV <- lag.listw(dnnw, i$c1)

# Scale the lag values
clagDV <- lagDV - mean(lagDV, na.rm = TRUE)

# This simply adds a label based on the values
lisa$Cat[which(cDV > 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "HH" 
lisa$Cat[which(cDV < 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "LL"      
lisa$Cat[which(cDV < 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "LH"
lisa$Cat[which(cDV > 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "HL"

## Quick summary of LISA output
table(lisa$Cat)
lisa$rowid <- as.integer(rownames(lisa))
d4 <- bind_cols(d4,lisa)

## IHS 5 - LISA, Dist Neighbors ------
i <- as(d5,"Spatial")
coords <- coordinates(i)
rn <- i$rowid
k1 <- knn2nb(knearneigh(coords, k=1), row.names=rn)
dsts <- unlist(nbdists(k1, coords))
max_1nn <- max(dsts)
summary(dsts)

# Calculate distance based neighbors
dnn <- dnearneigh(coords, d1=0, d2=1*max_1nn, row.names=rn)

# Make weights
dnnw <- nb2listwdist(dnn, i, type="idw", style="W", 
                     zero.policy=TRUE)

## LISA
lisa <- as.data.frame(localmoran(i$c1, dnnw, zero.policy = FALSE))

lisa$Cat <- rep("0", nrow(lisa))
# Scale the input data to mean
cDV <- i$c1 - mean(i$c1) 
# Get spatial lag values for each observation
lagDV <- lag.listw(dnnw, i$c1)

# Scale the lag values
clagDV <- lagDV - mean(lagDV, na.rm = TRUE)

# This simply adds a label based on the values
lisa$Cat[which(cDV > 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "HH" 
lisa$Cat[which(cDV < 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "LL"      
lisa$Cat[which(cDV < 0 & clagDV > 0 & lisa[,5] < 0.05)] <- "LH"
lisa$Cat[which(cDV > 0 & clagDV < 0 & lisa[,5] < 0.05)] <- "HL"

## Quick summary of LISA output
table(lisa$Cat)
lisa$rowid <- as.integer(rownames(lisa))
d5 <- bind_cols(d5,lisa)

## LISA Map for random effects ----
d <- bind_rows(d2,d3,d4,d5) %>%
  mutate(wave_label = case_when(wave=="IHS2" ~ "2004-05\n(Moran's I = 0.131)",
                                wave=="IHS3" ~ "2010-11\n(Moran's I = 0.101)",
                                wave=="IHS4" ~ "2016-17\n(Moran's I = 0.094)",
                                wave=="IHS5" ~ "2019-20\n(Moran's I = 0.007)"),
         wave_label2 = case_when(wave=="IHS2" ~ "2004-05",
                                 wave=="IHS3" ~ "2010-11",
                                 wave=="IHS4" ~ "2016-17",
                                 wave=="IHS5" ~ "2019-20"))


ggplot() +
  geom_sf(data=mwi1,color = "black", size = 0.1,fill=NA) +
  geom_sf(data=d %>% arrange(Cat), aes(color = Cat)) +
  scale_color_manual(values= c("cornsilk4","red","hotpink","skyblue","dodgerblue4")) +
  facet_wrap(~wave_label2,nrow=1) +
  guides(color = guide_legend(title = "LISA")) +
  labs(title = "EA random effects: Charcoal as primary fuel",
       subtitle = "Local spatial autocorrelation")+
  theme_map() +
  theme(plot.background = element_rect(fill="gray98"),
        strip.text = element_text(size = 11),
        plot.title = element_text(size = 12,face="bold"),
        plot.subtitle = element_text(size=11),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10.5,face="bold"),
        legend.position = c(.0001,.0001),
        legend.background = element_blank(),
        legend.key = element_blank())


ggsave(filename="outputs/figure_a1.png",
       device="png",
       width=11.34,
       height=6.73,
       units="in",
       dpi=350)

