# Replication package for: 
  # Beach et al., 2026 - "Do coupled population, economic, and land use dynamics explain household energy transitions in Malawi?" [World Development]

# 1a - Data preparation script: Forest loss measures
  ## Forest cover and loss data: GFCD v1.9
  ## https://glad.earthengine.app/view/global-forest-change

# Step 1 - stitch and reproject rasters from GFCD v1.9 -----------------------------------------------------------------
  # Summary:
    # Rasters should be one contiguous data file
    # Use raster calculations to create binary surface of forest cover lost in each set of IHS years

    
library(sf)
library(terra)

# (1a) Load data 
loss1 <- rast("data/Hansen_GFC-2021-v1.9_lossyear_00N_030E.tif")
loss2 <- rast("data/Hansen_GFC-2021-v1.9_lossyear_10S_030E.tif")
mask1 <- rast("data/Hansen_GFC-2021-v1.9_datamask_00N_030E.tif")
mask2 <- rast("data/Hansen_GFC-2021-v1.9_datamask_10S_030E.tif")
first1 <- rast("data/Hansen_GFC-2021-v1.9_treecover2000_00N_030E.tif")
first2 <- rast("data/Hansen_GFC-2021-v1.9_treecover2000_10S_030E.tif")
mwi <- st_read("data/gadm40_MWI_0.shp") %>% st_transform(.,st_crs(loss1))

# (1b) Create 50 km (ish) buffer of Malawi and rectangular bounding box
buff <- st_buffer(mwi,50000)

# (1c) Stitch the Hansen rasters together and clip to buffer extent
loss <- terra::merge(loss1,loss2) %>% crop(.,buff)
mask <- terra::merge(mask1,mask2) %>% crop(.,buff)
first <- terra::merge(first1,first2) %>% crop(.,buff)


# Step 2 - Create baseline tree cover raster [0/1] -------------------------------
m <- c(0, 30, 0,
       31, 100, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
tc00 <- classify(first, rclmat, right=NA)


# Step 3 - Reclassify loss years to IHS wave years: 4 rasters -------------
## (3a) Reclassify loss years to binary wave rasters
  ## 1=loss in that year. 0 means no loss in that year

# IHS2
m <- c(0,0,0,
       1,5,1,
       6,21,0)
rclmat <- matrix(m,ncol=3,byrow=T)
l2 <- classify(loss,rclmat,right=NA)

# IHS3
m <- c(0,5,0,
       6,11,1,
       12,21,0)
rclmat <- matrix(m,ncol=3,byrow=T)
l3 <- classify(loss,rclmat,right=NA)

# IHS4
m <- c(0,11,0,
       12,17,1,
       18,21,0)
rclmat <- matrix(m,ncol=3,byrow=T)
l4 <- classify(loss,rclmat,right=NA)

# IHS5
m <- c(0,17,0,
       18,20,1,
       21,21,0)
rclmat <- matrix(m,ncol=3,byrow=T)
l5 <- classify(loss,rclmat,right=NA)


## (3b) Create binary FC loss rasters
  ## 1= FC loss in that year. 0 means no loss in that year/or loss is not relevant to FC loss

fcl2 <- l2*tc00
fcl3 <- l3*tc00
fcl4 <- l4*tc00
fcl5 <- l5*tc00


# Step 4 - Write rasters -------------
writeRaster(tc00, "processed/fc_baseline2000.tif", overwrite=TRUE)
writeRaster(fcl2, "processed/fc_loss_ihs2.tif", overwrite=TRUE)
writeRaster(fcl3, "processed/fc_loss_ihs3.tif", overwrite=TRUE)
writeRaster(fcl4, "processed/fc_loss_ihs4.tif", overwrite=TRUE)
writeRaster(fcl5, "processed/fc_loss_ihs5.tif", overwrite=TRUE)







