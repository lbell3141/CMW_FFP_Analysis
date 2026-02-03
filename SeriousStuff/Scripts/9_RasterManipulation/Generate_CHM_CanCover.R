# use lidar collections to create site chms, and detect individual trees and tree heights
#srg, srm, wkg lidar stored on gaea server; cmw lidar from USGS and stored locally

library(lidR)
library(terra)

# kernel <- matrix(1, 3, 3) #for canopy detection raster prep
# 
# custom_ws <- function(x) {
#   a <- 0.6899878
#   b <- 0.1115501
#   floor_thresh <- 0.9784894
#   floor_val <- 4.285451
#   
#   y <- a * x + b
#   
#   y[x <= floor_thresh] <- floor_val
#   
#   y[is.na(y)] <- floor_val
#   y[y <= 0] <- floor_val
#   
#   return(y)
# }

#CMW============================================================================
cmwlidar_input_path <- "./Data/LiDAR/points.laz"

cmwlas <- readLAS(cmwlidar_input_path)

cmwlas <- classify_ground(cmwlas, csf())
cmwdtm <- rasterize_terrain(cmwlas, algorithm = tin(), res = 1)
cmwlas <- normalize_height(cmwlas, cmwdtm)

cmwlas_filt <- filter_poi(cmwlas, {
  Z >= 0 & Z <= 15 })

#writeLAS(cmwlas_filt, "./SeriousStuff/Data/LiDAR-derived/CMW_normfiltpts.las")

cmwchm <- pixel_metrics(las = cmwlas_filt, func = ~max(Z), res = 1)

#writeRaster(cmwchm, "./SeriousStuff/Data/LiDAR-derived/CMW_chm.tif")

#cover rast
cmwchm_bin <- cmwchm >= 1
cmwchm_bin <- as.numeric(cmwchm_bin)
#writeRaster(cmwchm_bin, "./SeriousStuff/Data/LiDAR-derived/CMW_CanopyCover.tif")

#tree detection
# chm_smoothed <- terra::focal(cmwchm, w = kernel, fun = median, na.rm = TRUE)
# ttops <- locate_trees(las = chm_smoothed, algorithm = lmf(ws = custom_ws, hmin = 1))


#SRG============================================================================
srglidar_input_path <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"

srglas <- readLAS(srglidar_input_path)

srglas <- classify_ground(srglas, csf())
srgdtm <- rasterize_terrain(srglas, algorithm = tin(), res = 1)
srglas <- normalize_height(srglas, srgdtm)

srglas_filt <- filter_poi(srglas, {
  Z >= 0 & Z <= 8 })

#writeLAS(srglas_filt, "./SeriousStuff/Data/LiDAR-derived/SRG_normfiltpts.las")

srgchm <- pixel_metrics(srglas = srglas_filt, func = ~max(Z), res = 1)

#writeRaster(chm, "./SeriousStuff/Data/LiDAR-derived/SRG_chm.tif")

#cover rast
srgchm_bin <- srgchm >= 1
srgchm_bin <- as.numeric(srgchm_bin)
#writeRaster(srgchm_bin, "./SeriousStuff/Data/LiDAR-derived/SRG_CanopyCover.tif")

#SRM============================================================================
srmlidar_input_path <- "Z:/Drone_Wim/SRERMesq5-27-25/lidars/terra_las/cloud_merged.las"

srmlas <- readLAS(srmlidar_input_path)

srmlas <- classify_ground(srmlas, csf())
srmdtm <- rasterize_terrain(srmlas, algorithm = tin(), res = 1)
srmlas <- normalize_height(srmlas, srmdtm)

srmlas_filt <- filter_poi(srmlas, {
  Z >= 0 & Z <= 11 })

#writeLAS(srmlas_filt, "./SeriousStuff/Data/LiDAR-derived/SRM_normfiltpts.las")

srmchm <- pixel_metrics(las = srmlas_filt, func = ~max(Z), res = 1)

#writeRaster(srmchm, "./SeriousStuff/Data/LiDAR-derived/SRM_chm.tif")

#cover rast
srmchm_bin <- srmchm >= 1
srmchm_bin <- as.numeric(srmchm_bin)
#writeRaster(srmchm_bin, "./SeriousStuff/Data/LiDAR-derived/SRM_CanopyCover.tif")

#Wkg============================================================================

wkglidar_input_path <- "Z:/Drone_Wim/WGprocessed1-16-26/WGgrass1-16-25Lidar/lidars/terra_las/cloud_merged.las"

wkglas <- readLAS(wkglidar_input_path)

wkglas <- classify_ground(wkglas, csf())
wkgdtm <- rasterize_terrain(wkglas, algorithm = tin(), res = 1)
wkglas <- normalize_height(wkglas, wkgdtm)

wkglas_filt <- filter_poi(wkglas, {
  Z >= 0 & Z <= 11 })

writeLAS(wkglas_filt, "./SeriousStuff/Data/LiDAR-derived/WKG_normfiltpts.las")

wkgchm <- pixel_metrics(las = wkglas_filt, func = ~max(Z), res = 1)

#writeRaster(wkgchm, "./SeriousStuff/Data/LiDAR-derived/WKG_chm.tif")

#cover rast
wkgchm_bin <- wkgchm >= 1
wkgchm_bin <- as.numeric(wkgchm_bin)
#writeRaster(wkgchm_bin, "./SeriousStuff/Data/LiDAR-derived/WKG_CanopyCover.tif")
