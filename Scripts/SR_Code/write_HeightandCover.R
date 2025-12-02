library(lidR)
library(terra)

lidar_input_path <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"

las <- readLAS(lidar_input_path)
las <- classify_ground(las, csf())
dtm <- rasterize_terrain(las, algorithm = tin(), res = 1)
las <- normalize_height(las, dtm)
# 
# crop_radius <- 200
# crop_area_m2 <- (2 * crop_radius)^2
# crop_area_ha <- crop_area_m2 / 10000
# 
# center_x <- mean(las@data$X)
# center_y <- mean(las@data$Y)
# las_filt <- filter_poi(las, {
#   Z >= 0 & Z <= 8 &
#     X >= (center_x - crop_radius) & X <= (center_x + crop_radius) &
#     Y >= (center_y - crop_radius) & Y <= (center_y + crop_radius)
# })
las_filt <- filter_poi(las, {
  Z >= 0 & Z <= 8 })

chm <- pixel_metrics(las = las_filt, func = ~max(Z), res = 1)
#chm <- rasterize_canopy(las = las, res = 2, algorithm = p2r())

writeRaster(chm, "./Data/RussHomework/SRG_RS/CHM.tif")

chm <- rast("./Data/RussHomework/SRG_RS/CHM.tif")
#cover rast
chm_bin <- chm >= 1
chm_bin <- as.numeric(chm_bin)
#writeRaster(chm_bin, "./Data/RussHomework/SRG_RS/Cover.tif")

#===============================================================================
lidar_input_path <- "Z:/Drone_Wim/SRERMesq5-27-2025/SRERMesq5-27-25/lidars/terra_las/cloud_merged.las"

las <- readLAS(lidar_input_path)
las <- classify_ground(las, csf())
dtm <- rasterize_terrain(las, algorithm = tin(), res = 1)
las <- normalize_height(las, dtm)
# 
# crop_radius <- 200
# crop_area_m2 <- (2 * crop_radius)^2
# crop_area_ha <- crop_area_m2 / 10000
# 
# center_x <- mean(las@data$X)
# center_y <- mean(las@data$Y)
# las_filt <- filter_poi(las, {
#   Z >= 0 & Z <= 8 &
#     X >= (center_x - crop_radius) & X <= (center_x + crop_radius) &
#     Y >= (center_y - crop_radius) & Y <= (center_y + crop_radius)
# })
las_filt <- filter_poi(las, {
  Z >= 0 & Z <= 11 })

chm <- pixel_metrics(las = las_filt, func = ~max(Z), res = 1)
#chm <- rasterize_canopy(las = las, res = 2, algorithm = p2r())

#writeRaster(chm, "./Data/RussHomework/SRM_RS/CHM.tif")


#cover rast
chm_bin <- chm >= 1
chm_bin <- as.numeric(chm_bin)
#writeRaster(chm_bin, "./Data/RussHomework/SRM_RS/Cover.tif")
