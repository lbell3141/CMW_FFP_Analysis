#calculate LAI from NDVI
#doi.org/10.1016/j.softx.2024.101776

#install.packages(LAIr)
#library(LAIr)
library(terra)

PathToNDVIrast <- "./Data/Planet/AUG2018/reprodNDVIrast.tif"
PathToNDVIrast <- "./Data/Planet/JUN2024/avg_NDVI_JUN2024.tif"
ndvi <- rast(PathToNDVIrast)

#lai <- NDVI2LAI(ndvi)


LAI = 0.57*exp(2.33*ndvi)
plot(LAI)
#===============================================================================
#===============================================================================
#===============================================================================
#estimation of LAI using Beer-Lambert derived model
#doi.org/10.1016/j.agrformet.2009.02.007
# L = -(cos(scan_angle)/k)*ln(R_ground / R_total)
# k can be assumed 0.5 for canopies with spherical distribution

library(lidR)
library(terra)

PathToPointCloud <- "./Data/LiDAR/points.laz"
PathToLAIrast <- "./Data/LiDAR/LAIlazr.tif"
las <- readLAS(PathToPointCloud)

#count number of ground and total points 
count_pts <- function(pointcloud, Classification) {
  list(
    ground_total <- sum(Classification == 2),
    point_total <- sum(Classification %in% c(1,2))
  )
}

count_rast <- grid_metrics(las, count_pts(pointcloud, Classification), res = 1)
ratio_rast <- log(count_rast[[1]] / count_rast[[2]])

#if mean scan angle is <10deg, ignore
#absolute value bc angles from -90 to 90
abs_scan_angle <- abs(las$ScanAngle)
avg_angle <- mean(abs_scan_angle, na.rm = T)
#convert to radians for cos()
rad_avg_angle <- (pi*avg_angle) / 180

lai_rast <- ((-1)*((cos(rad_avg_angle)) / 0.5) * ratio_rast)
#writeRaster(lai_rast, PathToLAIrast)

lairast <- rast(PathToLAIrast)
plot(lairast)
lairast3m <- aggregate(lairast, fact = 3, na.rm = T)
plot(lairast3m)
writeRaster(lairast3m, PathToLAIrast)
#===============================================================================
#===============================================================================
#===============================================================================
#doi.org/10.1016/j.foreco.2018.11.017
#canopyLazR

#install.packages("devtools")
#library(devtools)
#install_github("akamoske/canopyLazR")
library(canopyLazR)

#convert point cloud to voxels
laz.data <- laz.to.array(laz.file.path = PathToPointCloud,
                         voxel.resolution = 3,
                         z.resolution = 1,
                         use.classified.returns = T)
#make voxels mimic canopy height model
level.canopy <- canopy.height.levelr(lidar.array = laz.data)

#estimate LAD (Leaf Area Density: the total leaf area per unit of volume) per voxel
lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy,
                             voxel.height = 1,
                             beer.lambert.constant = NULL)
#convert LAD to raster stack
lad.raster <- lad.array.to.raster.stack(lad.array = lad.estimates,
                                        laz.array = laz.data,
                                        epsg.code = 32612)
#convert LAD to LAI
lai.raster <- raster::calc(lad.raster, fun = sum, na.rm = T)

#convert laz arrays to ground and canopy height rasters
grd.can.rasters <- array.to.ground.and.canopy.rasters(laz.data, 32612)
#calculate max LAD and height of max LAD
max.lad <- lad.ht.max(lad.array = lad.estimates,
                      laz.array = laz.data, 
                      ht.cut = 5,
                      epsg.code = 32612)

#calc volume of filled and empty voxels in a given column
empty.filled.volume <- canopy.porosity.filled.volume(lad.array = lad.estimates,
                                                     laz.array = laz.data, 
                                                     ht.cut = 5,
                                                     xy.res = 3,
                                                     z.res = 1,
                                                     epsg.code = 32612)
within.can.rugosity <- rugosity.within.canopy(lad.array = lad.estimates,
                                              laz.array = laz.data,
                                              ht.cut = 5,
                                              epsg.code = 32612)
ht.quantiles <- lad.quantiles(lad.array = lad.estimates,
                              laz.array = laz.data,
                              ht.cut = 5,
                              epsg.code = 32612)
can.volume <- canopy.volume(lad.array = lad.estimates,
                            laz.array = laz.data,
                            ht.cut = 5,
                            xy.res = 3,
                            z.res = 1,
                            epsg.code = 32612)
euphotic.depth <- can.volume$euphotic.volume.column.raster / (10*10*1)
toc.rugos <- toc.rugosity(chm.raster = grd.can.rasters$chm.raster,
                          xy.res = 3,
                          z.res = 1)
plot(lai.raster)
writeRaster(lai.raster, PathToLAIrast)
