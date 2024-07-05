library(terra)
library(tidyverse)

#pathtoPlanetRaster1 <- "./data/comparison_skysatcollect_analytic_sr_udm2/SkySatCollect/20201122_183822_ss01_u0001_analytic_SR_clip_file_format.tif"  # Folder with all PSScene data, including rasters
#pathtoPlanetRaster2 <- "./data/comparison_skysatcollect_analytic_sr_udm2/SkySatCollect/20230930_170227_ssc12_u0001_analytic_SR_clip_file_format.tif"  # Folder with all PSScene data, including rasters

#pathtoNDVIraster1 <- "./data/hm_NDVI_Nov20.tif"
#pathtoNDVIraster2 <- "./data/hm_NDVI_Sept23.tif"

pathtoPlanetRasters <- "./Data/Planet/Aug2018/Raw"

#create raster stack for planetscope scene rasters from jan 2020 to jan 2021
#identify file pattern to pull only tif files from the Planet output folder
allRastFiles <- list.files(pathtoPlanetRasters, pattern = "^\\d{8}_\\d{6}_\\d{2}_\\w+_3B_AnalyticMS_SR_clip\\.tif$", full.names = TRUE)
#use pattern to create a dataframe with tifs and tif dates
PSStifs <- tibble(filePath = allRastFiles) %>%
  mutate(fileName = basename(filePath),
         year = str_extract(fileName, "\\d{4}"),
         month = str_sub(fileName, start = 5, end = 6),
         day = str_sub(fileName, start = 7, end = 8),
         date = make_date(year, month, day)) %>%
  arrange(date)

#Process rasters to obtain NDVI and save data to a new folder
for (i in 1:nrow(PSStifs)){
  #reading in raster
  PSSrast <- rast(PSStifs$filePath[i])
  #calculating NDVI
  NDVIrast <- (PSSrast[[4]] - PSSrast[[3]]) / (PSSrast[[4]] + PSSrast[[3]])
  #save resulting raster of NDVI to specified folder
  #specify file name pattern for raster outputs
  NDVI_filename <- file.path(pathtoNDVIrasters, paste0("NDVI_", PSStifs$date[i], ".tif"))
  
  writeRaster(NDVIrast, filename = NDVI_filename, overwrite = TRUE)
  
  #see confirmation of creation for each new tif
  total_iterations <- nrow(PSStifs)
  cat("Progress:", i, "/", total_iterations, "\n")
  
}

#calculate average value of the raster stack per pixel
rasts <- list.files(path = "./Data/Planet/Aug2018/NDVI", pattern = "\\.tif$", full.names = TRUE)
stacked <- stack(rasts)
avg_stack <- calc(stacked, fun = mean)
writeRaster(avg_stack, "./Data/Planet/Aug2018/average_raster.tif", format = "GTiff")

#=====for single raster=========================================================
skysat_rast <- rast(pathtoPlanetRaster1)
NDVIrast <- (skysat_rast[[4]] - skysat_rast[[3]]) / (skysat_rast[[4]] + skysat_rast[[3]])
writeRaster(NDVIrast, pathtoNDVIraster1, overwrite = TRUE)

skysat_rast <- rast(pathtoPlanetRaster2)
NDVIrast <- (skysat_rast[[4]] - skysat_rast[[3]]) / (skysat_rast[[4]] + skysat_rast[[3]])
writeRaster(NDVIrast, pathtoNDVIraster2, overwrite = TRUE)
