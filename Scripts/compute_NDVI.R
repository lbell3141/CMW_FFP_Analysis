library(terra)
library(tidyverse)
library(stringr)

#pathtoPlanetRaster1 <- "./data/comparison_skysatcollect_analytic_sr_udm2/SkySatCollect/20201122_183822_ss01_u0001_analytic_SR_clip_file_format.tif"  # Folder with all PSScene data, including rasters
#pathtoPlanetRaster2 <- "./data/comparison_skysatcollect_analytic_sr_udm2/SkySatCollect/20230930_170227_ssc12_u0001_analytic_SR_clip_file_format.tif"  # Folder with all PSScene data, including rasters

#pathtoNDVIraster1 <- "./data/hm_NDVI_Nov20.tif"
#pathtoNDVIraster2 <- "./data/hm_NDVI_Sept23.tif"

pathtoPlanetRasters <- "C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/CMW_FFP_Analysis/Data/Planet/AUG2018/Raw/PSScene"
pathtoNDVIRasters <- "./Data/Planet/AUG2018/NDVI"
#create raster stack for planetscope scene rasters from jan 2020 to jan 2021
#identify file pattern to pull only tif files from the Planet output folder
#allRastFiles <- list.files(pathtoPlanetRasters, pattern = "^\\d{8}_\\d{6}_\\d{4}_3B_AnalyticMS_SR_clip\\.tif$", full.names = TRUE)
#above code not working, using stringr instead
find_files <- function(folder) {
  #list all files in folder, set pattern, create object with files that match the pattern
  all_files <- list.files(folder, full.names = T)
  pattern <- "AnalyticMS_SR_clip_file_format"
  files_oi <- all_files[str_detect(all_files, pattern)]
  return(files_oi)
}

PSStifs <- find_files(pathtoPlanetRasters)

#use pattern to create a dataframe with tifs and tif dates
PSStifs <- tibble(filePath = PSStifs) %>%
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
  NDVI_filename <- file.path(pathtoNDVIRasters, paste0("NDVI_", PSStifs$date[i], ".tif"))
  
  writeRaster(NDVIrast, filename = NDVI_filename, overwrite = TRUE)
  
  #see confirmation of creation for each new tif
  total_iterations <- nrow(PSStifs)
  cat("Progress:", i, "/", total_iterations, "\n")
  
}

#calculate average value of the raster stack per pixel
#rasters aren't aligned...
align_raster <- function(to_be_aligned, ref_for_alignment) {
  resample(to_be_aligned, ref_for_alignment, method = "bilinear")
}

#make list of NDVI rasts to loop through with the first raster to be referenced for alignment
rasts <- list.files(path = "./Data/Planet/AUG2018/NDVI", pattern = "\\.tif$", full.names = TRUE)
ref_rast <- rast(rasts[1])
#output list
clean_rasts <- list()

#loop through raster list (rasts). read in as rast from file path, align, store in clean_rasts list 
for (i in rasts) {
  rast_oi <- rast(i)
  aligned_rast <- align_raster(rast_oi, ref_rast)
  clean_rasts <- append(clean_rasts, list(aligned_rast))
}

#stack processed and aligned rasters and calculate the mean per layer/pixel
stacked_rast <- rast(aligned_rasters)
avg_raster <- mean(stacked_rast)

#save rast for use in ffp calc
writeRaster(mean_raster, filename = "./Data/Planet/AUG2018/avg_NDVI_aug2018.tif", overwrite = TRUE)


#=====for single raster=========================================================
skysat_rast <- rast(pathtoPlanetRaster1)
NDVIrast <- (skysat_rast[[4]] - skysat_rast[[3]]) / (skysat_rast[[4]] + skysat_rast[[3]])
writeRaster(NDVIrast, pathtoNDVIraster1, overwrite = TRUE)

skysat_rast <- rast(pathtoPlanetRaster2)
NDVIrast <- (skysat_rast[[4]] - skysat_rast[[3]]) / (skysat_rast[[4]] + skysat_rast[[3]])
writeRaster(NDVIrast, pathtoNDVIraster2, overwrite = TRUE)
