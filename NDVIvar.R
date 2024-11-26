#Calculate NDVI from a list of rasters
library(stringr)
library(terra)

PathToRawRasts <- "./Data_NDVIvar/RAW"

find_files <- function(folder) {
  #list all files in folder, set pattern, create object with files that match the pattern
  all_files <- list.files(folder, full.names = T)
  pattern <- ".tif"
  files_oi <- all_files[str_detect(all_files, pattern)]
  return(files_oi)
}
PSStifs <- find_files(PathToRawRasts)
NDVI_list <- list()
for (i in seq_along(PSStifs)){
  #reading in raster
  PSSrast <- rast(PSStifs[i])
  #calculating NDVI
  NDVI_list[[i]] <- (PSSrast[[4]] - PSSrast[[3]]) / (PSSrast[[4]] + PSSrast[[3]])
}

par(mfrow = c(2, 3)) 
for (i in seq_along(NDVI_list)) {
  #remove .tif from plot names
  file_name <- tools::file_path_sans_ext(basename(PSStifs[i]))
  plot(NDVI_list[[i]], main = paste(file_name, "NDVI"), col = viridis::viridis(100))
  }

#calculate variance of each raster
ndvi_var <- sapply(NDVI_list, function(x) {
  #extract rast values for calc
  values_x <- values(x)
  var(values_x, na.rm = T)
  })
#names(ndvi_var) <- tools::file_path_sans_ext(basename(PSStifs))
print(ndvi_var)
