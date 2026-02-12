library(terra)
library(tidyverse)
library(lubridate)
library(LAIr)

naip_dir <- "./SeriousStuff/Data/NAIP_imagery/1000m/RawImagery"
ndvi_dir <- "./SeriousStuff/Data/NAIP_imagery/1000m/NAIP-derived_NDVI"
lai_dir  <- "./SeriousStuff/Data/NAIP_imagery/1000m/NAIP-derived_LAI"

seasons <- c("premonsoon", "monsoon", "postmonsoon", "winter")
# 
# # Create output folders if not alr made
# walk(file.path(ndvi_dir, seasons), dir.create, recursive = TRUE, showWarnings = FALSE)
#walk(file.path(lai_dir,  seasons), dir.create, recursive = TRUE, showWarnings = FALSE)

parse_naip_name <- function(fname) {
  
  site <- str_extract(fname, "(?i)(?<=^)[A-Z]{3}(?=_)")
  date <- str_extract(fname, "\\d{8}")
  
  tibble(
    site = toupper(site),
    date = ymd(date)
  )
}



naip_to_ndvi <- function(infile) {
  r <- rast(infile)
  names(r) <- c("blue", "green", "red", "nir")
  (r$nir - r$red) / (r$nir + r$red)
}

for (season in seasons) {
  
  message("NDVI → ", season)
  
  in_dir  <- file.path(naip_dir, season)
  out_dir <- file.path(ndvi_dir, season)
  
  naip_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(naip_files) == 0) next
  
  for (f in naip_files) {
    
    meta <- parse_naip_name(basename(f))
    
    out_name <- paste0(
      meta$site, "_",
      format(meta$date, "%Y%m%d"),
      "_NDVI.tif"
    )
    
    out_file <- file.path(out_dir, out_name)
    if (file.exists(out_file)) next
    
    ndvi <- naip_to_ndvi(f)
    
    writeRaster(
      ndvi,
      out_file,
      overwrite = TRUE,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
  }
}


ndvi_to_lai <- function(ndvi_rast) {
  
  lai_all <- LAIr::NDVI2LAI(
    ndvi_rast,
    category = "Crop",
    biome   = 8
  )
  
  lai_hong <- lai_all[[2]]
  names(lai_hong) <- "LAI_Hong2004"
  lai_hong
}

for (season in seasons) {
  
  message("LAI → ", season)
  
  in_dir  <- file.path(ndvi_dir, season)
  out_dir <- file.path(lai_dir, season)
  
  ndvi_files <- list.files(in_dir, pattern = "_NDVI\\.tif$", full.names = TRUE)
  if (length(ndvi_files) == 0) next
  
  for (f in ndvi_files) {
    
    meta <- parse_naip_name(basename(f))
    
    if (is.na(meta$site) || is.na(meta$date)) {
      warning("Skipping unparseable file: ", basename(f))
      next
    }
    
    out_name <- paste0(
      "LAI_",
      meta$site, "_",
      format(meta$date, "%Y%m%d"),
      ".tif"
    )
    
    out_file <- file.path(out_dir, out_name)
    if (file.exists(out_file)) next
    
    ndvi <- rast(f)
    lai  <- ndvi_to_lai(ndvi)
    
    writeRaster(
      lai,
      out_file,
      overwrite = TRUE,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
  }
}
