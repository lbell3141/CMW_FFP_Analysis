library(terra)
library(LAIr)

naip_dir <- "./SeriousStuff/Data/NAIP_imagery"

lai_dir <- "./SeriousStuff/Data/NAIP-derived_LAI"

seasons <- c("premonsoon", "monsoon", "postmonsoon", "winter")

parse_naip_name <- function(fname) {
  tibble(
    site = str_extract(fname, "(?<=_)[A-Z]{3}(?=_)"),
    date = ymd(str_extract(fname, "\\d{8}"))
  )
}

process_naip_to_lai <- function(infile, outfile) {
  
  r <- rast(infile)
  
  # Ensure correct band order
  names(r) <- c("blue", "green", "red", "nir")
  
  # NDVI
  ndvi <- (r$nir - r$red) / (r$nir + r$red)
  
  # Convert NDVI to LAI
  lai <- LAIr::ndvi2lai(
    ndvi,
    method = "simple"   # robust + common
  )
  
  writeRaster(
    lai,
    outfile,
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW"))
  )
}




for (season in seasons) {
  
  message("Processing season: ", season)
  
  in_season_dir  <- file.path(naip_dir, season)
  out_season_dir <- file.path(lai_dir, season)
  
  dir.create(out_season_dir, showWarnings = FALSE)
  
  naip_files <- list.files(
    in_season_dir,
    pattern = "\\.tif$",
    full.names = TRUE
  )library(terra)
  library(LAIr)
  library(tibble)
  library(stringr)
  library(lubridate)
  
  naip_dir <- "./SeriousStuff/Data/NAIP_imagery"
  lai_dir  <- "./SeriousStuff/Data/NAIP-derived_LAI"
  seasons <- c("premonsoon", "monsoon", "postmonsoon", "winter")
  
  # Parse file name to get site and date
  parse_naip_name <- function(fname) {
    tibble(
      site = str_extract(fname, "(?<=_)[A-Z]{3}(?=_)"),
      date = ymd(str_extract(fname, "\\d{8}"))
    )
  }
  
  # Process a single NAIP file to LAI using Hong et al. 2004
  process_naip_to_lai <- function(infile, outfile) {
    
    r <- rast(infile)
    
    # Ensure correct band order
    names(r) <- c("blue", "green", "red", "nir")
    
    # NDVI
    ndvi <- (r$nir - r$red) / (r$nir + r$red)
    
    # Convert NDVI to LAI (all equations)
    lai_all <- LAIr::NDVI2LAI(ndvi, category = "Crop", biome = 8)
    
    # Select the Hong et al. 2004 layer (second layer)
    lai_hong <- lai_all[[2]]
    names(lai_hong) <- "LAI_Hong2004"
    
    # Write raster
    writeRaster(
      lai_hong,
      outfile,
      overwrite = TRUE,
      wopt = list(gdal = c("COMPRESS=LZW"))
    )
  }
  
  # Loop over seasons
  for (season in seasons) {
    
    message("Processing season: ", season)
    
    in_season_dir  <- file.path(naip_dir, season)
    out_season_dir <- file.path(lai_dir, season)
    dir.create(out_season_dir, showWarnings = FALSE, recursive = TRUE)
    
    naip_files <- list.files(
      in_season_dir,
      pattern = "\\.tif$",
      full.names = TRUE
    )
    
    for (f in naip_files) {
      
      meta <- parse_naip_name(basename(f))
      
      out_name <- paste0(
        "LAI_",
        meta$site, "_",
        format(meta$date, "%Y%m%d"),
        ".tif"
      )
      
      out_file <- file.path(out_season_dir, out_name)
      
      if (!file.exists(out_file)) {
        message("  → ", out_name)
        process_naip_to_lai(f, out_file)
      }
    }
  }
  
  
  for (f in naip_files) {
    
    meta <- parse_naip_name(basename(f))
    
    out_name <- paste0(
      "LAI_",
      meta$site, "_",
      format(meta$date, "%Y%m%d"),
      ".tif"
    )
    
    out_file <- file.path(out_season_dir, out_name)
    
    if (!file.exists(out_file)) {
      message("  → ", out_name)
      process_naip_to_lai(f, out_file)
    }
  }
}




#========================================================write out NDVI
# library(terra)
# library(tidyverse)
# library(lubridate)
# 
# naip_dir <- "./SeriousStuff/Data/NAIP_imagery/RawImagery"
# ndvi_dir <- "./SeriousStuff/Data/NAIP_imagery/NAIP-derived_NDVI"
# 
# seasons <- c("premonsoon", "monsoon", "postmonsoon", "winter")
# 
# # walk(
# #   file.path(ndvi_dir, seasons),
# #   dir.create,
# #   recursive = TRUE,
# #   showWarnings = FALSE
# # )
# 
# parse_naip_name <- function(fname) {
#   tibble(
#     file = fname,
#     site = str_extract(basename(fname), "(?<=_)[A-Z]{3}(?=_)"),
#     date = ymd(str_extract(basename(fname), "\\d{8}"))
#   )
# }
# 
# naip_to_ndvi <- function(infile) {
#   r <- rast(infile)
#   names(r) <- c("blue", "green", "red", "nir")
#   (r$nir - r$red) / (r$nir + r$red)
# }
# 
# for (season in seasons) {
# 
#   message("Processing ", season, "...")
# 
#   season_in_dir  <- file.path(naip_dir, season)
#   season_out_dir <- file.path(ndvi_dir, season)
# 
#   naip_files <- list.files(
#     season_in_dir,
#     pattern = "\\.tif$",
#     full.names = TRUE,
#     recursive = TRUE
#   )
# 
#   if (length(naip_files) == 0) next
# 
#   naip_index <- map_dfr(naip_files, parse_naip_name)
# 
#   for (i in seq_len(nrow(naip_index))) {
# 
#     row <- naip_index[i, ]
# 
#     out_name <- paste0(
#       row$site, "_",
#       season, "_",
#       format(row$date, "%Y%m%d"),
#       "_NDVI.tif"
#     )
# 
#     out_file <- file.path(season_out_dir, out_name)
# 
#     ndvi <- naip_to_ndvi(row$file)
# 
#     writeRaster(
#       ndvi,
#       out_file,
#       overwrite = TRUE,
#       wopt = list(gdal = c("COMPRESS=LZW"))
#     )
#   }
# }
# 
