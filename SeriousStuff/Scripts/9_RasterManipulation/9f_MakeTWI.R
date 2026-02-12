#create rasters of topographic wetness index from USGS-3DEP DEM rasters

library(terra)
library(dplyr)
library(stringr)
library(whitebox)

dem_dir <- "./SeriousStuff/Data/USGS_3DEP_DEM"
out_dir <- "./SeriousStuff/Data/3DEP-derived_TWI"

dir.create(out_dir, showWarnings = FALSE)

dem_files <- list.files(dem_dir, pattern = "\\.tif$", full.names = TRUE)

dem_index <- tibble(
  file = dem_files,
  site = str_sub(basename(dem_files), 1, 3)
)

sites <- unique(dem_index$site)

for (s in sites) {
  
  message("Processing site: ", s)
  
  site_files <- dem_index %>%
    filter(site == s) %>%
    pull(file)
  
#mosaic site tiles together
  dem_list <- lapply(site_files, rast)
  
  dem <- if (length(dem_list) > 1) {
    do.call(mosaic, dem_list)
  } else {
    dem_list[[1]]
  }
  
#force projection
  if (is.lonlat(dem)) {
    utm_epsg <- paste0(
      "EPSG:",
      terra::crs(dem, describe = TRUE)$epsg
    )
    dem <- project(dem, utm_epsg)
  }
  
#save intermediate and final rasts
  dem_filled  <- file.path(out_dir, paste0(s, "_dem_filled.tif"))
  flow_acc    <- file.path(out_dir, paste0(s, "_flow_acc.tif"))
  twi_file    <- file.path(out_dir, paste0(s, "_TWI.tif"))
  
  writeRaster(dem, dem_filled, overwrite = TRUE)
  
  # fill depressions on dem
  wbt_fill_depressions(
    dem = dem_filled,
    output = dem_filled
  )
  
  # calc 8 direction flow accumulation
  wbt_d8_flow_accumulation(
    input = dem_filled,
    output = flow_acc,
    out_type = "sca"
  )
  
  flow_acc <- rast(flow_acc)
  
  # calc slope (in radians)
  slope <- terrain(dem, v = "slope", unit = "radians")
  slope[slope < 0.001] <- 0.001
  
  # clean flow rast
  flow_acc[flow_acc <= 0] <- NA
  
  # calc twi
  twi <- log(flow_acc / tan(slope))
  names(twi) <- "TWI"
  
  writeRaster(twi, twi_file, overwrite = TRUE)
}

twi_test <- rast("3DEP-derived_TWI/SRG_TWI.tif")
plot(twi_test)
summary(values(twi_test))
