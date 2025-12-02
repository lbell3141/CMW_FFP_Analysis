lai_from_lidar <- function(lidar_input_path, lai_output_path) {
  library(lidR)
  library(terra)
  
  las <- readLAS(lidar_input_path)
  las <- classify_ground(las, csf())
  
  # Use formula interface directly, no separate function
  count_rast <- grid_metrics(
    las,
    ~list(
      ground_total = sum(Classification == 2, na.rm = TRUE),
      point_total  = length(Classification)
    ),
    res = 1
  )
  
  eps <- 1e-6
  ground_rast <- count_rast$ground_total + eps
  total_rast  <- count_rast$point_total + eps
  
  ratio_rast <- log(ground_rast / total_rast)
  
  # Scan angle handling
  if (is.null(las$ScanAngle) || all(is.na(las$ScanAngle))) {
    rad_avg_angle <- (pi * 1) / 180
  } else {
    avg_angle <- mean(abs(las$ScanAngle), na.rm = TRUE)
    rad_avg_angle <- (pi * avg_angle) / 180
  }
  
  # Beer-Lambert LAI model
  k <- 0.5
  lai_rast <- - (cos(rad_avg_angle) / k) * ratio_rast
  lai_rast <- rast(lai_rast)
  
  # Remove > 2 SD outliers
  lai_mean <- global(lai_rast, mean, na.rm = TRUE)[[1]]
  lai_sd   <- global(lai_rast, sd, na.rm = TRUE)[[1]]
  lai_cutoff <- lai_mean + 2 * lai_sd
  lai_rast_clean <- mask(lai_rast, lai_rast <= lai_cutoff, maskvalue = FALSE)
  
  writeRaster(lai_rast_clean, lai_output_path, overwrite = TRUE)
  plot(lai_rast_clean, main = "1m LAI")
  
  return(lai_rast_clean)
}
