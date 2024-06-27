#plot climatology ouptuts 
library(fields)
library(terra)

PathToOutput <- "./Plots/footprints/CMW_WD_by_10deg"

#===============================================================================
#================================Save ffp plots=================================
#===============================================================================

for (j in seq_along(ffp_list)) {
  #naming convention for plots
  plot_file <- sprintf("%s/plot_%03d.png", PathToOutput, j)  
  #image dimensions
  png(plot_file, width = 1000, height = 1000) 
  
  #Kljun plotting code
  image.plot(ffp_list[[j]]$x_2d[1,], ffp_list[[j]]$y_2d[,1], ffp_list[[j]]$fclim_2d) 
  for (i in seq_along(ffp_list[[j]]$xr)) {
    lines(ffp_list[[j]]$xr[[i]], ffp_list[[j]]$yr[[i]], type="l", col="red")
  }
  dev.off()
}

#===============================================================================
#===========================save ffp plots with RAP=============================
#===============================================================================
#on top of RAP data
#code adapted from Contours_to_Mask.R function, ffp_contours_to_mask
rast_file <- rast("./Data/RAP/avg_rast.tif")
woody <- rast_file[[2]]

ffp_list <- readRDS("./Data/calcd_ffp_list.rds")
x_list <- list()
y_list <- list()
for (i in seq_along(ffp_list)) {
  x_list[[i]] <- ffp_list[[i]]$xr
  y_list[[i]] <- ffp_list[[i]]$yr
}

ffp_contours_to_mask <- function(xr_list, yr_list, rast) {
  #lists needed to store: 1) standard plane coord pairs from ffp output, 2) coords converted to lat/long, and 3) points converted to lines
  coord_pair_list <- list()
  latlon_list <- list()
  lines_list <- list()
  
  #format raw points as numeric matrix with NAs removed to avoid errors
  for (i in 1:length(xr_list)) {
    df <- data.frame(x = as.numeric(xr_list[[i]]), y = as.numeric(yr_list[[i]]))
    df <- na.omit(df)
    coord_pair_list[[i]] <- as.matrix(df)
  }
  
  #convert standard plane coords to lat/lon coords (see notes above)
  for (i in seq_along(coord_pair_list)) {
    lat <- (coord_pair_list[[i]][, 2] / 111111) + 31.6637
    lon <- (coord_pair_list[[i]][, 1] / (111111 * cos(31.6637 * pi / 180))) - 110.1777
    latlon_list[[i]] <- cbind(lon, lat)
  }
  
  #convert points matrix to SpatVects to work with mask function
  for (i in seq_along(latlon_list)) {
    lines_list[[i]] <- terra::vect(latlon_list[[i]], type = "lines", crs = crs(rast))
  }
  return(lines_list)
}

#apply function to get lines
lines_list <- list()
for (i in seq_along(x_list)) {
  lines_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], woody)
}

#make and save plots 
#function for plotting each contour over RAP and saving to output path
plot_lines_over_raster <- function(lines_list, rast, output_dir) {
  
  #since lines_list contains lists of contours for each direction, iterate over each list element
  for (j in seq_along(lines_list)) {
    #plot name
    plot_file <- sprintf("%s/plot_%03d.png", output_dir, j)
    #plot dimensions
    png(plot_file, width = 1000, height = 1000)
    
    #plot RAP tile
    plot(rast, main = sprintf("Plot %03d", j), col = terrain.colors(10))
    
    #plot all contours for each element
    for (i in seq_along(lines_list[[j]])) {
        lines(lines_list[[j]][[i]], col = "red", lwd = 2)
    }
    
    dev.off()
  }
}

plot_lines_over_raster(lines_list, woody, PathToOutput)
