#generate a dataframe for directional canopy cover by month to use in driver_circles.R

#packages
library(sf)
library(terra)
library(dplyr)

#data
ffp_list <- readRDS("./Data/monthly_directional_ffps/jan_calcd_ffp_list.rds")
rast <- rast("./Data/Image_classification/canopy.tif")
twi <- rast("./Data/LiDAR/TWI/TWItwi.tif")

#===============================================================================
#============contour to mask as a function to apply to a list of ffps===========
#===============================================================================
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
    lines_list[[i]] <- vect(latlon_list[[i]], type = "lines")
  }
  
  #add raster CRS to FFP contours
  for (i in seq_along(lines_list)) {
    crs(lines_list[[i]]) <- crs(rast)
  }
  
  #make a list of 30%, 60%, and 90% contours
  part_con_list <- list(lines_list[[3]], lines_list[[6]], lines_list[[9]])
  #make a list of 60%, and 90% contours
  #part_con_list <- list(lines_list[[8]], lines_list[[9]])
  #part_con_list <- list(lines_list[[9]])
  
  #mask the raster with the contours
  masked_rast <- list()
  for (i in seq_along(part_con_list)) {
    masked_rast[[i]] <- mask(rast, part_con_list[[i]])
  }
  
  #subtract inner contours to get different contour groupings
  dif_con_list <- list(masked_rast[[1]], 
                       mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE),
                       mask(masked_rast[[3]], masked_rast[[2]], inverse = TRUE))
  #dif_con_list <- list(mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE))
  
  #compute avg values of pixels for each contour grouping
  rap_vals <- list()
  for (i in seq_along(dif_con_list)) {
    rap_vals[[i]] <- global(dif_con_list[[i]], fun = mean, na.rm = TRUE)
  }
  
  #weight contour areas
  veg_cover <- 0.3 * sum(unlist(rap_vals[[1]]), na.rm = T) + 0.3 * sum(unlist(rap_vals[[2]]), na.rm = T) + 0.3* sum(unlist(rap_vals[[3]]), na.rm = T)
  #veg_cover <- 0.6 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]]))
  #veg_cover <- 1 * sum(unlist(rap_vals[[1]]))
  return(veg_cover)
}

#===============================================================================
#============contour to mask as a function to apply to a list of ffps===========
#===============================================================================

#split ffp_list into a list of x coords and y coords
x_list <- list()
y_list <- list()
for (i in seq_along(ffp_list)) {
  x_list[[i]] <- ffp_list[[i]]$xr
  y_list[[i]] <- ffp_list[[i]]$yr
}

#run code for ffp_contours_to_mask function in Countours_to_Mask.R
#apply function to lists and loaded RAP data:
rap_ffp_list <- list()
for (i in seq_along(x_list)) {
  rap_ffp_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], twi)
}

#convert to df with correct WD
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]
#deg_int_real <- deg_int
rap_ffp_df <- data.frame(direction = deg_int_real,
                         veg_cover = unlist(rap_ffp_list)
)


jan <- rap_ffp_df

#list dfs
month_df_list <- list(jan)#, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

#bind into single df with month # identifier
for (i in seq_along(month_df_list)){
  month_df_list[[i]] <- month_df_list[[i]]%>%
    mutate(month = i)
}
month_cc <- bind_rows(month_df_list)

#save output
saveRDS(month_cc, file = "./Data/monthly_directional_ffps/month_cc.rds")
