#generate monthly directional values for geospatial data:
# 1) TWI (open topography product; also self calculated TWI if needed) 2) CHM (open topography product) 3) NDVI (planet labs data, processed in compute_NDVI.R)

#raster crs/extent converted in convert_UTM_to_coords.R

#load packages
library(terra)
library(dplyr)

#define data paths
raster_data_name <- "Cover"

PathToDataframeOutput <- "./Data/monthly_directional_ffps/raster_dataframes/FullRasterFrame.rds"

PathToMonthlyFootprintOutputs <- "./Data/monthly_directional_ffps"

PathToTWI <- "./Data/LiDAR/TWI/ReTWI.tif"
PathToCHM <- "./Data/LiDAR/CHM/reCHM.tif"
PathToCanopyCover <- "./Data/Image_classification/canopy.tif"
PathToReference <- "./Data/RAP/avg_rast.tif"
PathToNDVIfolder <- "./Data/Planet/2016to2019planetCOGs/reNDVI"

#load data
#for CHM filter for only tree heights (>3m)
rast_data <- rast(PathToCanopyCover)
#rast_data <- ifel(rast_data < 3, NA, rast_data)

#===============================================================================
#=======================amend contours to mask function=========================
#===============================================================================
#Function from Contours_to_Mask.R

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
  weighted_raster_val <- 0.3 * mean(unlist(rap_vals[[1]]), na.rm = T) + 0.3 * mean(unlist(rap_vals[[2]]), na.rm = T) + 0.3* mean(unlist(rap_vals[[3]]), na.rm = T)
  #veg_cover <- 0.6 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]]))
  #veg_cover <- 1 * sum(unlist(rap_vals[[1]]))
  
  return(weighted_raster_val)
}

#===============================================================================
#===============================apply function==================================
#===============================================================================
#tesing loop
#list RDS files; calculated/saved in calc_directional_ffp.R
ffp_objs <- list.files(PathToMonthlyFootprintOutputs, pattern = "\\.rds$", full.names = TRUE)

#list to store loop outputs
all_function_outputs <- list()

#loop through listed files:
for (file in ffp_objs) {
  #read in file
  ffp_list <- readRDS(file)
  
  #same as individual list- define lists for x and y coords from ffp;
  #create output list and apply function
  x_list <- list()
  y_list <- list()
  for (i in seq_along(ffp_list)) {
    x_list[[i]] <- ffp_list[[i]]$xr
    y_list[[i]] <- ffp_list[[i]]$yr
  }
  function_output_list <- list()
  for (i in seq_along(x_list)) {
    function_output_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], rast_data)
  }
  
  #name outputs:
  #extract month abbr from file name
  month_name <- sub("_calcd_ffp_list.rds", "", basename(file))
  #store outputs with month abbr and specify raster data type (define at the top of this script)
  all_function_outputs[[paste0(month_name, "_", raster_data_name)]] <- unlist(function_output_list)}

#convert to df with direction first
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]
directional_output_df <- data.frame(direction = deg_int_real)
#then add monthly values from loop 
for (col_name in names(all_function_outputs)) {
  directional_output_df[[col_name]] <- all_function_outputs[[col_name]]
}

cancov_df <- directional_output_df
chm_df <- directional_output_df
twi_df <- directional_output_df

#===============================================================================
#======================nested loop for multiple NDVI rasters====================
#===============================================================================
#list NDVI files (use reprojected rasters or will calculate NaN: reNDVI) and footprint objects
ndvi_files <- list.files(PathToNDVIfolder, pattern = "\\.tif$", full.names = TRUE)
ffp_objs <- list.files(PathToMonthlyFootprintOutputs, pattern = "\\.rds$", full.names = TRUE)

#output list
all_function_outputs <- list()

#loop through each month raster and footprint object
#no need to do fancy renaming/indexing if using the same naming convention of month abbr_file name
for (i in seq_along(ffp_objs)) {
  #load data from lists
  ndvi_rast <- rast(ndvi_files[i])
  ffp_list <- readRDS(ffp_objs[i])
  
  #same as functions/loops above
  x_list <- list()
  y_list <- list()
  for (j in seq_along(ffp_list)) {
    x_list[[j]] <- ffp_list[[j]]$xr
    y_list[[j]] <- ffp_list[[j]]$yr
  }
  function_output_list <- list()
  for (j in seq_along(x_list)) {
    function_output_list[[j]] <- ffp_contours_to_mask(x_list[[j]], y_list[[j]], ndvi_rast)
  }
  
  #naming/storing in final list (same as above)
  month_name <- sub("_calcd_ffp_list.rds", "", basename(ffp_files[i]))
  all_function_outputs[[paste0(month_name, "_NDVI")]] <- unlist(function_output_list)
}

#same
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]

directional_output_df <- data.frame(direction = deg_int_real)

for (col_name in names(all_function_outputs)) {
  directional_output_df[[col_name]] <- all_function_outputs[[col_name]]
}

ndvi_df <- directional_output_df

#===============================================================================
#==========================combine dataframes and save==========================
#===============================================================================
final_df <- full_join(chm_df, twi_df, by = "direction") %>%
  full_join(ndvi_df, by = "direction")%>%
  full_join(cancov_df, by = "direction")

saveRDS(final_df, file = PathToDataframeOutput)
