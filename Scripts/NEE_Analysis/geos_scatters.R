library(terra)

rf_pred <- readRDS("./Data/30minpredictionsRF.RDS")
rf_df <- bind_rows(rf_pred, .id = "mm")

gpp_dat <- dat_voi %>%
  select(yyyy, mm, gpp, dir_group) %>%
  rename(observed = gpp) %>%
  filter(!is.na(observed))

gpp_yr <- merge(rf_df, gpp_dat, by = c("mm", "observed", "dir_group")) %>%
  filter(yyyy == 2021, mm %in% 6:9) %>%
  mutate(residual = predicted - observed)%>%
  group_by(mm, dir_group)%>%
  summarize(residual = mean(residual, na.rm = T))



#testa <- readRDS("./Data/monthly_directional_ffps/raster_dataframes/FullRasterFrame.rds")

#CHM, TWI from initial footprints
#===============================================================================
#===============================================================================
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
  veg_cover <- 0.3 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]])) + 0.3* sum(unlist(rap_vals[[3]]))
  #veg_cover <- 0.6 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]]))
  #veg_cover <- 1 * sum(unlist(rap_vals[[1]]))
  return(veg_cover)
}

#===============================================================================
#===============================================================================
#canopy cover, NDVI, and LAI need to be redone
PathToReference <- "./Data/RAP/avg_rast.tif"
ref_rast <- rast(PathToReference)
#correct all crs before function application

PathToTWI <- "./Data/LiDAR/TWI/ReTWI.tif"
twi <- rast(PathToTWI)

PathToCHM <- "./Data/LiDAR/CHM/reCHM.tif"
chm <- rast(PathToCHM)

#modify CHM for canopy cover:
canopy <- chm>2
canopy_num <- as.numeric(canopy)

#_-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_
PathToLAI <- "./Data/LiDAR/LAI.tif"
lai <- rast(PathToLAI)
lai <- project(lai, crs(ref_rast), method = "bilinear")

#_-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_
PathToNDVI
#===============================================================================
#rerun Contours_to_Mask.R 107-168 to build geospatial dataframe

raster_data_name <- "TWI"
PathToDataframeOutput <- "./Data/monthly_directional_ffps/summer2021/dfs/dataframe.rds"
PathToMonthlyFootprintOutputs <- "./Data/monthly_directional_ffps/summer2021"

#_-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_-_-__-_
rast_data <- twi

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




