library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(plot3D)
library(sf)
library(terra)

pathtoRAPrast <- "./Data/RAP/dif_rast.tif"

#1D FFP====================================================================
L_bar <- mean(dat_voi_test$L, na.rm = T)
ustar_bar <- mean(dat_voi$u_star, na.rm = T)

FFP <- calc_footprint_FFP(4.666667, NaN, 1.47091, 1000, L_bar, 0.4, ustar_bar)
plot(FFP$x_ci,FFP$f_ci, type="l") 

#climatology====================================================================
dat_voi <- dat_ffp
dat_voi_test <- dat_voi%>%
  filter(yyyy == 2019)%>%
  filter(doy == 3)%>%
  mutate(sigma_v = 0.4)%>%
  mutate(h = 1000)%>%
  mutate(z0 = NaN)

FFP <- calc_footprint_FFP_climatology(dat_voi_test$zm, 
                                      dat_voi_test$z0, 
                                      dat_voi_test$u_mean, 
                                      dat_voi_test$h, 
                                      dat_voi_test$L, 
                                      dat_voi_test$sigma_v,
                                      dat_voi_test$u_star,
                                      dat_voi_test$wind_dir, 
                                      fig = 1,
                                      domain = c(-200,200,-200,200), 
                                      r = seq(0,80, by = 10))

image.plot(ffp_list[[24]]$x_2d[1,], ffp_list[[24]]$y_2d[,1], ffp_list[[24]]$fclim_2d) 
for (i in 1:9) lines(ffp_list[[24]]$xr[[i]], ffp_list[[24]]$yr[[i]], type="l", col="red") 

#surf3D(FFP$x_2d, FFP$y_2d,FFP$fclim_2d) 
#===============================================================================
#======================contour extents to raster masks==========================
#===============================================================================
#xr and yr: list number is % contour in output

#list to store FFP xy pairs- separate columns for clarity
coord_pair_list <- list()

#format raw points as numeric matrix with NAs removed to avoid errors
for (i in 1:length(FFP$xr)) {
  df <- data.frame(x = as.numeric(FFP$xr[[i]]), y = as.numeric(FFP$yr[[i]]))
  df <- na.omit(df)
  coord_pair_list[[i]] <- as.matrix(df)
}

#convert standard plane coords to lat/lon coords (approximate distance conversion)
#1 deg ~ 111,111m
#latitude increments do not change w position on the globe but longitude does, so include cosine correction (convert degrees to radians to do so)
#(0,0) = 31.6637, -110.1777 = tower position
latlon_list <- list()
for (i in seq_along(coord_pair_list)) {
  lat <- (coord_pair_list[[i]][, 2] / 111111) + 31.6637
  lon <- (coord_pair_list[[i]][, 1] / (111111 * cos(31.6637 * pi / 180))) - 110.1777
  latlon_list[[i]] <- cbind(lon, lat)
}

#list to store line geom
lines_list <- list()
#convert points matrix to Spatvects to work with mask function
for (i in seq_along(latlon_list)) {
  lines_list[[i]] <- vect(latlon_list[[i]], type = "lines")
}

#loading RAP data
rast <- rast(pathtoRAPrast)
#add raster crs to FFP contours
for (i in seq_along(lines_list)) {
  crs(lines_list[[i]]) <- crs(rast)
}

#make a list of 30%, 60% and, 90% contours
part_con_list <- list(lines_list[[3]],lines_list[[6]],lines_list[[9]])
masked_rast <- list()
for (i in seq_along(part_con_list)){
  masked_rast[[i]]<- mask(rast, part_con_list[[i]])
  }

#subtract inner contours: inverse argument returns areas were rasters do NOT overlap 
dif_con_list <- list(masked_rast[[1]], 
                     mask(masked_rast[[2]], masked_rast[[1]], inverse = T),
                     mask(masked_rast[[3]], masked_rast[[2]], inverse = T))

#compute avg values (terra::global for stats) of pixels for each contour grouping in list
rap_vals <- list()
for (i in seq_along(dif_con_list)){
  rap_vals[i] <- global(dif_con_list[[i]], fun = mean, na.rm = TRUE)
#weight contour areas
  veg_cover <- 0.3 * sum(unlist(lapply(rap_vals, sum)))
}

#store values by hand in array
veg_vect <- c(1,2,3,4)
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
  #part_con_list <- list(lines_list[[3]], lines_list[[6]], lines_list[[9]])
  #make a list of 60%, and 90% contours
  part_con_list <- list(lines_list[[8]], lines_list[[9]])
  #part_con_list <- list(lines_list[[9]])
  
  #mask the raster with the contours
  masked_rast <- list()
  for (i in seq_along(part_con_list)) {
    masked_rast[[i]] <- mask(rast, part_con_list[[i]])
  }

  #subtract inner contours to get different contour groupings
  #dif_con_list <- list(masked_rast[[1]], 
  #                     mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE),
  #                     mask(masked_rast[[3]], masked_rast[[2]], inverse = TRUE))
  dif_con_list <- list(mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE))
  
  #compute avg values of pixels for each contour grouping
  rap_vals <- list()
  for (i in seq_along(dif_con_list)) {
    rap_vals[[i]] <- global(dif_con_list[[i]], fun = mean, na.rm = TRUE)
  }
  
  #weight contour areas
 # veg_cover <- 0.3 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]])) + 0.3* sum(unlist(rap_vals[[3]]))
  #veg_cover <- 0.6 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]]))
  veg_cover <- 1 * sum(unlist(rap_vals[[1]]))
  return(veg_cover)
}

#===============================================================================
#=========lateral velocity fluctuation (sigma_v) sensitivity test===============
#===============================================================================
#for a 10 degree directional window, what is the sensitivity of the footprint prediction to (reasonable) changes in the value of sigma_v
#test dist of sigma_v

deg_int <- seq(0, 360, by = 10)
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- paste0(deg_int[-length(deg_int)], "-", deg_int[-1])
names(split_dat) <- dir_names

test_df <- split_dat[[20]]

test_dist_A <- abs(rnorm(nrow(test_df), mean = 0.2, sd = 0.2))
test_dist_B <- abs(rnorm(nrow(test_df), mean = 0.5, sd = 0.2))
test_dist_C <- abs(rnorm(nrow(test_df), mean = 0.8, sd = 0.2))
test_dist_D <- abs(rnorm(nrow(test_df), mean = 1.1, sd = 0.2))
test_dist_E<- abs(rnorm(nrow(test_df), mean = 1.4, sd = 0.2))

FFP_A <- calc_footprint_FFP_climatology(test_df$zm, 
                                        test_df$zo, 
                                        test_df$u_mean, 
                                        test_df$h, 
                                        test_df$L, 
                                        test_dist_A,
                                        test_df$u_star,
                                        test_df$wind_dir, 
                                        fig = 1,
                                        domain = c(-250,250,-250,250), 
                                        r = seq(0,90, by = 10))

FFP_B <- calc_footprint_FFP_climatology(test_df$zm, 
                                        test_df$zo, 
                                        test_df$u_mean, 
                                        test_df$h, 
                                        test_df$L, 
                                        test_dist_B,
                                        test_df$u_star,
                                        test_df$wind_dir, 
                                        fig = 1,
                                        domain = c(-250,250,-250,250), 
                                        r = seq(0,90, by = 10))
FFP_C <- calc_footprint_FFP_climatology(test_df$zm, 
                                        test_df$zo, 
                                        test_df$u_mean, 
                                        test_df$h, 
                                        test_df$L, 
                                        test_dist_C,
                                        test_df$u_star,
                                        test_df$wind_dir, 
                                        fig = 1,
                                        domain = c(-250,250,-250,250), 
                                        r = seq(0,90, by = 10))
FFP_D <- calc_footprint_FFP_climatology(test_df$zm, 
                                        test_df$zo, 
                                        test_df$u_mean, 
                                        test_df$h, 
                                        test_df$L, 
                                        test_dist_D,
                                        test_df$u_star,
                                        test_df$wind_dir, 
                                        fig = 1,
                                        domain = c(-250,250,-250,250), 
                                        r = seq(0,90, by = 10))
FFP_E <- calc_footprint_FFP_climatology(test_df$zm, 
                                        test_df$zo, 
                                        test_df$u_mean, 
                                        test_df$h, 
                                        test_df$L, 
                                        test_dist_E,
                                        test_df$u_star,
                                        test_df$wind_dir, 
                                        fig = 1,
                                        domain = c(-250,250,-250,250), 
                                        r = seq(0,90, by = 10))
