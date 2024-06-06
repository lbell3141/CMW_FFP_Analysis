library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(plot3D)
library(geos)
library(terra)

FFP <- calc_footprint_FFP(7, NaN, 1.47091, 1000, -3.543327409, 0.018049909, 0.257615)
plot(FFP$x_ci,FFP$f_ci, type="l") 

#climatology====================================================================
dat_voi_test <- dat_voi%>%
  filter(yyyy == 2008)%>%
  filter(doy == 180)%>%
  mutate(sigma_v = c(1.3, 2, 0.2, 0.4, 0.2,1.3, 2, 0.2, 0.4, 0.2,1.3, 2, 0.2, 0.4, 0.2, 0.9, 0.2))%>%
  mutate(h = 1000)

FFP <- calc_footprint_FFP_climatology(dat_voi_test$zm, 
                                      dat_voi_test$zo, 
                                      dat_voi_test$u_mean, 
                                      dat_voi_test$h, 
                                      dat_voi_test$L, 
                                      dat_voi_test$sigma_v,
                                      dat_voi_test$u_star,
                                      dat_voi_test$wind_dir, 
                                      fig = 1,
                                      domain = c(-250,250,-250,250), 
                                      r = seq(0,90, by = 10))

image.plot(FFP_B$x_2d[1,], FFP_B$y_2d[,1], FFP_B$fclim_2d) 
for (i in 1:8) lines(FFP_B$xr[[i]], FFP_B$yr[[i]], type="l", col="red") 

#surf3D(FFP$x_2d, FFP$y_2d,FFP$fclim_2d) 
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
#===============================================================================
#======================contour extents to shapefiles(?)=========================
#===============================================================================

#xr and yr: list number = % contour in output

#list for line vectors
coord_pair_list <- list()
line_list <- list()
#make lines from FFP output coords
#length of x depends on optional r argument in the climatology function
for (i in 1:length(FFP$xr)) {
  #combine list values to make df of ordered pairs
  combined_df <- data.frame(x = FFP$xr[[i]], y = FFP$yr[[i]])
  xy_df <- data.frame(x = combined_df[[1]], y = combined_df[[2]], line_id = i)
  #store in list
  coord_pair_list[[i]] <- xy_df
}

#(gives error bc 100% contour list in NA)
for (i in 1:length(coord_pair_list)) {
  line_list[[i]] <- geos_make_linestring(coord_pair_list[[i]]$x, coord_pair_list[[i]]$y, z = NA_real_)
}

#georef: make (0,0) equal to tower coords. pixels in meters


