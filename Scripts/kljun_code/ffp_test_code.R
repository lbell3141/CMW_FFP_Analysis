library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(plot3D)
library(sf)
library(terra)

pathtoRAPrast <- "./Data/RAP/dif_rast.tif"

#1D FFP====================================================================

FFP <- calc_footprint_FFP(7, NaN, 1.47091, 1000, -3.543327409, 0.018049909, 0.257615)
plot(FFP$x_ci,FFP$f_ci, type="l") 

#climatology====================================================================
dat_voi_test <- dat_voi%>%
  filter(yyyy == 2019)%>%
  filter(doy == 152)%>%
  mutate(sigma_v = c(1.3, 2, 0.2, 0.4, 0.2,1.3, 2, 0.2, 0.4, 0.2,1.3, 2, 0.2, 0.4, 0.2, 0.9, 0.2, 0.4, 0.7, 0.8))%>%
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

image.plot(FFP$x_2d[1,], FFP$y_2d[,1], FFP$fclim_2d) 
for (i in 1:8) lines(FFP$xr[[i]], FFP$yr[[i]], type="l", col="red") 

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
#latitude increments do not change w position on the globe but longistude does, so include cosine correction (convert degrees to radians to do so)
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

e <- lines_list[[9]]
testmask <- mask(rast, e)
plot(testmask)

