#make a map of photosynthetic capacity around the tower using 
#gridded LAI and avg directional GPP. 
#LiDAR flown July 2021, so we extract avg July GPP

library(terra)
library(dplyr)

PathToLAI <- "./Data/LiDAR/LAI3mrepro.tif"
lai_rast <- rast(PathToLAI)

#load dir_dat_avg from driver_circles.R
#extract relevant data (July GPP)
july_gpp <- dir_dat_avg %>%
  filter(month == 7)%>%
  select(direction, gpp)
july_gpp <- july_gpp[, 2:3]

#load shapefile
PathToShp <- "./Data/LiDAR/poly.shp"
poly <- vect(PathToShp)

#add info to wedges
#center of each wedge and extract coords
cent <- centroids(poly)
cent_coords <- geom(cent)[, c("x", "y")]
#define circle center
center_x <- mean(cent_coords[, "x"])
center_y <- mean(cent_coords[, "y"])
#calc deviation from north in deg and normalize to 360
angles <- atan2(cent_coords[, "x"] - center_x, cent_coords[, "y"] - center_y) * (180 / pi)
angles <- (new_angles + 360) %% 360  # Normalize to 0-360
#make wedge numbers
wedge_num <- 36
ang_wind <- seq(0, 360, length.out = wedge_num + 1)
wedge_directions <- cut(
  angles,
  breaks = ang_wind,
  labels = 1:wedge_num,
  include.lowest = T,
  right = F
)
#apply directions
poly$direction <- as.integer(wedge_directions)
direction_raster <- rasterize(poly, lai_rast, field = "direction")
plot(direction_raster)
#use gpp df to replace direction codes with gpp values
gpp_lookup <- setNames(july_gpp$gpp, july_gpp$direction)
gpp_raster <- direction_raster 
values(gpp_raster) <- gpp_lookup[values(gpp_raster)]
plot(gpp_raster)

#create photosynthetic capacity (PC) map
pc_map <- gpp_raster/lai_rast
pc_map <- lai_rast/gpp_raster
pc_map <- gpp_raster*lai_rast
#remove extra NA values
cropped_pc_map <- trim(pc_map)
plot(cropped_pc_map)

#remove outliers
value_range <- quantile(values(cropped_pc_map), probs = c(0.01, 0.99), na.rm = TRUE)
clipped_pc_map <- clamp(cropped_pc_map, lower = value_range[1], upper = value_range[2])

plot(
  clipped_pc_map, 
  axes = FALSE,
  frame.plot = FALSE,
  col = viridis(100),
  main = "Photosynthetic Efficiency (mmol Carbon / Leaf Area)")
