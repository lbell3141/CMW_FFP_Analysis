#make a map of photosynthetic capacity around the tower using 
#gridded LAI and avg directional GPP. 
#LiDAR flown July 2021, so we extract avg July GPP
#https://gis.stackexchange.com/questions/297719/splitting-circle-polygon-into-equal-sectors-in-qgis
library(terra)
library(dplyr)
library(viridis)

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
new_angles <- atan2(cent_coords[, "x"] - center_x, cent_coords[, "y"] - center_y) * (180 / pi)
angles <- (new_angles + 360) %% 360
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
#pc_map <- lai_rast/gpp_raster
#pc_map <- gpp_raster*lai_rast
#remove extra NA values
cropped_pc_map <- trim(pc_map)
plot(cropped_pc_map)

#remove outliers
#value_range <- quantile(values(pc_map), probs = c(0.001, 0.99), na.rm = TRUE)
#clipped_pc_map <- clamp(cropped_pc_map, lower = value_range[1], upper = value_range[2])

#plot(
#  clipped_pc_map, 
#  axes = FALSE,
#  frame.plot = FALSE,
#  col = viridis(100),
#  main = "Photosynthetic Capacity (mmol Carbon / m2 Leaf Area)")




#fix res issue
library(ggplot2)
pc_map_df <- as.data.frame(pc_map, xy = TRUE)
#replace outliers with upper end values for visual clarity
pc_map_df$direction <- ifelse(pc_map_df$direction > 10, 11, pc_map_df$direction)

ggplot(pc_map_df, aes(x = x, y = y, fill = direction)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "", title = "Photosynthetic Capacity (mmol C/Leaf Area)")+
  theme(plot.title = element_text(hjust = 0.5, size = 55),
        legend.text = element_text(size = 30))+
  guides(fill = guide_colorbar(barwidth = 3.5, barheight = 35))

