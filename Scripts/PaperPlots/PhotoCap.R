library(tidyr)
library(plantecophys)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(viridis)
library(gridExtra)
library(ggplot2)

library(terra)
library(dplyr)
library(viridis)

PathToLAI <- "./Data/LiDAR/LAI.tif"
lai_UTM <- rast(PathToLAI)
#reproject to WGS84
lai_rast <- project(lai_UTM, "EPSG:4326")

# Load and clean the AMF data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create the primary dataframe with necessary variables
dat_voi <- dat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

# Create 20-degree direction bins
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)

dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

#filter for lidar flight time
dat_voi <- dat_voi%>%
  filter(mm == 7)

dir_dat_avg <- dat_voi %>%
  mutate(
    direction = cut(wind_dir, breaks = seq(0, 360, by = 20),
                    labels = seq(20, 360, by = 20),
                    include.lowest = TRUE) %>%
      as.character() %>%
      as.numeric()
  ) %>%
  group_by(mm, direction) %>%
  summarise(gpp = mean(gpp, na.rm = TRUE), .groups = "drop") %>%
  filter(if_all(everything(), ~ !is.na(.)))

july_gpp <- dir_dat_avg

#rasterize GPP 
#load shapefile
PathToShp <- "./Data/d18_poly.shp"
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
wedge_num <- 18
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
gpp_df <- as.data.frame(gpp_raster, xy= T)
gpp_df <- gpp_df %>%
  rename(gpp = direction)
lai_df <- as.data.frame(lai_rast, xy = T)
# Merge the dataframes
combd <- merge(gpp_df, lai_df, by = c("x", "y"), all.x = TRUE)%>%
  mutate(direction = ifelse(is.na(LAI), NA, gpp / LAI))
#replace outliers with upper end values for visual clarity
combd$direction <- ifelse(combd$direction > 10, 11, combd$direction)

######=============Plot================
# Calculate max, middle, and mean values for the color scale
max_value <- max(combd$direction, na.rm = TRUE)
mean_value <- mean(combd$direction, na.rm = TRUE)
middle_value <- (max_value + min(combd$direction, na.rm = TRUE)) / 2

# Create the plot with the updated legend position and custom labels
ggplot(combd, aes(x = x, y = y, fill = direction)) +
  geom_raster() +
  scale_fill_viridis_c(
    breaks = c(min(combd$direction, na.rm = TRUE), middle_value, max_value), 
    labels = c(
      round(min(combd$direction, na.rm = TRUE), 0),
      round(middle_value, 0),
      round(max_value, 0)
    )
  ) +
  theme_void() +
  labs(fill = 
         "Photosynthetic Capacity
(μmol CO2 m-2 leaf area)") +  # Removed plot title
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_text(size = 18, hjust = 0.5),  # Customize legend title size and center it
    legend.title.position = "top",  # Move legend title above the colorbar
    legend.text = element_text(size = 16),  # Customize legend text size
    legend.key.width = unit(1, "cm"),  # Adjust width of color bar key
    legend.key.height = unit(1, "cm")  # Adjust height of color bar key
  ) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 2))  # Customize the color bar


#=================================Plot LAI======================================
max_value <- max(combd$LAI, na.rm = TRUE)
mean_value <- mean(combd$LAI, na.rm = TRUE)
middle_value <- (max_value + min(combd$LAI, na.rm = TRUE)) / 2

ggplot(combd, aes(x = x, y = y, fill = LAI)) +
  geom_raster() +
  scale_fill_viridis_c(
    breaks = c(min(combd$direction, na.rm = TRUE), middle_value, max_value), 
    labels = c(
      round(min(combd$direction, na.rm = TRUE), 0),
      round(middle_value, 0),
      round(max_value, 0)
    )
  ) +
  theme_void() +
  labs(fill = 
         "Leaf Area Index)") +  # Removed plot title
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_text(size = 18, hjust = 0.5),  # Customize legend title size and center it
    legend.title.position = "top",  # Move legend title above the colorbar
    legend.text = element_text(size = 16),  # Customize legend text size
    legend.key.width = unit(1, "cm"),  # Adjust width of color bar key
    legend.key.height = unit(1, "cm")  # Adjust height of color bar key
  ) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 2))  # Customize the color bar
