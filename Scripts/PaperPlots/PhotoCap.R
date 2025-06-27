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
  filter(mm == 7,
         yyyy == 2021)

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
library(ggplot2)
library(dplyr)
library(grid)
library(patchwork)
library(ggpubr)
library(cowplot)

#=================== Raster Plot (p4) ===================
max_value <- max(combd$direction, na.rm = TRUE)

p4 <- ggplot(combd, aes(x = x, y = y, fill = direction)) +
  geom_raster() +
  coord_fixed() +  # <- fixes the aspect ratio
  scale_fill_viridis_c(
    breaks = c(min(combd$direction, na.rm = TRUE), max(combd$direction, na.rm = TRUE)), 
    labels = c(
      round(min(combd$direction, na.rm = TRUE), 0),
      round(max(combd$direction, na.rm = TRUE), 0)
    )
  ) +
  theme_void() +
  labs(fill = "Photosynthetic Capacity\n(μmol CO2 m-2 leaf area)") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, hjust = 0.5),  # Increase margin to raise the title
    legend.title.position = "top",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(1, "cm")
  ) +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 2))

#=================== LAI Plot (p3) ===================
sec_lai <- merge(dir_dat_avg, combd, by = "gpp") %>%
  rename(direction = direction.x) %>%
  group_by(direction) %>%
  summarise(lai = mean(LAI, na.rm = TRUE))

p3 <- ggplot(sec_lai, aes(x = direction, y = 5, fill = lai)) +
  geom_col(width = 20) +
  coord_polar() +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(min(sec_lai$lai, na.rm = TRUE), max(sec_lai$lai, na.rm = TRUE)),
    breaks = c(min(sec_lai$lai, na.rm = TRUE), max(sec_lai$lai, na.rm = TRUE)),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "LAI") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5),  # Increase margin to raise the title
    legend.title.position = "top",
    legend.text = element_text(size = 13)
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3))

#=================== PC Plot (p2) ===================
sec_PC <- merge(dir_dat_avg, combd, by = "gpp") %>%
  rename(direction = direction.x) %>%
  group_by(direction) %>%
  summarise(PC = mean(direction.y, na.rm = TRUE))

p2 <- ggplot(sec_PC, aes(x = direction, y = 5, fill = PC)) +
  geom_col(width = 20) +
  coord_polar() +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(min(sec_PC$PC, na.rm = TRUE), max(sec_PC$PC, na.rm = TRUE)),
    breaks = c(min(sec_PC$PC, na.rm = TRUE), max(sec_PC$PC, na.rm = TRUE)),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "PC") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5),  # Increase margin to raise the title
    legend.title.position = "top",
    legend.text = element_text(size = 13)
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3))

#=================== GPP Plot (p1) ===================
sec_gpp <- merge(dir_dat_avg, combd, by = "gpp") %>%
  rename(direction = direction.x) %>%
  group_by(direction) %>%
  summarise(gpp = mean(gpp, na.rm = TRUE))

p1 <- ggplot(sec_gpp, aes(x = direction, y = 5, fill = gpp)) +
  geom_col(width = 20) +
  coord_polar() +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(min(sec_gpp$gpp, na.rm = TRUE), max(sec_gpp$gpp, na.rm = TRUE)),
    breaks = c(min(sec_gpp$gpp, na.rm = TRUE), max(sec_gpp$gpp, na.rm = TRUE)),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "GPP") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5),  # Increase margin to raise the title
    legend.title.position = "top",
    legend.text = element_text(size = 13)
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3))

#=================== Combine All Plots ===================
# ---- 1. Create p4 without the legend ----
p4_nolegend <- p4 + theme(legend.position = "none")

# ---- 2. Extract the legend from p4 ----
legend_p4 <- ggpubr::get_legend(
  p4 + 
    guides(fill = guide_colorbar(barwidth = 3, barheight = 1)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = 13),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(2, "cm")
    )
)

# ---- 3. Left column: p4 plot stacked with its legend (tight spacing) ----
left_column <- plot_grid(
  p4_nolegend,
  legend_p4,
  ncol = 1,
  rel_heights = c(1, 0.3)  # Adjust to control legend spacing
)

# ---- 4. Right column: p1, p3, p2 stacked vertically ----
right_column <- plot_grid(
  p1, p3, p2,
  ncol = 1,
  rel_heights = c(1, 1, 1)
)

# ---- 5. Combine both columns side-by-side ----
final_plot <- plot_grid(
  left_column,
  right_column,
  ncol = 2,
  rel_widths = c(1.4, 1.3)  # Adjust width balance between columns
)

# ---- 6. Display final plot ----
final_plot

#=================== Omit High Res Plot ===================
#=================== LAI Plot (p3) ===================
sec_lai <- merge(dir_dat_avg, combd, by = "gpp") %>%
  rename(direction = direction.x) %>%
  group_by(direction) %>%
  summarise(lai = mean(LAI, na.rm = TRUE))

p3 <- ggplot(sec_lai, aes(x = direction, y = 5, fill = lai)) +
  geom_col(width = 20) +
  coord_polar() +
  scale_fill_viridis_c(
    option = "viridis",
    limits = range(sec_lai$lai, na.rm = TRUE),
    breaks = range(sec_lai$lai, na.rm = TRUE),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "\nLAI\n") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5, margin = margin(t = -20)),
    legend.text = element_text(size = 13),
    legend.title.position = "top"
  ) +
  guides(fill = guide_colorbar(barwidth = 3.5, barheight = 1))

#=================== PC Plot (p2) ===================
sec_PC <- merge(dir_dat_avg, combd, by = "gpp") %>%
  rename(direction = direction.x) %>%
  group_by(direction) %>%
  summarise(PC = mean(direction.y, na.rm = TRUE))

p2 <- ggplot(sec_PC, aes(x = direction, y = 5, fill = PC)) +
  geom_col(width = 20) +
  coord_polar() +
  scale_fill_viridis_c(
    option = "viridis",
    limits = range(sec_PC$PC, na.rm = TRUE),
    breaks = range(sec_PC$PC, na.rm = TRUE),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "Photosynthetic\nCapacity\n") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5, margin = margin(t = -20)),
    legend.text = element_text(size = 13),
    legend.title.position = "top"
  ) +
  guides(fill = guide_colorbar(barwidth = 3.5, barheight = 1))

#=================== GPP Plot (p1) ===================
sec_gpp <- merge(dir_dat_avg, combd, by = "gpp") %>%
  rename(direction = direction.x) %>%
  group_by(direction) %>%
  summarise(gpp = mean(gpp, na.rm = TRUE))

p1 <- ggplot(sec_gpp, aes(x = direction, y = 5, fill = gpp)) +
  geom_col(width = 20) +
  coord_polar() +
  scale_fill_viridis_c(
    option = "viridis",
    limits = range(sec_gpp$gpp, na.rm = TRUE),
    breaks = range(sec_gpp$gpp, na.rm = TRUE),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(fill = "\nGPP\n") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 16, hjust = 0.5, margin = margin(t = -20)),
    legend.text = element_text(size = 12),
    legend.title.position = "top"
  )+
  guides(fill = guide_colorbar(barwidth = 3.5, barheight = 1))

#=================== Combine all ===================
(p1 | p3 | p2) 

