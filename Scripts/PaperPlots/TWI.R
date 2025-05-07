#TWI paper plots
# Load libraries
library(tidyr)
library(plantecophys)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(viridis)
library(gridExtra)

#===============================================================================
# Data Prep
#===============================================================================

# Load the full raster data frame
PathToMmDat <- "./Data/monthly_directional_ffps/raster_dataframes/FullRasterFrame.rds"
mm_dat <- readRDS(PathToMmDat)

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
# Map month abbreviations to numbers
month_map <- c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
               "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)

# --- Step 1: Average gpp by month and 20-degree wind direction bins ---
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

avgGPPvals <- dir_dat_avg%>%
  group_by(direction)%>%
  summarize(avgGPP = mean(gpp, na.rm = T))
  
# --- Step 2: Process mm_dat to tidy long format ---
mm_dat_long <- mm_dat %>%
  pivot_longer(
    cols = -direction,
    names_to = c("mm", "variable"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    mm = month_map[mm]  # map month abbreviations directly
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )
avgTWIvals <- mm_dat_long %>%
  mutate(direction_bin = ceiling(direction / 20) * 20) %>%  # Bin direction to nearest 20 (e.g., 10 & 20 → 20)
  group_by(direction_bin) %>%
  summarize(avgTWI = mean(TWI, na.rm = TRUE)) %>%
  arrange(direction_bin)%>%
  rename(direction = direction_bin)

# --- Step 3: Merge the two datasets ---
combd_df <- inner_join(avgGPPvals, avgTWIvals, by = c("direction"))

#===============================================================================
# TWI plot - avg values for each direction (no month component)
#===============================================================================
twi_plot <- ggplot(combd_df, aes(x = avgGPP, y = avgTWI)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  labs(x = "GPP", y = "TWI") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),  # Increase axis title font size
    axis.text = element_text(size = 12)    # Increase axis value label font size
  ) +
  ggtitle("")
twi_plot
#===============================================================================
# TWI plot - zscores for each direction for each month 
#===============================================================================
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

# --- Step 2: Process mm_dat to tidy long format ---
mm_dat_long <- mm_dat %>%
  pivot_longer(
    cols = -direction,
    names_to = c("mm", "variable"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    mm = month_map[mm]  # map month abbreviations directly
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )%>%
  select(mm, direction, TWI)
avgTWI_20d <- mm_dat_long %>%
  mutate(direction_bin = ceiling(direction / 20) * 20) %>%  # Assign to 20° bins
  group_by(mm, direction_bin) %>%
  summarize(TWI = mean(TWI, na.rm = TRUE), .groups = "drop") %>%
  arrange(mm, direction_bin)%>%
  rename(direction = direction_bin)

# --- Step 3: Merge the two datasets ---
combd_df <- inner_join(dir_dat_avg, avgTWI_20d, by = c("mm", "direction"))

# Z-scores 
combd_df_zscores <- combd_df %>%
  select(-direction) %>%
  group_by(mm) %>%
  reframe(
    across(where(is.numeric), ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
  ) %>%
  ungroup() %>%
  bind_cols(select(combd_df, direction))

# Fit linear model
lm_twi <- lm(gpp ~ TWI, data = combd_df_zscores)
r2_twi <- summary(lm_twi)$r.squared

# Create plot
twi_plot <- ggplot(combd_df_zscores, aes(x = TWI, y = gpp)) +
  geom_point(color = "black", size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, label = paste0("R² = ", round(r2_twi, 2)),
           hjust = 1.1, vjust = 1.5, size = 6) +
  labs(x = "TWI", y = "GPP") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  ) +
  ggtitle("")


#===============================================================================
# TWI Raster
#===============================================================================

library(terra)
library(RColorBrewer)

#=======================
# Set resolution
#=======================
resolution <- 2  # Change this to try different resolutions

#=======================
# Load and resample raster
#=======================
twi <- rast("./Data/LiDAR/TWI/TWIfullarea.tif")

twi_template <- rast(
  ext(twi),
  resolution = resolution,
  crs = crs(twi)
)

twi_resampled <- resample(twi, twi_template, method = "bilinear")

#=======================
# Create buffer and clip
#=======================
pt_ll <- vect(data.frame(lon = -110.1783, lat = 31.6633),
              geom = c("lon", "lat"), crs = "EPSG:4326")
pt_utm <- project(pt_ll, crs(twi_resampled))
buf_200m <- buffer(pt_utm, width = 200)

twi_clipped <- mask(crop(twi_resampled, buf_200m), buf_200m)

#=======================
# Color palette setup
#=======================
n_colors <- 100
pal <- colorRampPalette(c("red", "white", "blue"))(n_colors)
zlim <- range(values(twi_clipped), na.rm = TRUE)

#=======================
# Layout for raster + legend
#=======================
library(terra)
library(ggplot2)
library(RColorBrewer)

#=======================
# Set resolution
#=======================
resolution <- 1  # Change this to try different resolutions

#=======================
# Load and resample raster
#=======================
twi <- rast("./Data/LiDAR/TWI/TWIfullarea.tif")

twi_template <- rast(
  ext(twi),
  resolution = resolution,
  crs = crs(twi)
)

twi_resampled <- resample(twi, twi_template, method = "bilinear")

#=======================
# Create buffer and clip
#=======================
pt_ll <- vect(data.frame(lon = -110.1783, lat = 31.6633),
              geom = c("lon", "lat"), crs = "EPSG:4326")
pt_utm <- project(pt_ll, crs(twi_resampled))
buf_200m <- buffer(pt_utm, width = 200)

twi_clipped <- mask(crop(twi_resampled, buf_200m), buf_200m)

#=======================
# Convert raster to data frame for ggplot
#=======================
twi_df <- as.data.frame(twi_clipped, xy = TRUE, na.rm = TRUE)

#=======================
# Color palette setup
#=======================
n_colors <- 100
pal <- colorRampPalette(c("red", "white", "blue"))(n_colors)

#=======================
# Plotting with ggplot2
#=======================
ggplot(twi_df, aes(x = x, y = y, fill = TWIfullarea)) +
  geom_raster() +  # Use geom_raster to plot raster data
  scale_fill_gradientn(
    colors = pal, 
    na.value = "transparent",
    breaks = c(min(twi_df$TWIfullarea, na.rm = TRUE), max(twi_df$TWIfullarea, na.rm = TRUE)),  # Define the breaks for the color bar
    labels = c("Dry", "Wet")  # Set custom labels
  ) +  # Custom color palette
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = "bottom",  # Position of the color bar (bottom)
    text = element_text(color = "black"),
    legend.text = element_text(size = 21, color = "black"),  # Increase font size of legend labels
    legend.title = element_blank()  # Remove legend title
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 7,  # Control width (horizontal length of the bar)
      barheight = 2,  # Control height (vertical height of the color bar)
      ticks = FALSE  # Remove ticks
    )
  ) +
  labs(title = "")

