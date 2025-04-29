#TWI paper plots
# Load libraries
library(tidyr)
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

# --- Step 3: Merge the two datasets ---
combd_df <- inner_join(dir_dat_avg, mm_dat_long, by = c("mm", "direction"))

# Z-scores 
combd_df_zscores <- combd_df %>%
  select(-direction) %>%
  group_by(mm) %>%
  reframe(
    across(where(is.numeric), ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
  ) %>%
  ungroup() %>%
  bind_cols(select(combd_df, direction))

#===============================================================================
# TWI plot
#===============================================================================
twi_plot <- ggplot(combd_df_zscores, aes(x = gpp, y = TWI)) +
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
# TWI Raster
#===============================================================================
