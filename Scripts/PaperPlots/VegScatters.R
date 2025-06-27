# Load libraries
library(plantecophys)
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

#filter combd_df for summer
combd_df <- combd_df%>%
  filter(mm %in% 6:9)


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
# Plotting
#===============================================================================

# Fit models and extract R^2
lm_ndvi <- lm(gpp ~ NDVI, data = combd_df_zscores)
r2_ndvi <- summary(lm_ndvi)$r.squared

lm_chm <- lm(gpp ~ CHM, data = combd_df_zscores)
r2_chm <- summary(lm_chm)$r.squared

lm_cover <- lm(gpp ~ Cover, data = combd_df_zscores)
r2_cover <- summary(lm_cover)$r.squared

# Extract p-values from the summary of each model
p_ndvi <- summary(lm_ndvi)$coefficients[2, 4]   # p-value for NDVI slope
p_chm <- summary(lm_chm)$coefficients[2, 4]     # p-value for CHM slope
p_cover <- summary(lm_cover)$coefficients[2, 4] # p-value for Cover slope

# Print p-values
cat("P-value for NDVI:", p_ndvi, "\n")
cat("P-value for Canopy Height:", p_chm, "\n")
cat("P-value for Cover:", p_cover, "\n")

sig_label <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("ns")  # not significant
}




# Plot 1: GPP vs NDVI
p1 <- ggplot(combd_df_zscores, aes(x = NDVI, y = gpp)) +
  geom_point(color = "black", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_ndvi, 2), sig_label(p_ndvi),"\n"),
           hjust = 1.1, vjust = 1.5, size = 5)+
  labs(x = "NDVI", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Plot 2: GPP vs Canopy Height
p2 <- ggplot(combd_df_zscores, aes(x = CHM, y = gpp)) +
  geom_point(color = "black", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
                   label = paste0("R² = ", round(r2_chm, 2), sig_label(p_chm),"\n"),
                   hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "Canopy Height", y = "GPP") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Plot 3: GPP vs Cover
p3 <- ggplot(combd_df_zscores, aes(x = Cover, y = gpp)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_cover, 2), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "Canopy Cover", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Arrange the plots
grid.arrange(p2, p3, p1, ncol = 3)


#===============================================================================
# TWI plot
#===============================================================================
# Fit linear model for GPP vs TWI
lm_twi <- lm(TWI ~ gpp, data = combd_df_zscores)
r2_twi <- summary(lm_twi)$r.squared
p_twi <- summary(lm_cover)$coefficients[2, 4] # p-value for Cover slope
cat("P-value for twi:", p_twi, "\n")

# Create plot with updated point size and R² annotation
twi_plot <- ggplot(combd_df_zscores, aes(x = TWI, y = gpp)) +
  geom_point(color = "black", size = 1.5) +
  #geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  annotate("text", x = Inf, y = Inf, 
           label = paste0("R² = ", round(r2_cover, 2), "\n"),
           hjust = 1.1, vjust = 1.5, size = 5) +
  labs(x = "TWI", y = "") +
  theme_classic() +
  theme(
    text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
#===============================================================================
# add twi to veg scatters
#===============================================================================
grid.arrange(p2, p1, p3, twi_plot, ncol = 4)
