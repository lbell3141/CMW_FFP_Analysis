# ============================================================
# Four-Site GPP Climatology Plot (Using Mixed Raw + Processed Inputs)
# ============================================================

library(dplyr)
library(lubridate)
library(plantecophys)
library(ggplot2)
library(zoo)

# ------------------------------------------------------------
# 1. Function to compute climatology from a dataframe 
#    already containing: site, doy, gpp, HH_UTC  (your dat_voi format)
# ------------------------------------------------------------
make_climatology_from_datvoi <- function(df, site_name) {
  
  df %>%
    mutate(site = site_name) %>%
    filter(HH_UTC >= 8 & HH_UTC <= 17) %>%  # daytime only
    group_by(site, doy) %>%
    summarise(
      avg_gpp = mean(gpp, na.rm = TRUE),
      se_gpp  = sd(gpp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(doy) %>%
    mutate(gpp_roll = rollmean(avg_gpp, k = 7, fill = NA, align = "center"))
}

# ------------------------------------------------------------
# 2. CMW must start from raw CSV file
# ------------------------------------------------------------
df_CMW_raw <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",
                       na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

dat_voi_CMW <- df_CMW_raw %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm   = month(TIMESTAMP_START),
    doy  = yday(TIMESTAMP_START),
    day  = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    gpp    = GPP_PI
  )

clim_CMW <- make_climatology_from_datvoi(dat_voi_CMW, "CMW")

# ------------------------------------------------------------
# 3. Process SRM, SRG, Wkg (already in dat_voi_ format)
# ------------------------------------------------------------

clim_SRM <- make_climatology_from_datvoi(dat_voi_srm, "SRM")
clim_SRG <- make_climatology_from_datvoi(dat_voi_srg, "SRG")
clim_Wkg <- make_climatology_from_datvoi(dat_voi_Wkg, "Wkg")

# ------------------------------------------------------------
# 4. Combine datasets
# ------------------------------------------------------------
clim_all <- bind_rows(clim_CMW, clim_SRM, clim_SRG, clim_Wkg)

# ------------------------------------------------------------
# 5. Plot
# ------------------------------------------------------------
climatology_plot_all <- ggplot(clim_all, aes(x = doy, color = site)) +
  
  # ribbons
  geom_ribbon(aes(
    ymin = avg_gpp - se_gpp,
    ymax = avg_gpp + se_gpp,
    fill = site
  ),
  alpha = 0.2,
  color = NA) +
  
  # growing season highlight
  annotate("rect", xmin = 152, xmax = 273, ymin = -Inf, ymax = Inf,
           fill = "pink", alpha = 0.5) +
  
  geom_line(aes(y = avg_gpp), size = 1) +
  
  labs(
    x = "Day of Year",
    y = "GPP (mmol CO2/m2/s)",
    color = "Site",
    fill = "Site",
    title = ""
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "right"
  )

print(climatology_plot_all)
