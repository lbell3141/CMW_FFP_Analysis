#plot meteorological linear model circles 20-deg for each month
#===============================================================================
# Load Packages
#===============================================================================
library(tidyverse)
library(lubridate)
library(patchwork)

#===============================================================================
# Data Preparation
#===============================================================================

# Load data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create cleaned primary dataframe
dat_cln <- dat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
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
  filter(HH_UTC >= 8 & HH_UTC <= 17) %>%
  mutate(
    dir_group = cut(wind_dir, breaks = seq(0, 360, by = 20), include.lowest = TRUE, labels = seq(20, 360, by = 20)),
    direction = as.numeric(as.character(dir_group))
  ) %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Split by month
mm_split_dat <- split(dat_cln, dat_cln$mm)

#===============================================================================
# Modeling Function
#===============================================================================

mod_mm <- function(x) {
  model <- lm(gpp ~ swc + temp_atmos + rel_h + ppfd + wind_sp + precip + HH_UTC, data = x)
  
  x %>%
    mutate(
      modeled_gpp = predict(model, x),
      diff_gpp = modeled_gpp - gpp
    ) %>%
    filter(modeled_gpp > 0) %>%
    group_by(dir_group) %>%
    summarize(
      mod_avg_gpp = mean(modeled_gpp, na.rm = TRUE),
      real_avg_gpp = mean(gpp, na.rm = TRUE),
      diff_avg_gpp = mean(diff_gpp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(direction = as.numeric(as.character(dir_group))) %>%
    arrange(direction)
}

# Apply modeling function to each month's data
mm_mod_list <- lapply(mm_split_dat, mod_mm)

# Find overall color scale range
all_diff_avg_gpp <- unlist(lapply(mm_mod_list, \(df) df$diff_avg_gpp))
overall_min <- min(all_diff_avg_gpp, na.rm = TRUE)
overall_max <- max(all_diff_avg_gpp, na.rm = TRUE)

#===============================================================================
# Plotting Function
#===============================================================================

plot_gpp_difference <- function(df, month) {
  ggplot(df) +
    geom_col(
      aes(x = direction, y = 5, fill = diff_avg_gpp),
      width = 20
    ) +
    coord_polar() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(overall_min, overall_max),
      name = "GPP (µmolCO2 m-2 s-1)"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = "bottom",
      text = element_text(color = "black"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    ) +
    guides(fill = guide_colorbar(barwidth = 6, barheight = 1)) +
    labs(title = month)
}

#===============================================================================
# Create and Combine Plots
#===============================================================================

month_names <- month.name
plot_list <- Map(plot_gpp_difference, mm_mod_list, month_names)

final_plot <- wrap_plots(plot_list, ncol = 6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display
final_plot +
  plot_annotation(title = "Directional Residual GPP") +
  theme(text = element_text(color = "black"))
