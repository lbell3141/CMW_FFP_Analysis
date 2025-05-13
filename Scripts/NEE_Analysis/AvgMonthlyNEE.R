#plot NEE by month
#Code to plot figure 1
library(dplyr)
library(lubridate)
library(plantecophys)
library(stringr)
library(purrr)
library(tidyverse)
library(patchwork)

#===============================================================================
#Dataframe Prep
#===============================================================================

# Load data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
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
    nee = NEE_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

# Split into direction windows
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

# Average by month and direction
dir_dat_avg <- dat_voi %>%
  group_by(mm, dir_group) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(direction = as.numeric(as.character(dir_group))) %>%   # <- THIS is the important fix
  select(mm, direction, nee)
plot_df <- dir_dat_avg
# Pivot data: split by month
month_split <- split(plot_df, plot_df$mm)
month_abbr <- month.abb

# Rename columns for each month
month_split <- map2(month_split, month_abbr, function(df, abbr) {
  df %>%
    rename_with(~ ifelse(.x %in% c("mm", "direction"), .x, paste0(abbr, "_", .x))) %>%
    select(-mm)
})

# Merge all months
plot_df <- reduce(month_split, full_join, by = "direction")
plot_df <- plot_df[1:18,]
#===============================================================================
#Plotting
#===============================================================================
# This is your function to create ONE monthly plot
create_monthly_nee_plot <- function(month_abbr, month_full) {
  variable_col <- paste0(month_abbr, "_nee")  # column like "Jan_nee", "Feb_nee", etc.
  
  # Find min and max values for the color scale
  min_value <- min(plot_df[[variable_col]], na.rm = TRUE)
  max_value <- max(plot_df[[variable_col]], na.rm = TRUE)
  
  ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(variable_col))) +  # Constant y value of 5
    geom_col(width = 20) +  # width matches your 20-degree bins
    coord_polar() +  # Keep the plot circular
    scale_fill_viridis_c(option = "viridis", limits = c(min_value, max_value),  # set the range
                         breaks = c(min_value, max_value),  # show only min and max
                         labels = c(sprintf("%.1f", min_value), sprintf("%.1f", max_value)),  # Round to nearest tenth
                         name = NULL) +  # Remove nee label
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),  # Remove gridlines behind the circles
      legend.position = "bottom",
      panel.background = element_blank(),  # Remove background circle
      plot.background = element_blank(),    # Remove plot background
      legend.title = element_text(size = 14),  # Increase font size for legend title
      legend.text = element_text(size = 10)    # Increase font size for legend labels
    ) +
    labs(title = month_full) +
    guides(fill = guide_colorbar(barwidth = 3, barheight = 0.5))  # Make the color bar less horizontally long
}

# ---- Set up months
month_abbr <- month.abb  # Jan, Feb, etc
month_full <- month.name # January, February, etc

# ---- Create list of plots
plots <- Map(create_monthly_nee_plot, month_abbr, month_full)

# ---- Combine into one big panel
final_plot <- wrap_plots(plots, ncol = 6) & 
  theme(legend.position = "bottom")

# ---- Display
final_plot + plot_annotation(title = "Average Directional NEE (µmolCO2 m-2 s-1)")
