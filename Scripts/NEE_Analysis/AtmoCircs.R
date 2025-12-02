#plot atmospheric drivers for June, July, Aug, Sept

# Load libraries
library(dplyr)
library(lubridate)
library(purrr)
library(plantecophys)
library(ggplot2)
library(viridis)
library(patchwork)
library(tidyr)

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
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
   filter(HH_UTC >= 8 & HH_UTC <= 17)

# Bin wind direction into 20° groups
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

# Variables and months
vars <- c("gpp", "temp_atmos", "swc", "ppfd", "wind_sp", "rel_h")
months_to_plot <- 6:9
month_abbr <- month.abb[months_to_plot]
month_full <- month.name[months_to_plot]

# Summarize by month and direction
dir_dat_avg <- dat_voi %>%
  filter(mm %in% months_to_plot) %>%
  group_by(mm, dir_group) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(direction = as.numeric(as.character(dir_group))) %>%
  select(mm, direction, all_of(vars))

# Split and reshape by month
month_split <- split(dir_dat_avg, dir_dat_avg$mm)
month_split <- map2(month_split, month_abbr, function(df, abbr) {
  df %>%
    rename_with(~ ifelse(.x %in% c("mm", "direction"), .x, paste0(abbr, "_", .x))) %>%
    select(-mm)
})
plot_df <- reduce(month_split, full_join, by = "direction")
plot_df <- plot_df[1:18, ]  # keep 18 wind directions

library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# Function to create a circular plot for each variable in the wide format
create_circular_plot_wide <- function(data, variable_col) {
  min_value <- min(data[[variable_col]], na.rm = TRUE)
  max_value <- max(data[[variable_col]], na.rm = TRUE)
  
  ggplot(data, aes(x = direction, y = 5, fill = !!sym(variable_col))) +
    geom_col(width = 20) +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(min_value, max_value),
      breaks = c(min_value, max_value),
      labels = c(sprintf("%.1f", min_value), sprintf("%.1f", max_value)),
      name = NULL
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",  # Move legend to bottom for each plot
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.title = element_blank()
    ) +
    guides(fill = guide_colorbar(barwidth = 3, barheight = 0.5))
}

# Create a plot list to store the plots
plot_list <- list()

# Loop through all columns (variables) excluding 'direction' and create a plot for each
for (variable_col in names(plot_df)[-1]) {  # Exclude 'direction' column
  plot_list[[variable_col]] <- create_circular_plot_wide(plot_df, variable_col)
}

# Optionally combine the plots into a single layout with patchwork (e.g., 7 columns)
combined_plot <- wrap_plots(plot_list, ncol = 6)
combined_plot
