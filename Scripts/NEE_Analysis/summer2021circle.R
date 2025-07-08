library(tidyverse)
library(viridis)
library(patchwork)

# ---- Step 1: Average LAI by month and direction from your df
dir_dat_avg <- geos_zscores %>%
  group_by(month, direction) %>%
  summarise(LAI_z = mean(LAI_z, na.rm = TRUE), .groups = "drop")

# ---- Step 2: Filter to summer months
summer_months <- c("Jun", "Jul", "Aug", "Sep")
dir_dat_avg <- dir_dat_avg %>%
  filter(month %in% summer_months)

# ---- Step 3: Split and reshape
month_abbr <- unique(dir_dat_avg$month)  # Should be like "Jun", "Jul", etc.
month_split <- split(dir_dat_avg, dir_dat_avg$month)

# Rename columns to have month prefix (e.g., "Jul_LAI")
month_split <- map2(month_split, month_abbr, function(df, abbr) {
  df %>%
    rename_with(~ ifelse(.x == "LAI_z", paste0(abbr, "_LAI_z"), .x)) %>%
    select(-month)
})

# ---- Step 4: Merge back into a single plot_df
plot_df <- reduce(month_split, full_join, by = "direction")

# ---- Step 5: Define function to plot one circular LAI plot
create_monthly_lai_plot <- function(month_abbr, month_full) {
  variable_col <- paste0(month_abbr, "_LAI_z")
  
  min_val <- min(plot_df[[variable_col]], na.rm = TRUE)
  max_val <- max(plot_df[[variable_col]], na.rm = TRUE)
  
  ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(variable_col))) +
    geom_col(width = 20) +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(min_val, max_val),
      breaks = c(min_val, max_val),
      labels = function(x) sprintf("%.1f", x),
      name = "LAI_z"
    ) +
    labs(title = month_full) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = 11),
      legend.position = "bottom"
    ) +
    guides(fill = guide_colorbar(barwidth = 3, barheight = 0.5))
}

# ---- Step 6: Generate plots
month_full <- c("June", "July", "August", "September")
month_abbr <- intersect(c("Jun", "Jul", "Aug", "Sep"), unique(dir_dat_avg$month))
month_full <- month_full[match(month_abbr, c("Jun", "Jul", "Aug", "Sep"))]

lai_plots <- Map(create_monthly_lai_plot, month_abbr, month_full)

# ---- Step 7: Combine all into one figure
final_plot <- wrap_plots(lai_plots, ncol = 4) & 
  theme(legend.position = "bottom")

LAI_z_plot <- final_plot + 
  plot_annotation(title = "Average Directional LAI (m²/m²)", 
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))

# ---- Step 8: Display
print(LAI_z_plot)
#===============================================================================
#===============================================================================
#===============================================================================
# ---- Step 1: Average LAI by month and direction from your df
dir_dat_avg <- geos_zscores %>%
  group_by(month, direction) %>%
  summarise(residual_gpp_z = mean(residual_gpp_z, na.rm = TRUE), .groups = "drop")

# ---- Step 2: Filter to summer months
summer_months <- c("Jun", "Jul", "Aug", "Sep")
dir_dat_avg <- dir_dat_avg %>%
  filter(month %in% summer_months)

# ---- Step 3: Split and reshape
month_abbr <- unique(dir_dat_avg$month)  # Should be like "Jun", "Jul", etc.
month_split <- split(dir_dat_avg, dir_dat_avg$month)

# Rename columns to have month prefix (e.g., "Jul_LAI")
month_split <- map2(month_split, month_abbr, function(df, abbr) {
  df %>%
    rename_with(~ ifelse(.x == "residual_gpp_z", paste0(abbr, "_residual_gpp_z"), .x)) %>%
    select(-month)
})

# ---- Step 4: Merge back into a single plot_df
plot_df <- reduce(month_split, full_join, by = "direction")

# ---- Step 5: Define function to plot one circular LAI plot
create_monthly_lai_plot <- function(month_abbr, month_full) {
  variable_col <- paste0(month_abbr, "_residual_gpp_z")
  
  min_val <- min(plot_df[[variable_col]], na.rm = TRUE)
  max_val <- max(plot_df[[variable_col]], na.rm = TRUE)
  
  ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(variable_col))) +
    geom_col(width = 20) +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(min_val, max_val),
      breaks = c(min_val, max_val),
      labels = function(x) sprintf("%.1f", x),
      name = "residual_gpp_z"
    ) +
    labs(title = month_full) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.title.position = "top",
      legend.text = element_text(size = 11),
      legend.position = "bottom"
    ) +
    guides(fill = guide_colorbar(barwidth = 3, barheight = 0.5))
}

# ---- Step 6: Generate plots
month_full <- c("June", "July", "August", "September")
month_abbr <- intersect(c("Jun", "Jul", "Aug", "Sep"), unique(dir_dat_avg$month))
month_full <- month_full[match(month_abbr, c("Jun", "Jul", "Aug", "Sep"))]

lai_plots <- Map(create_monthly_lai_plot, month_abbr, month_full)

# ---- Step 7: Combine all into one figure
final_plot <- wrap_plots(lai_plots, ncol = 4) & 
  theme(legend.position = "bottom")

Residual_z_plot <- final_plot + 
  plot_annotation(title = "Average Directional GPP Residual", 
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))

# ---- Step 8: Display
print(Residual_z_plot)

(LAI_plot | Cover_plot) /
(Height_plot | Residual_plot)

(LAI_z_plot) /
  (Cover_z_plot) /
  (Height_plot)/
  (Residual_z_plot)
