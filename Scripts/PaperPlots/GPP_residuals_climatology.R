library(tidyverse)
library(patchwork)
library(grid)
library(gridExtra)
library(viridis)
library(dplyr)

# --- 1. Climatology plot (unchanged) ---
climatology_plot <- ggplot(plot_dat, aes(x = doy)) +
  geom_ribbon(aes(ymin = avg_gpp - se_gpp, ymax = avg_gpp + se_gpp), fill = "grey", alpha = 0.4) +
  annotate("rect", xmin = 152, xmax = 273, ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.3) +
  geom_line(aes(y = avg_gpp), color = "black", size = 0.8) +
  labs(x = "Day of Year", y = "Average GPP", title = "") +
  theme_minimal(base_size = 14)

# --- 2. Prepare residual GPP plots ---

mm_mod_list <- lapply(mm_rf_results, \(x) x$summary)
mm_mod_list <- mm_mod_list[6:9]  # summer months: Jun-Sep

all_diff_avg_gpp <- unlist(lapply(mm_mod_list, function(df) df$diff_avg_gpp))
overall_min <- min(all_diff_avg_gpp, na.rm = TRUE)
overall_max <- max(all_diff_avg_gpp, na.rm = TRUE)

plot_gpp_difference <- function(df, month_num) {
  df <- df %>%
    mutate(direction = factor(direction, levels = seq(0, 340, by = 20)))
  
  ggplot(df) +
    geom_col(aes(x = direction, y = 5, fill = diff_avg_gpp), width = 1) +
    coord_polar() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(overall_min, overall_max),
      breaks = c(overall_min, 0, overall_max),
      labels = c(sprintf("%.2f", overall_min), "0", sprintf("%.2f", overall_max)),
      name = NULL
    ) +
    ylim(0, 7) +  # SAME fixed y-limits here too
    guides(fill = guide_colorbar(
      barwidth = 6, barheight = 1,
      title = "Residual GPP\n(Predicted - Observed)",
      title.position = "bottom",
      title.hjust = 0.5
    )) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 12, hjust = 0.5),
      text = element_text(color = "black"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    )
}


residual_plot_list <- Map(plot_gpp_difference, mm_mod_list, as.integer(names(mm_mod_list)))

# Remove individual legends, collect a shared one
residual_stack <- wrap_plots(residual_plot_list, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")  # suppress individual legends here

# Extract the residual legend for later
get_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg_index <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg_index]]
}

residual_legend <- get_legend(
  residual_plot_list[[1]] + theme(legend.position = "bottom")
)

# Combine residual plots + legend stacked vertically
residual_with_legend <- plot_spacer() / residual_stack / residual_legend +
  plot_layout(heights = c(0.02, 1, 0.1))

# --- 3. Prepare GPP circle plots ---

month_abbr <- c("Jun", "Jul", "Aug", "Sep")
month_full <- c("June", "July", "August", "September")

# Calculate global min and max for GPP across all months (to share scale and legend)
all_gpp_values <- unlist(lapply(month_abbr, function(m) plot_df[[paste0(m, "_gpp")]]))
gpp_min <- min(all_gpp_values, na.rm = TRUE)
gpp_max <- max(all_gpp_values, na.rm = TRUE)

# Shared fill scale for GPP plots
shared_gpp_scale <- scale_fill_viridis_c(
  option = "viridis",
  limits = c(gpp_min, gpp_max),
  breaks = c(gpp_min, gpp_max),
  labels = c(sprintf("%.1f", gpp_min), sprintf("%.1f", gpp_max)),
  name = "Average GPP",
  guide = guide_colorbar(
    title.position = "bottom",
    title.hjust = 0.5,
    barwidth = 4,   # adjust bar width if needed
    barheight = 1.2  # adjust bar height if needed
  )
)

create_monthly_gpp_plot <- function(month_abbr, month_full) {
  variable_col <- paste0(month_abbr, "_gpp")
  min_value <- min(plot_df[[variable_col]], na.rm = TRUE)
  max_value <- max(plot_df[[variable_col]], na.rm = TRUE)
  
  ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(variable_col))) +
    geom_col(width = 20) +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(min_value, max_value),
      breaks = c(min_value, max_value),
      labels = c(sprintf("%.1f", min_value), sprintf("%.1f", max_value)),
      name = "Average GPP",
      guide = guide_colorbar(title.position = "bottom", title.hjust = 0.5)
    ) +
    ylim(0, 7) +   # FIXED y limits to align circle radius
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10)
    )
}


gpp_plot_list <- Map(create_monthly_gpp_plot, month_abbr, month_full)

# Create dummy legend plot for GPP (uses shared scale)
legend_plot <- ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(paste0(month_abbr[1], "_gpp")))) +
  geom_col(width = 20) +
  coord_polar() +
  shared_gpp_scale +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )

# Extract the shared GPP legend grob
shared_legend <- get_legend(legend_plot)

# Stack GPP plots without legends
GPP_plot_stacked <- wrap_plots(gpp_plot_list, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# --- 4. Create left-side month label column ---

month_labels <- c("June", "July", "August", "September")

label_plots <- lapply(month_labels, function(label) {
  ggplot() +
    theme_void() +
    annotate("text", x = 0, y = 0, label = label, size = 6, fontface = "bold") +
    theme(plot.margin = ggplot2::margin(10, 0, 10, 0))
})

label_column <- wrap_plots(label_plots, ncol = 1)

# --- 5. Combine everything together ---

# Compose the four main components horizontally: 
# month labels | GPP plots | residual plots with legend | climatology plot
final_main <- label_column | GPP_plot_stacked | residual_with_legend | climatology_plot

# Now add the shared GPP legend below the GPP plot column manually with grid.arrange
# We'll create a layout with two rows:
# 1. the main combined plot (above)
# 2. the shared GPP legend (below, spanning only the GPP plot column width)

# Extract widths from the main combined plot
main_widths <- c(0.3, 0.5, 0.5, 1)

# For simplicity, arrange with grid.arrange:
grid.newpage()
grid.arrange(
  grobs = list(final_main),
  layout_matrix = rbind(c(1)),
  heights = unit(1, "null")
)

# Now manually draw the shared GPP legend below the GPP plot column
# If you want them truly stacked vertically with aligned widths, you can:
# 1) use patchwork's patchworkGrob to combine the main plot with legend
# 2) or arrange using grid.arrange with layout_matrix

# Example with patchwork (placing legend under GPP plots only):

final_with_legend <- (label_column | (GPP_plot_stacked / wrap_plots(shared_legend)) | residual_with_legend | climatology_plot) +
  plot_layout(widths = main_widths, heights = c(1, 0.1))

print(final_with_legend)
