plot_gpp_difference_with_legend <- function(df, month_num) {
  df <- df %>%
    mutate(direction = factor(direction, levels = seq(0, 340, by = 20)))
  
  # Calculate min and max residual for this month
  min_value <- min(df$diff_avg_gpp, na.rm = TRUE)
  max_value <- max(df$diff_avg_gpp, na.rm = TRUE)
  
  ggplot(df) +
    geom_col(aes(x = direction, y = 15, fill = diff_avg_gpp), width = 1) +
    coord_polar() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(min_value, max_value),
      breaks = c(min_value, max_value),
      labels = c(sprintf("%.2f", min_value), sprintf("%.2f", max_value)),
      name = NULL,  # no legend title
      guide = guide_colorbar(
        title = NULL,
        barwidth = 3,
        barheight = 0.5,
        ticks.colour = "black",
        label.position = "bottom"
      )
    ) +
    ylim(0, 15) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.background = element_blank(),
      legend.text = element_text(size = 12)
    )
}
residual_plot_list <- Map(plot_gpp_difference_with_legend, mm_mod_list, as.integer(names(mm_mod_list)))
stacked_residual_plots <- wrap_plots(residual_plot_list, ncol = 1)

# Optional: add title above the column
final_residual_plot <- stacked_residual_plots #+ plot_annotation(
#   title = "Residual GPP",
#   theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
# )

print(final_residual_plot)
global_min <- min(all_diff_avg_gpp, na.rm = TRUE)
global_max <- max(all_diff_avg_gpp, na.rm = TRUE)
scale_fill_gradient2(
  low = "blue", mid = "white", high = "red",
  midpoint = 0,
  limits = c(global_min, global_max),
  breaks = c(global_min, global_max),
  labels = c(sprintf("%.2f", global_min), sprintf("%.2f", global_max)),
  name = NULL,
  guide = guide_colorbar(...)
)
