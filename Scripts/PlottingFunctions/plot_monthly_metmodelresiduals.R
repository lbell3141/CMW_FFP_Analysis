plot_monthly_metmodel_residuals <- function(monthly_prediction_list) {
  library(ggplot2)
  library(patchwork)

  # Calculate global residual range for color scaling
  all_resids <- unlist(lapply(monthly_prediction_list, \(df) if(!is.null(df)) df$diff_avg_gpp))
  overall_min <- min(all_resids, na.rm = TRUE)
  overall_max <- max(all_resids, na.rm = TRUE)
  
  # Function to plot one month
  plot_gpp_difference <- function(df, month_name) {
    if (is.null(df)) return(NULL)
    ggplot(df, aes(x = factor(direction, levels = seq(0, 340, by = 20)),
                   y = 5, fill = diff_avg_gpp)) +
      geom_col(width = 1) +
      coord_polar() +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0,
        limits = c(overall_min, overall_max),
        name = "Residual GPP"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom"
      ) +
      labs(title = month_name)
  }
  
  # Make plots for all months
  plot_list <- Map(plot_gpp_difference, monthly_prediction_list, names(monthly_prediction_list))

  # Combine into one figure
  final_plot <- (
    wrap_plots(plot_list, ncol = min(6, length(plot_list))) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  ) + 
    plot_annotation(title = "Directional Residual GPP (Predicted − Observed)")
  
  return(final_plot)
}
