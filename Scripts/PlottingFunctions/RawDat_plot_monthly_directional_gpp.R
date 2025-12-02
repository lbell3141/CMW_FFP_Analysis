

plot_monthly_directional_gpp <- function(dir_dat_avg) {
  # Load necessary packages
  library(dplyr)
  library(lubridate)
  library(plantecophys)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  
  # Prep dataframe from plotting
  month_split <- split(dir_dat_avg, dir_dat_avg$mm)
  month_abbr <- month.abb[1:12]
  
  month_split <- map2(month_split, month_abbr, function(df, abbr) {
    df %>%
      rename_with(~ ifelse(.x %in% c("mm", "direction"), .x, paste0(abbr, "_", .x))) %>%
      select(-mm)
  })
  
  plot_df <- reduce(month_split, full_join, by = "direction")
  plot_df <- plot_df[1:18, ]
  
  #Loop through and create monthly plots
  create_monthly_gpp_plot <- function(month_abbr, month_full) {
    variable_col <- paste0(month_abbr, "_gpp")
    
    min_value <- min(plot_df[[variable_col]], na.rm = TRUE)
    max_value <- max(plot_df[[variable_col]], na.rm = TRUE)
    
    ggplot(plot_df, aes(x = direction, y = 5, fill = !!sym(variable_col))) +
      geom_col(width = 20) +
      coord_polar() +
      scale_fill_viridis_c(option = "viridis", limits = c(min_value, max_value),
                           breaks = c(min_value, max_value),
                           labels = c(sprintf("%.1f", min_value), sprintf("%.1f", max_value)),
                           name = NULL) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)
      ) +
      labs(title = month_full) +
      guides(fill = guide_colorbar(barwidth = 3, barheight = 0.5))
  }
  
  # Apply loop
  month_full <- month.name[1:12]
  plots <- Map(create_monthly_gpp_plot, month_abbr, month_full)
  
  final_plot <- wrap_plots(plots, ncol = 6) &
    theme(legend.position = "bottom")
  
  GPP_plot <- final_plot + plot_annotation(title = "Average Directional GPP (µmol CO₂ m⁻² s⁻¹)")
  return(GPP_plot)
}
