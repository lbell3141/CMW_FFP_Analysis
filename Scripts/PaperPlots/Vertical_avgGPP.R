#average gpp stack 

library(tidyverse)
library(patchwork)
library(viridis)

# Assume plot_df is your data frame and direction is factor/continuous variable
# month_abbr <- c("Jun", "Jul", "Aug", "Sep")

create_monthly_gpp_plot <- function(month_abbr) {
  variable_col <- paste0(month_abbr, "_gpp")
  
  min_value <- min(plot_df[[variable_col]], na.rm = TRUE)
  max_value <- max(plot_df[[variable_col]], na.rm = TRUE)
  
  ggplot(plot_df, aes(x = direction, y = 15, fill = !!sym(variable_col))) +
    geom_col(width = 20) +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(min_value, max_value),
      breaks = c(min_value, max_value),
      labels = c(sprintf("%.1f", min_value), sprintf("%.1f", max_value)),
      name = NULL,  # No title on legend
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
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.text = element_text(size = 12)
    )
}

# Create list of plots (each includes its own legend)
gpp_plot_list <- lapply(month_abbr, create_monthly_gpp_plot)

# Stack them vertically (ncol=1)
stacked_gpp_plots <- wrap_plots(gpp_plot_list, ncol = 1)

# Add title above entire column
final_plot <- stacked_gpp_plots  
  #plot_annotation(
  #title = "Average GPP",
  #theme = theme(plot.title = element_text(size = 14, hjust = 0.5)))

print(final_plot)
