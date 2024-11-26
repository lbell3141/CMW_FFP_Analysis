create_plot <- function(month_abbr, month_full, variable) {
  variable_col <- paste0(month_abbr, "_", variable)
  min_value <- min(plot_df[[variable_col]], na.rm = TRUE)
  max_value <- max(plot_df[[variable_col]], na.rm = TRUE)
  
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(4, 5)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "direction",
        y = 5,
        fill = variable_col  
      ),
      position = "dodge2"
    ) +
    geom_segment(
      aes_string(
        x = "direction",
        y = 5,
        xend = "direction", 
        yend = 5
      ),
      linetype = "dashed",
      color = "white"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis", 
      name = "", 
      breaks = c(min_value, max_value),
      labels = scales::label_number()(c(min_value, max_value))
    ) +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank(),  
      legend.position = "bottom",
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month_full) +
    guides(
      fill = guide_colorbar(barwidth = 4, barheight = 0.5)
    )
}

# Generate plots using your Map function
plots <- Map(create_plot, month_abbr, month_full, MoreArgs = list(variable = variable))

# Combine plots into a single frame
final_plot <- wrap_plots(plots, ncol = 6) & 
  theme(legend.position = "bottom")

final_plot + 
  plot_annotation(title = "NDVI")
