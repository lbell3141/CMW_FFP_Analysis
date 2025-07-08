library(tidyverse)
library(viridis)
library(patchwork)

# ---- Setup variables and months ----
variables <- c("residual_gpp_z", "LAI_z", "Cover_z", "Height_z", "TWI_z")
variable_labels <- c("GPP\nResidual", "LAI", "Cover", "Height", "TWI")
months_abbr <- c("Jun", "Jul", "Aug", "Sep")
months_full <- c("June", "July", "August", "September")

# ---- Filter to summer months ----
dir_dat_avg <- geos_zscores %>%
  filter(month %in% months_abbr)

# ---- Function to create circular directional plots ----
create_circular_plot <- function(df, var, month_abbr) {
  df_month <- df %>% filter(month == month_abbr)
  
  min_val <- min(df_month[[var]], na.rm = TRUE)
  max_val <- max(df_month[[var]], na.rm = TRUE)
  
  ggplot(df_month, aes(x = direction, y = 15, fill = !!sym(var))) +
    geom_col(width = 20) +
    coord_polar() +
    scale_fill_viridis_c(
      option = "viridis",
      limits = c(min_val, max_val),
      breaks = c(min_val, max_val),
      labels = function(x) sprintf("%.1f", x),
      name = NULL  # No legend title
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    ) +
    guides(fill = guide_colorbar(barwidth = 2.5, barheight = 0.8))
}

# ---- Create aligned title row with month names ----
month_title_labels <- map(months_full, ~ {
  ggplot() + 
    annotate("text", x = 1, y = 1, label = .x, size = 7, fontface = "bold") +
    theme_void()
})

# Left label spacer with fixed width
left_spacer <- ggplot() +
  theme_void()

month_title_row <- wrap_plots(c(list(left_spacer), month_title_labels),
                              nrow = 1, widths = c(4, rep(3, length(months_abbr))))

# ---- Create plot matrix (rows = variables, columns = months) ----
all_rows <- list()

for (i in seq_along(variables)) {
  var <- variables[i]
  var_label <- variable_labels[i]
  
  plots_row <- map(months_abbr, ~ 
                     create_circular_plot(dir_dat_avg, var, .x)
  )
  
  label_plot <- ggplot() + 
    annotate("text", x = 1.45, y = 1, label = var_label, size = 7, fontface = "bold", hjust = 1) +
    xlim(0, 3) +  # ensures enough space
    theme_void()
  
  row_plot <- wrap_plots(c(list(label_plot), plots_row), nrow = 1, widths = c(4, rep(3, length(plots_row))))
  all_rows[[i]] <- row_plot
}

# ---- Combine title and variable rows ----
final_plot <- wrap_plots(c(list(month_title_row), all_rows), ncol = 1, heights = c(0.6, rep(3, length(all_rows)))) +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )

# ---- Display
print(final_plot)
