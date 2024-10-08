#circular plot of canopy cover
#plotting tutorial: https://medium.com/@victorallan/create-stunning-circular-bar-plots-in-r-ggplot2-with-minimal-code-28e2aaf6fe36

library(terra)
library(ggplot2)

#===============================================================================
#==========================load raster and ffp data=============================
#===============================================================================

canopy_file <- "./Data/Image_classification/canopy.tif"
canopy <- rast(canopy_file)
#plot(canopy)

#load in function from Contours_to_Mask.R ln.107:167
ffp_list <- readRDS("./Data/calcd_ffp_list.rds")
x_list <- list()
y_list <- list()
for (i in seq_along(ffp_list)) {
  x_list[[i]] <- ffp_list[[i]]$xr
  y_list[[i]] <- ffp_list[[i]]$yr
}

#run code for ffp_contours_to_mask function in Countours_to_Mask.R
#apply function to lists and loaded RAP data:
rap_ffp_list <- list()
for (i in seq_along(x_list)) {
  rap_ffp_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], canopy)
}

#convert to df with correct WD
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]

rap_ffp_df <- data.frame(direction = deg_int_real,
                         veg_cover = unlist(rap_ffp_list)
)

#===============================================================================
#======================make circular plot of canopy cover========================
#===============================================================================
#prepare plot dataframe
start_deg <- seq(0,350,10)
start_skip <- 40
start_deg_real <- start_deg[!start_deg %in% skip]

plot_df <- rap_ffp_df %>%
  mutate(start = start_deg_real) %>%
  summarize(start = start,
            end = direction, 
            canopy_cover = veg_cover)



#begin plotting:
#make custom grid
grid <- ggplot(plot_df) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0,0.9)), 
    color = "lightgrey"
  )

bar_plot <- grid +
  geom_col(
    aes(
      x = end,
      y = canopy_cover,
      fill = canopy_cover
    ),
    position = "dodge2",
    show.legend = F,
    alpha = 0.9
  )

circ_plot <- bar_plot +
  geom_segment(
    aes(
      x = end,
      y = canopy_cover,
      xend = end, 
      yend = 0.85
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  coord_polar()
circ_plot



#additional aesthetic changes to be made (from tutorial)
with_aes <- circ_plot +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    ) # For for legend guide
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(), # Removing the x axis title
    axis.ticks = element_blank(), # Removing the x axis ticks
    axis.text.y = element_blank(), # Removing the y axis text
    # Use gray text for the Treatment names
    axis.text.x = element_text(color = "gray12", size = 12),  #Making the text grey to look asthetic
    # Move the legend to the bottom
    legend.position = "bottom",
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),
    
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )

with_aes
#=========redo gpp circle only
#dir_dat_avg from driver_circles.R
#prepare plot dataframe

plot_df <- dir_dat_avg%>%
  select(direction, month, gpp)

#begin plotting:
#make custom grid
create_plot <- function(month_abbr, month_full, variable) {
  col_name <- paste0(month_abbr, "_", variable)
  
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0, 16)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "direction",
        y = 16,
        fill = col_name
      ),
      position = "dodge2"
    ) +
    geom_segment(
      aes_string(
        x = "direction",
        y = 16,
        xend = "direction", 
        yend = 16
      ),
      linetype = "dashed",
      color = "gray12"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(option = "viridis", name = "", breaks = pretty(plot_df[[col_name]], n = 5)) +
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
    labs(title = month_full)+
    guides(
      fill = guide_colorbar(barwidth = 8, barheight = 0.5)
    )
}
month_abbr <- month.abb
month_full <- month.name

# Define the variable suffix (e.g., 'gpp' in Jan_gpp, Feb_gpp, etc.)
variable <- "gpp"

# Create individual plots with loop
plots <- Map(create_plot, month_abbr, month_full, MoreArgs = list(variable = variable))

# Combine plots into a single frame
final_plot <- wrap_plots(plots, ncol = 6) & 
  theme(legend.position = "bottom")

final_plot + 
  plot_annotation(title = "Average Directional GPP")
