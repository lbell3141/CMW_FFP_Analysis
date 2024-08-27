#circular plot for just average monthly gpp
#df from driver_circles.R
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(viridis)


#=============misc work reformatting dfs / adding dfs together from saved/loaded data
month_cc_df <- readRDS("./Data/monthly_directional_ffps/raster_dataframes/month_cc.RDS")
plot_df <- dir_dat_avg[, c(1,2,7)]
plot_df <- plot_df %>%
  rename(month = mm)%>%
  rename(direction = dir_group)
plot_df <- full_join(plot_df, month_cc_df, by = c("direction", "month"))
#break into list
month_split <- split(plot_df, plot_df$month)
#add month abbreviations to all variable column names
month_abbr <- month.abb

#loop through and rename
month_split <- lapply(1:12, function(i) {
  df <- month_split[[i]]
  
  #create new column names for variables, but don't rename direction or month column
  new_colnames <- names(df)
  new_colnames <- ifelse(new_colnames != c("month", "direction"), 
                         paste0(month_abbr[i], "_", new_colnames),
                         new_colnames)
  
  #apply names
  df <- df %>%
    rename_with(~ new_colnames)
  
  return(df)
})

#remove month column
for (i in seq_along(month_split)){
  month_split[[i]] <- month_split[[i]] %>%
    ungroup() %>%
    select(-month)
}

#combine dataframes in list to make a single plotting frame
plot_df <- reduce(month_split, full_join, by = "direction")
#good above 
#combine geospatial df with plot frame
colnames(mm_dat) <- str_replace_all(colnames(mm_dat), "^\\w{3}", str_to_title)
mm_dat <- mm_dat %>%
  rename(direction = Direction)
plot_df <- full_join(plot_df, mm_dat, by = "direction")


#run here

#use reshape package: convert to long columns instead of rows
long_plot_df <- melt(plot_df, id.vars = "direction", 
                     measure.vars = c(paste0(month_abbr, "_gpp"), paste0(month_abbr, "_Cover")),
                     variable.name = "Month_Variable", value.name = "Value")

#individual columns for month and variable values
long_plot_df$Month <- gsub("_.*", "", long_plot_df$Month_Variable)
long_plot_df$Variable <- gsub(".*_", "", long_plot_df$Month_Variable)

#making plots (same code, but legends removed in order to add a common legend back in)
create_plot <- function(month_abbr, month_full, variable) {
  variable_col <- paste0(month_abbr, "_", variable)
  canopy_cover_col <- paste0(month_abbr, "_Cover")
  
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0, 0.8)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "direction",
        y = canopy_cover_col,
        fill = variable_col  
      ),
      position = "dodge2"
    ) +
    geom_segment(
      aes_string(
        x = "direction",
        y = canopy_cover_col,
        xend = "direction", 
        end = 0.8 
      ),
      linetype = "dashed",
      color = "gray12"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(option = "viridis", name = "GPP", limits = c(min_value, max_value)) +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_text(color = "gray12", size = 12),  
      legend.position = "none",  # Remove individual legends
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month_full) 
}

#define abbr for pulling columns
#full month names for individual plot titles
month_abbr <- month.abb
month_full <- month.name

#change variable depending on target plot
variable <- "gpp"

#define min and max for common legend using name string to define
min_value <- min(sapply(month_abbr, function(m) min(plot_df[[paste0(m, "_", variable)]])))
max_value <- max(sapply(month_abbr, function(m) max(plot_df[[paste0(m, "_", variable)]])))

#create plot obj with all 12 plots looped through 
plots <- Map(create_plot, month_abbr, month_full, MoreArgs = list(variable = variable))

#example plot for defining legend
legend_plot <- ggplot(plot_df) +
  geom_col(
    aes_string(x = "direction", y = paste0(month_abbr[1], "_Cover"), fill = paste0(month_abbr[1], "_", variable)),
    position = "dodge2"
  ) +
  scale_fill_viridis_c(option = "viridis", name = "GPP", limits = c(min_value, max_value)) +
  theme_void() +
  theme(legend.position = "bottom")

#add legend, define layout; display final plot
final_plot <- wrap_plots(plots, ncol = 6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
final_plot

#=================common legend for NDVI
#circular plot for just average monthly ndvi
#combine geospatial df with plot frame
colnames(mm_dat) <- str_replace_all(colnames(mm_dat), "^\\w{3}", str_to_title)
mm_dat <- mm_dat%>%
  rename(direction = Direction)
plot_df <- mm_dat

#use reshape package: convert to long columns instead of rows
long_plot_df <- melt(plot_df, id.vars = "direction", 
                     measure.vars = c(paste0(month_abbr, "_NDVI"), paste0(month_abbr, "_CHM")),
                     variable.name = "Month_Variable", value.name = "Value")

#individual columns for month and variable values
long_plot_df$Month <- gsub("_.*", "", long_plot_df$Month_Variable)
long_plot_df$Variable <- gsub(".*_", "", long_plot_df$Month_Variable)

#making plots (same code, but legends removed in order to add a common legend back in)
create_plot <- function(month_abbr, month_full, variable) {
  variable_col <- paste0(month_abbr, "_", variable)
  canopy_cover_col <- paste0(month_abbr, "_CHM")
  
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0, 7.5)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "direction",
        y = canopy_cover_col,
        fill = variable_col  
      ),
      position = "dodge2"
    ) +
    geom_segment(
      aes_string(
        x = "direction",
        y = canopy_cover_col,
        xend = "direction", 
        end = 7.5 
      ),
      linetype = "dashed",
      color = "gray12"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(option = "viridis", name = "NDVI", limits = c(min_value, max_value)) +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_text(color = "gray12", size = 12),  
      legend.position = "none",  # Remove individual legends
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month_full) 
}

#define abbr for pulling columns
#full month names for individual plot titles
month_abbr <- month.abb
month_full <- month.name

#change variable depending on target plot
variable <- "NDVI"

#define min and max for common legend using name string to define
min_value <- min(sapply(month_abbr, function(m) min(plot_df[[paste0(m, "_", variable)]])))
max_value <- max(sapply(month_abbr, function(m) max(plot_df[[paste0(m, "_", variable)]])))

#create plot obj with all 12 plots looped through 
plots <- Map(create_plot, month_abbr, month_full, MoreArgs = list(variable = variable))

#example plot for defining legend
legend_plot <- ggplot(plot_df) +
  geom_col(
    aes_string(x = "direction", y = paste0(month_abbr[1], "_CHM"), fill = paste0(month_abbr[1], "_", variable)),
    position = "dodge2"
  ) +
  scale_fill_viridis_c(option = "viridis", name = "NDVI", limits = c(min_value, max_value)) +
  theme_void() +
  theme(legend.position = "bottom")

#add legend, define layout; display final plot
final_plot <- wrap_plots(plots, ncol = 6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
final_plot
