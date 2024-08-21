#GPP ~ canopy cover + time of year
#for 2017:2021 comparing productivity by direction for key times of year
#using circular plots

library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(terra)
library(ggplot2)
library(viridis)
library(patchwork)
#===============================================================================
#=================================load flux data================================
#===============================================================================

dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#create dataframe
dat_voi <- dat_file %>%
  # Ensure TIMESTAMP_START is in the correct date-time format
  mutate(TIMESTAMP_START = ymd_hms(TIMESTAMP_START)) %>%
  # Extract components
  summarize(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
  ) %>%
  # Filter for valid (non-NA) values and the specified hour range
  filter(if_any(everything(), ~ !is.na(.))) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

#split into direction windows
deg_int <- seq(10, 350, by = 10)
deg_int_real <- deg_int
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, breaks = c(0, deg_int, 360), include.lowest = TRUE, labels = c(deg_int_real, 360)))
for (i in seq_along(split_dat)) {
  split_dat[[i]]$dir_group <- rep(names(split_dat)[i], nrow(split_dat[[i]]))
}
dir_dat <- do.call(rbind, split_dat)

#make a df for avg monthly gpp
#prep to make a list of month dfs to loop through
months <- seq(1,12,1)
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
#empty list for dfs
gpp_list <- list()

#loop through each month number; 
for (i in seq_along(months)) {
  month <- months[i]
  month_name <- month_names[i]
  
  #calc avg gpp by month (across 5 years) by direction
  temporary_df <- dir_dat %>%
    filter(yyyy %in% 2015:2019) %>%
    filter(mm == month) %>%
    group_by(dir_group) %>%
    summarize(avg_gpp = mean(gpp, na.rm = TRUE)) %>%
    arrange(as.numeric(dir_group))
  
  #add month name to df
  colnames(temporary_df)[colnames(temporary_df) == "avg_gpp"] <- paste0("avg_gpp_", month_name)
  
  #add temp dfs to list 
  gpp_list[[i]] <- temporary_df
}

#stitch dfs from list together; make directions numeric to avoid plotting issues
gpp_df <- reduce(gpp_list, left_join, by = "dir_group")
gpp_df <- gpp_df %>%
  mutate(dir_group = as.numeric(dir_group))
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
                         canopy_cover = unlist(rap_ffp_list))
rap_ffp_df <- rap_ffp_df%>%
  rename(dir_group = direction)%>%
  mutate(dir_group = as.numeric(dir_group))

#===============================================================================
#======================make circular plot of canopy cover========================
#===============================================================================
#prepare plot dataframe; combine gpp and canopy cover
plot_df <- left_join(rap_ffp_df, gpp_df, by = "dir_group")
plot_df <- plot_df %>%
  mutate(canopy_cover = (100*canopy_cover - 55))
#begin plotting:
variable_fill = plot_df$avg_gpp_Mar
name = "March GPP"
#make custom grid
grid <- ggplot(plot_df) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0,20)), 
    color = "lightgrey"
  )

#make standard bar plot before wrapping in to a circle
bar_plot <- grid +
  geom_col(
    aes(
      x = dir_group,
      y = canopy_cover,
      fill = variable_fill
    ),
    position = "dodge2",
    show.legend = T,
    #alpha = 0.9
  )

#wrap bar plot into circle
circ_plot <- bar_plot +
  geom_segment(
    aes(
      x = dir_group,
      y = canopy_cover,
      xend = dir_group, 
      yend = 20
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  coord_polar()
#circ_plot

#additional aesthetic changes to be made (from tutorial)
with_aes <- circ_plot +
  scale_fill_viridis_c(option = "viridis",
                       name = name) +
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    ) # For for legend guide
  ) +
  theme(
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.y = element_blank(), 
    axis.text.x = element_text(color = "gray12", size = 12),  
    legend.position = "bottom",
    text = element_text(color = "gray12", family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )

with_aes

#===============================================================================
#=================================plotting loop=================================
#===============================================================================
#create a function for making circle plots 
create_plot <- function(month_col, month_name) {
  ggplot(plot_df) +  
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0, 20)), 
      color = "lightgrey"
    ) +
    geom_col(
      aes_string(
        x = "dir_group",
        y = "canopy_cover",
        fill = month_col  
      ),
      position = "dodge2",
      show.legend = T
    ) +
    geom_segment(
      aes(
        x = dir_group,
        y = canopy_cover,
        xend = dir_group, 
        yend = 20
      ),
      linetype = "dashed",
      color = "gray12"
    ) + 
    coord_polar() +
    scale_fill_viridis_c(option = "viridis", name = "GPP") +
    theme(
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_text(color = "gray12", size = 12),  
      legend.position = "bottom",
      text = element_text(color = "gray12", family = "Bell MT"),
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    labs(title = month_name) 
}

#make a list of column names and real month names (for plotting below)
month_columns <- paste0("avg_gpp_", month.abb)
month_names <- month.name 

#use plotting function and generated month names to create a list of plots
plots <- Map(create_plot, month_columns, month_names)
#use patchwork::wrap_plots to put plots in same window
final_plot <- wrap_plots(plots, ncol = 6) 


#===============================================================================
#==========================amending gapfilled values============================
#===============================================================================
#load dir_dat from above
gf_dat <- dir_dat %>%
  mutate(FLAG = if_else(u_star < 0.2, 1, 0)) %>%
  filter(FLAG == 1)

gf_dat_test <- dir_dat %>%
  mutate(FLAG = if_else(u_star < 0.2, 1, 0)) %>%
  filter(FLAG == 1)%>%
  filter(dir_group ==10)
ggplot(data = gf_dat_test, mapping = aes(x= dir_group, y = gpp)) +
  geom_boxplot()

real_dat <- dir_dat %>%
  mutate(FLAG = if_else(u_star < 0.2,1,0))%>%
  filter(FLAG == 0)
dir_dat = real_dat
