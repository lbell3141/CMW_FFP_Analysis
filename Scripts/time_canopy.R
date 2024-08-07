#GPP ~ canopy cover + time of year
#for 2017:2021 comparing productivity by direction for key times of year
#using circular plots

library(lubridate)
library(dplyr)
library(terra)
library(ggplot2)

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

#make dataframes for dry mature, wet mature, and dormant phenophases
Mar_df <- dir_dat %>%
  filter(yyyy %in% 2015:2019)%>%
  filter(mm == 3)%>%
  group_by(dir_group)%>%
  summarize(avg_gpp_mar = mean(gpp, na.rm = T))%>%
  arrange(as.numeric(dir_group))
June_df <- dir_dat %>%
  filter(yyyy %in% 2015:2019)%>%
  filter(mm == 6)%>%
  group_by(dir_group)%>%
  summarize(avg_gpp_jun = mean(gpp, na.rm = T))%>%
  arrange(as.numeric(dir_group))
Aug_df <- dir_dat %>%
  filter(yyyy %in% 2015:2019)%>%
  filter(mm == 8)%>%
  group_by(dir_group)%>%
  summarize(avg_gpp_aug = mean(gpp, na.rm = T))%>%
  arrange(as.numeric(dir_group))
gpp_df <- left_join(Mar_df, June_df, by = "dir_group")
gpp_df <- left_join(gpp_df, Aug_df, by = "dir_group")
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
  mutate(canopy_cover = (80 - 100*canopy_cover))
#begin plotting:
#make custom grid
grid <- ggplot(plot_df) +  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0,20)), 
    color = "lightgrey"
  )

bar_plot <- grid +
  geom_col(
    aes(
      x = dir_group,
      y = canopy_cover,
      fill = avg_gpp_mar
    ),
    position = "dodge2",
    show.legend = F,
    alpha = 0.9
  )

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
