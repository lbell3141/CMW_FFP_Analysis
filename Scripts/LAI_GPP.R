#calculating avergae LAI around the tower and scaling it to associated directional GPP flux

library(terra)
library(lubridate)
library(dplyr)
library(plotrix)
library(ggplot2)
#load function in Contours_to_Mask.R, lns 107-167
#load LAI and flux data; load seasonal footrpint contours obj

lai <- rast("./Data/LiDAR/LAI3mrepro.tif")
#flux_dat <- read from calc_directional_ffp.R lns 93-135
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
  rap_ffp_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], lai)
}

#convert to df with correct WD
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]

rap_ffp_df <- data.frame(direction = deg_int_real,
                         lai = unlist(rap_ffp_list)
)

avg_gpp <- avg_gpp%>%
  rename(direction = dir_group)
combd_df <- merge(rap_ffp_df, avg_gpp, by = "direction")
combd_df <- combd_df %>%
  mutate(p_cap = avg_gpp * lai)
plot(combd_df$direction, combd_df$p_cap)
plot(combd_df$lai,combd_df$avg_gpp)
#===========
#LiDAR flown in July 2021, so instead of all GPP, just avg july GPP, then avg july 2021 GPP
dat <- read.csv("./Data/combined_CMdata.csv", header = T)
dat$TIMESTAMP_END <- ymd_hms(as.character(dat$TIMESTAMP_END))

#format df with same restrictions used in ffp calcs
meas_h <- 14
d <- (2/3) * meas_h

CM_dat <- dat %>%
  mutate(
    yyyy = year(TIMESTAMP_END),
    mm = month(TIMESTAMP_END),
    day = day(TIMESTAMP_END),
    HH_UTC = hour(TIMESTAMP_END),
    MM = minute(TIMESTAMP_END),
    zm = meas_h - d,
    ol = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    wind_dir = WD_1_1_1,
    test = zm/ol
  ) %>%
  #filter(test >= -15.5)%>%
  #USTAR < 0.2 is already gapfilled in for GPP (FC = raw carbon flux not corrected)
  #filter(USTAR > 0.2)%>%
  dplyr::select(yyyy, mm, day, HH_UTC, MM, wind_dir, GPP_PI)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)%>%
  filter(mm == 7)%>%
  filter(yyyy == 2021)

#split into separate wind directions bins of 10 degrees
deg_int <- seq(0, 360, by = 20)
#skip <- 50
#deg_int_real <- deg_int[!deg_int %in% skip]
labels <- deg_int[-1]
split_dat <- split(CM_dat, cut(CM_dat$wind_dir, deg_int, include.lowest = TRUE, labels = labels))
for (i in seq_along(split_dat)) {
  split_dat[[i]]$dir_group <- rep(names(split_dat)[i], nrow(split_dat[[i]]))
}
dir_dat <- do.call(rbind, split_dat)

#avg gpp by group
avg_gpp <- dir_dat %>%
  group_by(dir_group) %>%
  summarize(
    avg_gpp = mean(GPP_PI, na.rm = T)
  )

#======
avg_gpp <- avg_gpp%>%
  rename(direction = dir_group)
combd_df <- merge(rap_ffp_df, avg_gpp, by = "direction")
combd_df <- combd_df %>%
  mutate(p_cap = avg_gpp * lai,
         norm_gpp = avg_gpp / lai)
plot(combd_df$direction, combd_df$p_cap)
plot(combd_df$lai,combd_df$avg_gpp)
#====================
#err_pcap <- std.error(combd_df$p_cap)
ggplot(combd_df) + 
  
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = 9),
    color = "lightgrey"
  )+
  
  geom_col(
    aes(
      x = direction,
      y = 8,
      fill = norm_gpp
    ),
    position = "dodge2",
    show.legend = T,
    alpha = 0.9)+
  
  geom_segment(
    aes(
      x = direction,
      y = 8,
      xend = direction,
      yend = 8
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  
  coord_polar()+
  
  #geom_errorbar(aes(x = direction,
 #                   ymin = p_cap - bar_error,
  #                  ymax = p_cap + bar_error),
  #              width = 5)+
  
  scale_fill_viridis_c(option = "viridis", 
                       name = "LAI-Normalized GPP") +
  scale_x_continuous(
    breaks = combd_df$direction,
    labels = combd_df$direction
  ) +
  theme(
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.y = element_blank(), 
    axis.text.x = element_text(),  
    legend.position = "bottom",
    text = element_text(color = "gray12", family = "Bell MT"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "July 2021")+
  guides(
    fill = guide_colorbar(barwidth = 8, barheight = 0.8)
  )
