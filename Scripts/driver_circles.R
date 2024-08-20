#compare drivers of gpp by creating directional (circular) plots with driver values normalized by month
#drivers considered: atmos temp, rel humidity, VPD, latent heat, soil water, wind speed, ppfd

library(lubridate)
library(dplyr)
library(plantecophys)

library(ggplot2)
library(viridis)
library(patchwork)

#load flux data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#plotting normalized values, so create a function for zscores: 
calc_z_score <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

#create dataframe
dat_voi<- dat_file %>%
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
    VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17) %>%
  #group by month and calculate zscores (based on monthly values that is)
  group_by(mm) %>%
  mutate(
    wind_sp_mzs = calc_z_score(wind_sp),
    temp_atmos_mzs = calc_z_score(temp_atmos),
    rel_h_mzs = calc_z_score(rel_h),
    VPD_mzs = calc_z_score(VPD),
    ppfd_mzs = calc_z_score(ppfd),
    le_mzs = calc_z_score(le),
    swc_mzs = calc_z_score(swc)
  ) %>%
  ungroup()

#split into direction windows
deg_int <- seq(10, 350, by = 10)
deg_int_real <- deg_int
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, breaks = c(0, deg_int, 360), include.lowest = TRUE, labels = c(deg_int_real, 360)))
for (i in seq_along(split_dat)) {
  split_dat[[i]]$dir_group <- rep(names(split_dat)[i], nrow(split_dat[[i]]))
}
dir_dat <- do.call(rbind, split_dat)

#calc avg normalized value for each direction group in each month
#time averages aren't correct with this but we don't need for plotting so I'm not fixing it- just fyi
dir_dat_avg <- dir_dat %>%
  group_by(mm, dir_group)%>%
  summarise_all(~mean(.x, na.rm = T)) %>%
  mutate(dir_group = as.numeric(dir_group))%>%
  arrange(mm, dir_group)
dir_dat_avg = dir_dat_avg[,c(1:2,19:25)]

#=====================plotting=========================
#clean up plot_df
#add canopy cover by month 


#load function from time_canopy.R: 
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
