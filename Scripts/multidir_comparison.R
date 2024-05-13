#Multidirectional productivity comparison

library(lubridate)
library(dplyr)
library(plantecophys)
library(ggplot2)
library(zoo)
library(gghighlight)

dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

#use library plantecophys to calc VPD
dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)

#create dataframe
dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    wind_sp = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    temp_atmos = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
    precip = P,
    rel_h = RH_1_1_1,
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.2)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0) 

#split into 8 wind directions
deg_int <- seq(0, 360, by = 45)
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- c("0-45", "45-90","90-135","135-180","180-225", "225-270", "270-315", "315-360") 
dir_colors <- c("lightgrey","lightgrey", "red", "maroon", "lightgrey", "lightgrey", "blue", "skyblue", "lightgrey", "lightgrey")

#arrange by doy and calculate moving average (k = window parameter)
for (i in seq_along(split_dat)) {
  split_dat[[i]] <- split_dat[[i]] %>% 
    arrange(doy) %>%
    mutate(wind_direction = dir_names[i])  # Add a column to identify wind direction category
  split_dat[[i]]$movavg <- rollmean(split_dat[[i]]$gpp, k = 500, fill = NA, align = "center")
}

#make plot obj
plot <- ggplot()

for (i in seq_along(split_dat)) {
  plot <- plot + 
    geom_line(data = split_dat[[i]], aes(x = doy, y = movavg, color = wind_direction),  linewidth = 1.1)  
  }

#adding legend
plot <- plot + 
  scale_color_manual(values = dir_colors, 
                     breaks = dir_names, 
                     labels = dir_names) +
  labs(x = "Day of Year", y = "GPP (µmolCO2 m-2 s-1)", main = "Annual GGP Flux by Wind Direction", color = "Wind Direction (Degrees)") +
  theme_minimal()

print(plot)


#=====================four directions===========================================
deg_int <- seq(0, 360, by = 90)
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- c("0-90","90-180","180-270", "270-360") 

#arrange by doy and calculate rolling mean (k = window parameter)
for (i in seq_along(split_dat)) {
  split_dat[[i]] <- split_dat[[i]] %>% 
    arrange(doy) %>%
    mutate(wind_direction = dir_names[i]) 
  split_dat[[i]]$movavg <- rollmean(split_dat[[i]]$gpp, k = 500, fill = NA, align = "center")
}

#make plot obj
plot <- ggplot()

for (i in seq_along(split_dat)) {
  plot <- plot + 
    geom_line(data = split_dat[[i]], aes(x = doy, y = movavg, color = wind_direction)) 
}

#adding legend
plot <- plot + 
  scale_color_manual(values = rainbow(length(split_dat)), 
                     breaks = dir_names, 
                     labels = dir_names) +
  theme_minimal()

print(plot)

#===========================moving window=======================================
#create moving window for wind directions
#window names used for ID later
#window medians to be used later in plotting
window_size <- 5
window_list <- list()
window_names <- character(length(window_list))
window_median <- numeric(length(window_list))

for (i in 1:360) {
  start <- (i - 1) %% 360
  end <- (start + window_size -1) %%360
  window_list[[i]] <- c(start, end)
  
  window_names[[i]] <- paste(start, "-", end, sep = "") 
  window_median[[i]] <- median(start, end)
}  


#subset flux data for each window in list
#conditional statement because window wraps around from 257-3 degrees
split_dat <- list()
for(i in 1: length(window_list)) {
  start <- window_list[[i]][1]
  end <-  window_list[[i]][2]
  if (end > start){
    sub_dat <- dat_voi%>%
      filter(wind_dir >= start & wind_dir <= end)
  } else{
    sub_dat <- dat_voi%>%
    filter((wind_dir >= start & wind_dir <= 360) | (wind_dir >= 0 & wind_dir <= end))
  }
  split_dat[[i]] <- sub_dat
}  
  
#calculate average gpp by wind direction (dataframe in list)
avg_gpp <- numeric(length(split_dat))

for (i in seq_along(split_dat)) {
  avg_gpp[i] <- mean(split_dat[[i]]$gpp, na.rm = TRUE)
}
#calculate standard error for each direction
se_gpp <- sapply(split_dat, function(x) {
  mean(x$gpp, na.rm = TRUE) / sqrt(length(x$gpp))
})

#make new df for easier plotting
plot_frame <- data.frame(window_median, avg_gpp, se_gpp)

#plot midpoint and average gpp; use se to make y ranges
plot <- ggplot(data = plot_frame, aes(window_median, avg_gpp, ymin = avg_gpp - se_gpp, ymax = avg_gpp + se_gpp)) +
  geom_point() + 
  geom_errorbar(width = 0.2) + 
  labs(x = "Wind Direction Window Median", y = "Average GPP per Wind Direction", title = "Productivity per Wind Direction with Standard Error")

plot



#=================================WD frequency==================================
num_obs <- numeric(length(split_dat))

for (i in seq_along(split_dat)) {
  num_obs[i] <- nrow(split_dat[[i]])
}
plot_frame <- data.frame(window_median, num_obs)
plot <- ggplot(data = plot_frame, aes(window_median, num_obs))+
  geom_point() + 
  labs(x = "Wind Direction Window Median", y = "Number of Observations", title = "Wind Direction Frequency")

plot
#===============================================================================
#calculate moving avg for each frame
#arrange by doy and calculate rolling mean (k = window parameter)
#for (i in seq_along(split_dat)) {
#  split_dat[[i]] <- split_dat[[i]] %>% 
#    arrange(doy)%>%
#    mutate(wind_direction = window_names[i]) 
#  split_dat[[i]]$movavg <- rollmean(split_dat[[i]]$gpp, k = 5, fill = NA, align = "center")
#}

#===============================================================================
#=======================integrating fluxes: unnecessary==========================
#===============================================================================

#integrate gpp (for avg area under curves)  
#function for trapezoidal integration: calculates interval width, then area 
trap_int <- function(x, y) {
  dx <- diff(x)
  area <- sum((y[-1] + y[-length(y)]) * dx) / 2
  return(area)
}

#vector to store values
int_gpp <- numeric(length(split_dat))

#integrate each frame in list with loop
  #extract x (time) and y (gpp) values from each frame for calculation w function
  #movavg calculation leaves NA that causes error in function calculations, so extract real values only from x and y vectors (!is.na)
  #apply function
for (i in seq_along(split_dat)) {
  time <- split_dat[[i]]$doy
  gpp_movavg <- split_dat[[i]]$movavg
  
  non_na <- !is.na(gpp_movavg)
  time <- time[non_na]
  gpp_movavg <- gpp_movavg[non_na]
  
  int_gpp[i] <- trap_int(time, gpp_movavg)
}

#plot midpoint and integrated gpp
plot_frame <- data.frame(window_median, int_gpp)

plot <- ggplot(data = plot_frame, aes(window_median, int_gpp))+
          geom_point() + 
          labs(x = "Wind Direction Window Median", y = "Total Annual GPP", title = "Wind Direction Comparison")

plot
#===============================================================================
#===============================================================================
#===============================================================================
