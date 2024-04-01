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

#arrange by doy and calculate rolling mean (k = window parameter)
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
    mutate(wind_direction = dir_names[i])  # Add a column to identify wind direction category
  split_dat[[i]]$movavg <- rollmean(split_dat[[i]]$gpp, k = 500, fill = NA, align = "center")
}

#make plot obj
plot <- ggplot()

for (i in seq_along(split_dat)) {
  plot <- plot + 
    geom_line(data = split_dat[[i]], aes(x = doy, y = movavg, color = wind_direction))  # Map wind direction category to color aesthetic
}

#adding legend
plot <- plot + 
  scale_color_manual(values = rainbow(length(split_dat)), 
                     breaks = dir_names, 
                     labels = dir_names) +
  theme_minimal()

print(plot)


