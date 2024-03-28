#Multidirectional productivity comparison

library(lubridate)
library(dplyr)
library(plantecophys)
library(ggplot2)
library(zoo)

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

#Split into 8 wind directions
deg_int <- seq(0, 360, by = 45)
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

# Create a ggplot object
plot <- ggplot()


for (i in seq_along(split_dat)) {
  # Arrange the data frame by 'doy'
  split_dat[[i]] <- split_dat[[i]] %>% arrange(doy)
  
  # Calculate moving average and add it as a new column
  split_dat[[i]]$movavg <- rollmean(split_dat[[i]]$gpp, k = 500, fill = NA, align = "center")
}


# Add geom_point for each data frame in split_dat
for (i in seq_along(split_dat)) {
  plot <- plot + geom_line(data = split_dat[[i]], aes(x = doy, y = movavg), color = rainbow(length(split_dat))[i])
}

# Print the plot
print(plot)





