#Wind direction characterization
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

#NW_dat_ffp <- dat_voi%>%
#  filter(wind_dir %in% 270:360)
#SE_dat_ffp <- dat_voi%>%
#  filter(wind_dir %in% 90:180)

#===============================================================================
#==============Interannual Variability of median wind direction ================
#===============================================================================

intan_var_doy <- dat_voi%>%
  group_by(yyyy, doy)%>%
  summarize(med_doy = median(wind_dir, na.rm = T))
intan_var_an <- dat_voi%>%
  group_by(yyyy)%>%
  summarize(med_yr = median(wind_dir, na.rm = T))
intan_combd <- merge(intan_var_an, intan_var_doy)
intan_combd$day_record_num <- seq_len(nrow(intan_combd))
intan_combd$date <- as.Date(paste0(intan_combd$yyyy, "-",intan_combd$doy), format = "%Y-%j")

#full time series (20 yr record)
ggplot(intan_combd, aes(x = date)) +
  geom_line(aes(y = med_doy, color = "Daily Median Wind Direction")) +
  geom_point(aes(y = med_yr, color = "Annual Median Wind Direction")) +
  scale_color_manual(values = c("red", "lightblue"), labels = c("Daily Median", "Annual Median")) +
  labs(x = "Day of Year", y = "Median Wind Direction", color = NULL, 
       title = "Interannual Variability of Wind Direction") +
  theme_minimal() +
  theme(legend.position = "bottom") 

#time series for each year over 365 days
ggplot(intan_combd, aes(x = doy, y = med_doy, color = factor(yyyy))) +
  geom_point(alpha = 0.7) +  # Add transparency to lines
  labs(x = "Day of Year", y = "Value", color = "Year") +
  theme_minimal()
#===============================================================================
#============================Diurnal Variation ==============================
#===============================================================================

NW_dat_ffp_HH <- NW_dat_ffp%>%
  group_by(HH_UTC, MM)%>%
  summarize(gpp_HH_avg = mean(gpp, na.rm = T))
NW_dat_ffp_HH$dir <- "NW"
SE_dat_ffp_HH <- SE_dat_ffp%>%
  group_by(HH_UTC, MM)%>%
  summarize(gpp_HH_avg = mean(gpp, na.rm = T)) 
SE_dat_ffp_HH$dir <- "SE"
HHcombd_dat <- rbind(NW_dat_ffp_HH, SE_dat_ffp_HH)
  
ggplot(HHcomd_dat)
#Annual Variation

#GPP, NEE, and LE by WD


#table of average driver values (winter, pre, post)- PPFD, RH, TA, VPD, WS, RECO, GPP, NEE