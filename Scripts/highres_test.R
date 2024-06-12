#estimating sigma_v using high res flux data (10Hz)
#testing estimates with daytime values for a single day (19 Jan 2022)

#load relevant libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(plantecophys)


#set data paths
pathtoHFdata <- "./Data/High_Freq_Data/Jan2019.txt"
pathtoHHdata <- "./Data/AMF_US-CMW_BASE_HH_2-5.csv"

#===============================================================================
#=====================Aggregate 10Hz to 3s observations=========================
#===============================================================================
#load high frequency data (10Hz obs)
hf_data <- read.table(pathtoHFdata, header = TRUE, sep = "\t")
#format timestamp
hf_data$TimeStamp <- ymd_hms(hf_data$TimeStamp)
#split into groups of 30 observations 
#add row number
hf_data <- hf_data %>%
  mutate(row_ID = row_number())
#adding dates by brute force bc something is wonky when I do it in the pipe w the rest of the values
#new column for dates in order to keep hm info in timestamp column
hf_data <- hf_data %>%
  mutate(Date = as.Date(TimeStamp))
group_date <- hf_data %>%
  # "%/% rounds down to the whole number in the quotient
  group_by(group = (row_ID - 1) %/% 30) %>%
  summarise(Date = first(Date))

#add date to real df
ag_3s_df <- hf_data %>%
  group_by(group = (row_ID - 1) %/% 30) %>%
  summarise(
    Date = first(Date),
    Time = max(TimeStamp, na.rm = TRUE),
    Ux = mean(Ux, na.rm = TRUE),
    Uy = mean(Uy, na.rm = TRUE),
    Uz = mean(uZ, na.rm = TRUE)
  )

#===============================================================================
#==================Calculate theta_i from vector components=====================
#===============================================================================
#use x and y components of wind direction and pythagorean thm to calc magnitude (wind speed)
ag_3s_df$u_i <- ((((ag_3s_df$Ux)^2) + ((ag_3s_df$Uy)^2)))^(1/2)
#atan2 returns quadrant-sensitive outputs (ie 0 to pi to -pi instead of just 0 to pi)
ag_3s_df$theta_stand <- atan2(ag_3s_df$Ux, ag_3s_df$Uy) 

#convert to compass degrees from standard plane radians
radians_to_compass <- function(angle_rad) {
  angle_deg <- angle_rad * (180 / pi)  
  compass_angle <- (90 - angle_deg) %% 360  
  return(compass_angle)
}

ag_3s_df$theta_cc <- radians_to_compass(ag_3s_df$theta_stand)

#===============================================================================
#====================Calculate half-hourly theta_v_bar==========================
#===============================================================================

bar_df <- ag_3s_df

#group by half hour (every 600 observations) and calc variables
bar_df_HH <- bar_df %>%
  group_by(group = group %/% 600) %>%
  summarise(
    u_i_bar = (1 / n()) * sum(u_i),
    u_x_bar = (1 / n()) * sum(u_i * sin(theta_stand - pi)),
    u_y_bar = (1 / n()) * sum(u_i * cos(theta_stand - pi)),
    theta_v_bar = atan2(u_x_bar, u_y_bar) + pi,
    sigma_v_sqd = (1 / (n() - 1)) * sum(u_i * sin(theta_v_bar - theta_stand)),
    sigma_v = sqrt(abs(sigma_v_sqd))
  )
bar_df_HH$theta_v_bar_comp <- radians_to_compass(bar_df_HH$theta_v_bar)
#check relationship
plot(bar_df_HH$u_i_bar, bar_df_HH$sigma_v)

#===============================================================================
#==============Plotting Variability in Diurnal U and HH theta===================
#===============================================================================

#=======================Diurnal wind speed changes==============================
plot_df_theta <- ag_3s_df
plot_df_theta$Time <- hms(plot_df_theta$Time)

plot_df_theta <- plot_df_theta %>%
  #day time values needed only. 8am -> 600*16 -> row 9600; 5pm -> 17*2*600 -> row 20400
  filter(row_number() >= 9600 & row_number() <= 20399)%>%
  mutate(Hour = hour(Time),
         Minute = minute(Time))%>%
  #creating groups of HH with correct time labels
  mutate(HalfHour = ifelse(Minute < 30, "00", "30"),
         TimeGroup = paste0(sprintf("%02d", Hour), ":", HalfHour))

ggplot(plot_df_theta, aes(x = factor(TimeGroup), y = theta_cc)) + 
  geom_boxplot() +
  labs(x = "Time of Day (HH)", y = "Wind Direction") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#=======================Diurnal wind speed changes==============================

dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
#===============================================================================
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)

meas_h <- 14
d <- 7
bound_h <- 1000

dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h - d,
    zo = NaN,
    h = 1000,
    d = d, 
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    wind_sp = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    temp_atmos = TA_1_1_1,
    sigma_v = 0.8,
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
  select(yyyy, mm, doy, day, HH_UTC, MM, zm, zo, h, u_mean, sigma_v, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0) 
dat_voi$L = as.numeric(dat_voi$L)
#dat_voi$sigma_v = as.numeric(dat_voi$sigma_v)
dat_voi$u_star = as.numeric(dat_voi$u_star)
dat_voi$wind_dir = as.numeric(dat_voi$wind_dir)
#===============================================================================
dat_voi_hfday <- dat_voi%>%
  filter(yyyy == 2019)%>%
  filter(doy == 1)

#plotting 
ggplot(dat_voi, aes(x = factor(doy), y = wind_sp)) + 
  geom_boxplot() +
  labs(x = "Day of Year (2001-2021)", y = "Wind Speed (m/s)") +
  scale_x_discrete(breaks = seq(0, 365, by = 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  




