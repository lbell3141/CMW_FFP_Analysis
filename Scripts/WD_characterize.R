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
#==============Interannual Variability of Median Wind Direction ================
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

#full time series (21 yr record)
ggplot(intan_combd, aes(x = date)) +
  geom_line(aes(y = med_doy, color = "Daily Median Wind Direction")) +
  geom_point(aes(y = med_yr, color = "Annual Median Wind Direction")) +
  scale_color_manual(values = c("red", "lightblue"), labels = c("Annual Median", "Daily Median")) +
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
#===================Interannual Variability in Average GPP======================
#===============================================================================

intan_var_doy <- dat_voi%>%
  group_by(yyyy, doy)%>%
  summarize(avg_doy_gpp = mean(gpp, na.rm = T))
intan_var_an <- dat_voi%>%
  group_by(yyyy)%>%
  summarize(avg_an_gpp = mean(gpp, na.rm = T))
intan_combd <- merge(intan_var_an, intan_var_doy)
intan_combd$day_record_num <- seq_len(nrow(intan_combd))
intan_combd$date <- as.Date(paste0(intan_combd$yyyy, "-",intan_combd$doy), format = "%Y-%j")

#plotting
ggplot(intan_combd, aes(x = date)) +
  geom_line(aes(y = avg_doy_gpp, color = "Daily Avg GPP")) +
  geom_point(aes(y = avg_an_gpp, color = "Annual Avg GPP")) +
  scale_color_manual(values = c("red", "lightblue"), labels = c("Annual Average", "Daily Average")) +
  labs(x = "Year", y = "Average GPP", color = NULL, 
       title = "Interannual Variability of GPP") +
  theme_minimal() +
  theme(legend.position = "bottom") 

#=========================Converting to Z-Scores================================
#calculate z-scores for gpp
gpp_zs <- intan_var_gpp
gpp_zs$rec_avg <- mean(gpp_zs$avg_an_gpp)
gpp_zs$rec_sd <- sd(gpp_zs$avg_an_gpp)
gpp_zs$gpp_z_score <- ((gpp_zs$avg_an_gpp - gpp_zs$rec_avg) / gpp_zs$rec_sd)

#plot z-scores
ggplot(gpp_zs, aes(x = yyyy)) +
  geom_point(aes(y = gpp_z_score, color = "Annual GPP Z-Score")) +
  scale_color_manual(values = "navy", labels = c("Annual GPP Z-Score")) +
  labs(x = "Year", y = "Average GPP", color = NULL, 
       title = "Interannual Variability of GPP") +
  theme_minimal() +
  theme(legend.position = "bottom") 

#===============================================================================
#==========Relating Interannual Variation in Wind Direction and GPP============
#===============================================================================
#calculate z scores for annual median wind direction values
wd_zs <- intan_var_an
wd_zs$rec_avg <- mean(wd_zs$med_yr)
wd_zs$rec_sd <- sd(wd_zs$med_yr)
wd_zs$wd_z_score <- ((wd_zs$med_yr - wd_zs$rec_avg) / wd_zs$rec_sd)

#plot wind direction z-scores
ggplot(wd_zs, aes(x = yyyy)) +
  geom_point(aes(y = wd_z_score, color = "Annual Median Wind Direction Z-Score")) +
  scale_color_manual(values = "navy", labels = c("Annual Median Wind Direction Z-Score")) +
  labs(x = "Year", y = "Annual Median Wind Direction Z-Score", color = NULL, 
       title = "Interannual Variability of Median Wind Direction") +
  theme_minimal() +
  theme(legend.position = "bottom") 

#combine zscore gpp and wd dataframes 
combd_zs <- data.frame(yyyy = gpp_zs$yyyy)
combd_zs$gpp_zs <- gpp_zs$gpp_z_score
combd_zs$wd_zs <- wd_zs$wd_z_score

#plot wind direction and gpp z scores
ggplot(combd_zs, aes(x = wd_zs)) +
  geom_point(aes(y = gpp_zs)) +
  scale_color_manual(values = "black") +
  labs(x = "Annual Wind Direction Z Score", y = "Annual GPP Z Score", color = NULL, 
       title = "") +
  theme_minimal() +
  theme(legend.position = "bottom") 


#===============================================================================
#======================Z Score for GPP Bias by Wind Direction===================
#===============================================================================
#create moving window for wind directions
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

#convert list to a df
#give each a list ID
split_dat_ID <- lapply(seq_along(split_dat), function(i) {
  split_dat_ID <- split_dat[[i]]
  split_dat_ID$WD <- i
  return(split_dat_ID)
})
#merge lists into a dataframe (bind_rows from dplyr)
combd_WD <- bind_rows(split_dat_ID)

#z scores
combd_WD$gpp_avg <- mean(combd_WD$gpp, na.rm = T)
combd_WD$gpp_sd <- sd(combd_WD$gpp, na.rm = T)
combd_WD$gpp_bias_zs <- ((combd_WD$gpp - combd_WD$gpp_avg) / combd_WD$gpp_sd)

#zs dataframe
WD_gpp_bias <- combd_WD %>%
  group_by(WD)%>%
  summarize(avg_gpp_zs = mean(gpp_bias_zs, na.rm = T))

#==========Interannual Variability of WD expressed as WD GPP Bias===============

#intan_var_an from line 67; round wind directions to whole numbers
intan_var_an$med_yr <- round(intan_var_an$med_yr)
names(intan_var_an)[which(names(intan_var_an) == "med_yr")] <- "WD"
combd_bias <- merge(intan_var_an, WD_gpp_bias)

#plot IA WD z score with year
ggplot(combd_bias, aes(x = yyyy)) +
  geom_point(aes(y = avg_gpp_zs)) +
  scale_color_manual(values = "black") +
  labs(x = "Year", y = "Wind Direction GPP Z Score", color = NULL, 
       title = "Annual GPP Z Score based on Wind Direction GPP Bias") +
  theme_minimal() +
  theme(legend.position = "bottom") 



