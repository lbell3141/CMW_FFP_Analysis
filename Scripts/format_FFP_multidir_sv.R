
library(lubridate)
library(dplyr)

csvtestoutput = "./single_sv_test.csv"
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    #h = bound_h,
    z0 = "-999",
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = 0,
    u_star = USTAR,
    WD_1_1_1 = WD_1_1_1,
    WS_1_1_1 = WS_1_1_1,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.2)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, WD_1_1_1, WS_1_1_1)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)
#%>%filter(yyyy %in% 2018:2021)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$WD_1_1_1 = as.numeric(dat_ffp$WD_1_1_1)


#creating list of wind directions
wdsub_dat_ffp <- dat_ffp%>%
  filter(WD_1_1_1 %in% 170:270)

deg_int <- seq(170, 270, by = 10)
split_dat <- split(wdsub_dat_ffp, cut(wdsub_dat_ffp$WD_1_1_1, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- c("170-180","180-190","190-200", "200-210", "210-220", "220-230", "230-240", "240-250", "250-260", "260-270") 
names(split_dat) <- dir_names

test_dat <- split_dat

#separate sigma_v calculation using function from sigma_v_calc.R
sigma_vs <- vector("list", length(split_dat))
WD_means <- vector("list", length(split_dat))

for (i in seq_along(split_dat)){
  WD_means[[i]]$avg_WD = mean(split_dat[[i]]$WD_1_1_1, na.rm = T)
  WD_means[[i]]$avg_WD_rad = WD_means[[i]]$avg_WD* pi / 180 
}


for (i in seq_along(split_dat)){
  sigma_vs[[i]]$WD_rad <- split_dat[[i]]$WD_1_1_1 * pi / 180 
  sigma_vs[[i]]$u_bar_x =  split_dat[[i]]$WS_1_1_1 * sin(sigma_vs[[i]]$WD_rad - pi)
  sigma_vs[[i]]$u_bar_y =  split_dat[[i]]$WS_1_1_1 * cos(sigma_vs[[i]]$WD_rad - pi)
  sigma_vs[[i]]$theta_v_bar = atan2(sigma_vs[[i]]$u_bar_x, sigma_vs[[i]]$u_bar_y) + pi
  
  sigma_vs[[i]]$var_sigma_v = (split_dat[[i]]$WS_1_1_1 * sin(WD_means[[i]]$avg_WD_rad - sigma_vs[[i]]$WD_rad))^2
  sigma_vs[[i]]$sigma_v = sqrt(sigma_vs[[i]]$var_sigma_v)
  
}

for (i in seq_along(split_dat)){
  #write values calculated above onto respective columns in each wind direction dataframe
  split_dat[[i]]$sigma_v <- sigma_vs[[i]]$sigma_v
  #clean up dataframe format for ffp calculator
  #remove wind speed column
  split_dat[[i]]$WS_1_1_1 <- NULL
  #rename wind direction column
  colnames(split_dat[[i]])[colnames(split_dat[[i]]) == "WD_1_1_1"] <- "wind_dir"
}

#write csvs 
for (i in seq_along(split_dat)){
  filename <- paste0(dir_names[i], ".csv")
  write.csv(split_dat[[i]], file = filename, row.names = F)
}


#========================single val sig_v=======================================
#convert test_dat WD from degrees to radians
for (i in seq_along(test_dat)){
  test_dat[[i]]$WD_1_1_1 = test_dat[[i]]$WD_1_1_1 * pi / 180
}

for (i in seq_along(test_dat)){
  sigma_v <- (1/(nrow(test_dat[[i]]) - 1)) * sum(test_dat[[i]]$WS_1_1_1 * sin(WD_means[[i]]$avg_WD_rad - test_dat[[i]]$WD_1_1_1))
  test_dat[[i]]$sigma_v <- sign(sigma_v) * sqrt(abs(sigma_v))
}

unique(test_dat[[1]]$sigma_v)

for (i in seq_along(test_dat)) {
  test_dat[[i]]$WS_1_1_1 <- NULL
  #convert WD in radians back to degrees and rename
  test_dat[[i]]$WD_1_1_1 <- test_dat[[i]]$WD_1_1_1 * 180 / pi
  colnames(test_dat[[i]])[colnames(test_dat[[i]]) == "WD_1_1_1"] <- "wind_dir"
}

#test_dat[[1]]$sigma_v <- 0.2

#write csvs 
write.csv(test_dat[[1]], file = "single_sv_test.csv", row.names = FALSE)

