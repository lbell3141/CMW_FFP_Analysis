#subset to peak flux time to see bias with removed diurnality 
#def have another script that already does this but it's lost to the sands of time

library(lubridate)

#load csv with data used in the ffp calcs
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
  filter(HH_UTC %in% 8:17)

#split into separate wind directions bins of 10 degrees
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]
split_dat <- split(CM_dat, cut(CM_dat$wind_dir, deg_int, include.lowest = TRUE, labels = deg_int_real))
for (i in seq_along(split_dat)) {
  split_dat[[i]]$dir_group <- rep(names(split_dat)[i], nrow(split_dat[[i]]))
}
dir_dat <- do.call(rbind, split_dat)

test_df <- dir_dat %>%
  group_by(dir_group)%>%
  summarize(GPP = mean(GPP_PI, na.rm = T))%>%
  mutate(dir_group = as.numeric(dir_group))

par(mfrow =(1,1))
plot(test_df$dir_group, test_df$GPP)

