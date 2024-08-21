#checking quality flags

library(dplyr)
library(lubridate)

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
  #filter(USTAR > 0.2)%>%
  dplyr::select(yyyy, mm, day, HH_UTC, MM, wind_dir, GPP_PI)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)



#===============================================================================
#===============================================================================
test <- dat %>%
  select(TIMESTAMP_END, FC, USTAR, GPP_PI)



#===============================================================================
#===============================================================================
#raster image processing
library(terra)
image <- rast("./Data/Image_classification/CMW_editedAP_modified.tif")

blue <- image[[1]]
green <- image[[2]]
red <- image[[3]]

#filter for light pixels (ground)
ground <- (blue < 200) & (green < 200) & (red < 200)
#results in T/F, so convert to 1/0
#writeRaster(ground, "./Data/Image_classification/canopy.tif", overwrite=TRUE)
#test <- rast("./Data/Image_classification/canopy.tif")
#plot(test)

