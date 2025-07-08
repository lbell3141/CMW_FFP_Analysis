
library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(terra)
library(dplyr)
library(lubridate)
library(raster)

#===============================================================================
#======================calculate directional footprint==========================
#===============================================================================
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
  filter(HH_UTC %in% 8:17)%>%
  filter(mm %in% 6:9,
         yyyy == 2021)

#split into separate wind directions bins of 20 degrees
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
CM_dat <- CM_dat %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))
dat_by_month <- split(CM_dat, CM_dat$mm)
for (i in names(dat_by_month)) {
  assign(paste0("dat_", i), dat_by_month[[i]])
}
#===============================================================================
#list for FFP results
ffp_list <- list()

#create another function: pulls out variables instead of naming every time for ffp calculation
calc_ffp <- function(x) {
  calc_footprint_FFP_climatology(
    zm = x$zm, 
    z0 = x$z0, 
    umean = x$umean, 
    h = x$h, 
    ol = x$ol, 
    sigmav = x$sigmav,
    ustar = x$ustar,
    wind_dir = x$wind_dir, 
    fig = 1,
    domain = c(-200, 200, -200, 200), 
    r = seq(0, 90, by = 10)
  )
}

#use new function on list; calc ffp and store in list
ffp_list <- lapply(sep_split_dat, calc_ffp)

#write list of calculations to data:
saveRDS(ffp_list, file = "./Data/monthly_directional_ffps/summer2021/sep_calcd_ffp_list.rds")
#to read use readRDS()
