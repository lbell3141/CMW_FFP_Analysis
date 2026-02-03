#predict avg footprint for all four sites + visualize

source("./Scripts/kljun_code/calc_footprint_FFP_climatology.R")

library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(terra)
library(dplyr)
library(lubridate)
library(raster)

#create another function: pulls out variables instead of naming every time for ffp calculation
calc_ffp <- function(x) {
  calc_footprint_FFP_climatology(
    zm = x$zm, 
    z0 = x$z0, 
    umean = x$umean, 
    h = x$h, 
    ol = x$ol, 
    sigmav = as.numeric(x$sigmav),
    ustar = x$ustar,
    wind_dir = x$wind_dir, 
    fig = 1,
    domain = c(-1000, 1000, -1000, 1000), 
    r = seq(0, 90, by = 10)
  )
}

#----------------Generate Footprint Climatologies-------------------------------
#CMW============================================================================

cmwffpdat <- read.csv("./SeriousStuff/Data/combined_CMdata.csv")%>%
  mutate(TIMESTAMP_END = ymd_hms(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS_1_1_1 >= 0)%>%
  filter(HH %in% 8:17)

avg_WS <- mean(cmwffpdat$WS_1_1_1, na.rm = T)

meas_h <- 14
can_h <- 7
d <- (2/3) * can_h
bound_h <- 1000

cmwall_clim_data <- cmwffpdat %>%
  reframe(
    yyyy = yyyy,
    mm = mm,
    day =day,
    HH_UTC = HH,
    MM = MM,
    zm = meas_h,
    umean = avg_WS,
    h = bound_h,
    ol = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigmav = sigma_v,
    ustar = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/ol)
cmwall_clim_dat <- cmwall_clim_data%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(if_all(everything(), ~ !is.na(.)))%>%
  filter(HH_UTC %in% 8:17)
  
cmwall_clim_dat <- cmwall_clim_dat%>%
  mutate(z0 = NaN)

cmwffp_list <- calc_ffp(cmwall_clim_dat)

saveRDS(cmwffp_list, "./SeriousStuff/Data/Footprints/SiteClimatologies/CMW_daytimeannual_ffptest.rds")

#SRG============================================================================

SRGffpdat <- read.csv("./SeriousStuff/Data/Input2dFootprint_SRG2024.csv")

SRGtowerdat <- read.csv("./SeriousStuff/Data/AMF_US-SRG_FLUXNET_FULLSET_HH_2008-2024_5-7.csv", na.strings = "-9999")%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS >= 0)%>%
  filter(HH %in% 8:17)

srgtimemerge <- merge(SRGtowerdat, SRGffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(srgtimemerge$WS, na.rm = T)

d <- 0.335
bound_h <- 1000

srgall_clim_data <- SRGffpdat %>%
  reframe(
    yyyy = yyyy,
    mm = mm,
    day =day,
    HH_UTC = HH,
    MM = MM,
    zm = zm,
    umean = avg_WS,
    h = bound_h,
    ol = L,
    sigmav = sigma_v,
    ustar = u_star,
    wind_dir = wind_dir,
    test = zm/L)
srgall_clim_dat <- srgall_clim_data%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(if_all(everything(), ~ !is.na(.)))%>%
  filter(HH_UTC %in% 8:17)
srgall_clim_dat <- srgall_clim_dat%>%
  mutate(z0 = NaN)

srgffp_list <- calc_ffp(srgall_clim_dat)

#saveRDS(srgffp_list, "./SeriousStuff/Data/Footprints/SiteClimatologies/SRG_daytimeannual_ffp.rds")

#SRM============================================================================

srmffpdat <- read.csv("./SeriousStuff/Data/Input2dFootprint_SRM2024.csv")

srmtowerdat <- read.csv("./SeriousStuff/Data/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2024_4-7.csv", na.strings = "-9999")%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS >= 0)%>%
  filter(HH %in% 8:17)

srmtimemerge <- merge(srmtowerdat, srmffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(srmtimemerge$WS, na.rm = T)

d <- 0.335
bound_h <- 1000

srmall_clim_data <- srmffpdat %>%
  reframe(
    yyyy = yyyy,
    mm = mm,
    day =day,
    HH_UTC = HH,
    MM = MM,
    zm = zm,
    umean = avg_WS,
    h = bound_h,
    ol = L,
    sigmav = sigma_v,
    ustar = u_star,
    wind_dir = wind_dir,
    test = zm/L)
srmall_clim_dat <- srmall_clim_data%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(if_all(everything(), ~ !is.na(.)))%>%
  filter(HH_UTC %in% 8:17)
srmall_clim_dat <- srmall_clim_dat%>%
  mutate(z0 = NaN)

srmffp_list <- calc_ffp(srmall_clim_dat)

#saveRDS(srmffp_list, "./SeriousStuff/Data/Footprints/SiteClimatologies/SRM_daytimeannual_ffp.rds")

#Wkg============================================================================

wkgffpdat <- read.csv("./SeriousStuff/Data/Input2dFootprint_Wkg2024.csv", skip = 0)

wkgtowerdat <- read.csv("./SeriousStuff/Data/AMF_US-Wkg_FLUXNET_FULLSET_HH_2004-2024_4-7.csv", na.strings = "-9999")%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS >= 0)%>%
  filter(HH %in% 8:17)

wkgtimemerge <- merge(wkgtowerdat, wkgffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(wkgtimemerge$WS, na.rm = T)

d <- 0.335
bound_h <- 1000

wkgall_clim_data <- wkgffpdat %>%
  reframe(
    yyyy = yyyy,
    mm = mm,
    day =day,
    HH_UTC = HH,
    MM = MM,
    zm = zm,
    umean = avg_WS,
    h = bound_h,
    ol = L,
    sigmav = sigma_v,
    ustar = u_star,
    wind_dir = wind_dir,
    test = zm/L)
wkgall_clim_dat <- wkgall_clim_data%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(if_all(everything(), ~ !is.na(.)))%>%
  filter(HH_UTC %in% 8:17)
wkgall_clim_dat <- wkgall_clim_dat%>%
  mutate(z0 = NaN)%>%
  filter(wind_dir %in% 0:360)

wkgffp_list <- calc_ffp(wkgall_clim_dat)

#saveRDS(wkgffp_list, "./SeriousStuff/Data/Footprints/SiteClimatologies/Wkg_daytimeannual_ffp.rds")

