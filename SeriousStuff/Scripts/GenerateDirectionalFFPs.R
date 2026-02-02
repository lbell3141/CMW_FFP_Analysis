#generate ffp maps in 18 directions for each site

source("./Scripts/kljun_code/calc_footprint_FFP_climatology.R")

library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(terra)
library(dplyr)
library(lubridate)
library(raster)

deg_int <- seq(0, 360, by = 20)

#lists for FFP results
cmwffp_list <- list()
srgffp_list <- list()
srmffp_list <- list()
wkgffp_list <- list()

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

#CMW============================================================================

cmwffpdat <- read.csv("./SeriousStuff/Data/combined_CMdata.csv")%>%
 mutate(TIMESTAMP_END = ymd_hms(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS_1_1_1 >= 0)%>%
  filter(HH %in% 8:17)%>%
  filter(mm %in% 5:7)

avg_WS <- mean(cmwffpdat$WS_1_1_1, na.rm = T)
#range(cmwtimemerge$WS_1_1_1, na.rm = T)

#format df for ffp calc
meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

cmwall_clim_data <- cmwffpdat %>%
  reframe(
    yyyy = yyyy,
    mm = mm,
    day =day,
    HH_UTC = HH,
    MM = MM,
    zm = meas_h - d,
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
  filter(HH_UTC %in% 8:17)%>%
  filter(mm %in% 5:9)
cmwall_clim_dat <- cmwall_clim_dat%>%
  mutate(z0 = NaN)

#split into separate wind directions bins of 20 degrees
cmwall_clim_dat$dir_bin <- cut(
  cmwall_clim_dat$wind_dir,
  breaks = deg_int,
  include.lowest = TRUE,
  right = FALSE,  # intervals [0,20)
  labels = head(deg_int, -1)  #lower bounds as labels
)
cmwsplit_dat <- split(cmwall_clim_dat, cmwall_clim_dat$dir_bin)

#####Calc + save footprints

#use new function on list; calc ffp and store in list
cmwffp_list <- lapply(cmwsplit_dat, calc_ffp)

#write list of calculations to data:
saveRDS(cmwffp_list, file = "./SeriousStuff/Data/Footprints/CMW_MJJ24_ffp_list.rds")

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
  filter(HH %in% 8:17)%>%
  filter(mm %in% 5:7)

srgtimemerge <- merge(SRGtowerdat, SRGffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(srgtimemerge$WS, na.rm = T)
#range(srgtimemerge$WS_1_1_1, na.rm = T)

#format df for ffp calc
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
  filter(HH_UTC %in% 8:17)%>%
  filter(mm %in% 5:9)
srgall_clim_dat <- srgall_clim_dat%>%
  mutate(z0 = NaN)

#split into separate wind directions bins of 20 degrees
srgall_clim_dat$dir_bin <- cut(
  srgall_clim_dat$wind_dir,
  breaks = deg_int,
  include.lowest = TRUE,
  right = FALSE,  # intervals [0,20)
  labels = head(deg_int, -1)  #lower bounds as labels
)
srgsplit_dat <- split(srgall_clim_dat, srgall_clim_dat$dir_bin)

#####Calc + save footprints

#use new function on list; calc ffp and store in list
srgffp_list <- lapply(srgsplit_dat, calc_ffp)

#write list of calculations to data:
saveRDS(srgffp_list, file = "./SeriousStuff/Data/Footprints/SRG_MJJ24_ffp_list.rds")


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
  filter(HH %in% 8:17)%>%
  filter(mm %in% 5:7)

srmtimemerge <- merge(srmtowerdat, srmffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(srmtimemerge$WS, na.rm = T)
#range(srmtimemerge$WS_1_1_1, na.rm = T)

#format df for ffp calc
d <- 2.01
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
  filter(HH_UTC %in% 8:17)%>%
  filter(mm %in% 5:9)
srmall_clim_dat <- srmall_clim_dat%>%
  mutate(z0 = NaN)

#split into separate wind directions bins of 20 degrees
srmall_clim_dat$dir_bin <- cut(
  srmall_clim_dat$wind_dir,
  breaks = deg_int,
  include.lowest = TRUE,
  right = FALSE,  # intervals [0,20)
  labels = head(deg_int, -1)  #lower bounds as labels
)
srmsplit_dat <- split(srmall_clim_dat, srmall_clim_dat$dir_bin)

#####Calc + save footprints

#use new function on list; calc ffp and store in list
srmffp_list <- lapply(srmsplit_dat, calc_ffp)

#write list of calculations to data:
saveRDS(srmffp_list, file = "./SeriousStuff/Data/Footprints/SRM_MJJ24_ffp_list.rds")


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
  filter(HH %in% 8:17)%>%
  filter(mm %in% 5:7)

wkgtimemerge <- merge(wkgtowerdat, wkgffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(wkgtimemerge$WS, na.rm = T)
#range(wkgtimemerge$WS, na.rm = T)

#format df for ffp calc
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
  filter(HH_UTC %in% 8:17)%>%
  filter(mm %in% 5:9)
wkgall_clim_dat <- wkgall_clim_dat%>%
  mutate(z0 = NaN)

#split into separate wind directions bins of 20 degrees
wkgall_clim_dat$dir_bin <- cut(
  wkgall_clim_dat$wind_dir,
  breaks = deg_int,
  include.lowest = TRUE,
  right = FALSE,  # intervals [0,20)
  labels = head(deg_int, -1)  #lower bounds as labels
)
wkgsplit_dat <- split(wkgall_clim_dat, wkgall_clim_dat$dir_bin)

#####Calc + save footprints

#use new function on list; calc ffp and store in list
wkgffp_list <- lapply(wkgsplit_dat, calc_ffp)

#write list of calculations to data:
saveRDS(wkgffp_list, file = "./SeriousStuff/Data/Footprints/Wkg_MJJ24_ffp_list.rds")
