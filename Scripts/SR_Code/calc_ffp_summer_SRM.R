source("./Scripts/kljun_code/calc_footprint_FFP_climatology.R")


library(fields)
library(EBImage)
install.packages("./spatialfil_0.15.tar.gz", repos = NULL, type = "source")
library(terra)
library(dplyr)
library(lubridate)
library(raster)

org_dat

SRMtowerdat <- read.csv("./Data/RussHomework/GapfilledPartitionedFluxes_US-SRM_HH_2018-2024/GapfilledPartitionedFluxes_US-SRM_HH_202312312330_202412312330.csv", na.strings = "-9999")%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS_1_1_1 >= 0)

SRMtowerdat <- org_dat%>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END))%>%
  mutate(    yyyy = year(TIMESTAMP_END),
             mm = month(TIMESTAMP_END),
             day = day(TIMESTAMP_END),
             HH = hour(TIMESTAMP_END),
             MM = minute(TIMESTAMP_END))%>%
  filter(WS_1_1_1 >= 0)%>%
  filter(HH %in% 8:17)%>%
  filter(mm %in% 5:9)



SRMffpdat <- read.csv("./Data/RussHomework/Input2dFootprint_SRM2024.csv")


timemerge <- merge(SRMtowerdat, SRMffpdat, by = c("yyyy", "mm", "day", "HH", "MM"))
avg_WS <- mean(timemerge$WS_1_1_1, na.rm = T)
range(timemerge$WS_1_1_1, na.rm = T)

#format df for ffp calc
d <- 2.01
bound_h <- 1000

all_clim_data <- SRGffpdat %>%
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
all_clim_dat <- all_clim_data%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(if_all(everything(), ~ !is.na(.)))%>%
  filter(HH_UTC %in% 8:17)%>%
  filter(mm %in% 5:9)
all_clim_dat <- all_clim_dat%>%
  mutate(z0 = NaN)


#split into separate wind directions bins of 10 degrees
deg_int <- seq(0, 360, by = 20)
all_clim_dat$dir_bin <- cut(
  all_clim_dat$wind_dir,
  breaks = deg_int,
  include.lowest = TRUE,
  right = FALSE,  # intervals like [0,20)
  labels = head(deg_int, -1)  # use lower bounds as labels
)
split_dat <- split(all_clim_dat, all_clim_dat$dir_bin)

sep_split_dat <- lapply(split_dat, function(df) {
  df %>% filter(mm == 9)
})


#===============================================================================
#======================calculate directional footprint==========================
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
    sigmav = as.numeric(x$sigmav),
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
saveRDS(ffp_list, file = "./Data/RussHomework/SRM_2024_ffps/sep_calcd_ffp_list.rds")
#to read use readRDS()
