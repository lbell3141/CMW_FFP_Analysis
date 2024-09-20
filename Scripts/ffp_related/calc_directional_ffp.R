#calculate footprint climatologies by 10 degrees
#split_dat list from format_processed_data.R
#calc_footprint_FFP_climatology function from calc_footprint_FFP_climatology.R script (Kljun et al. 2015)

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
ffp_list <- lapply(dec_split_dat, calc_ffp)

#write list of calculations to data:
saveRDS(ffp_list, file = "dec_calcd_ffp_list.rds")
#to read use readRDS()

#===============================================================================
#=========================calculate RAP cover value=============================
#===============================================================================
#load ffps and RAP data
ffp_list <- readRDS("./Data/calcd_ffp_list.rds")
rap <- rast("./Data/RAP/avg_rast.tif")
#dif_rap <- rast("./Data/RAP/dif_rast.tif")
rast <- raster("./Data/Planet/JUN2024/reprodNDVIrast.tif")
canopy <- rast("./Data/Image_classification/canopy.tif")
#reproject:

proj_ext <- extent(-110.18013, -110.175952, 31.662064, 31.665691)
extent(rast) <- proj_ext
crs(rast) <- crs(rap)
rast <- rast(rast)  

#writeRaster(rast, "./Data/Planet/JUN2024/reprodNDVIrast.tif")
#increase raster resolution
rap_resamp <- disagg(rap, fact = 10)
#avg rap rast has woody and herbaceous layers
woody <- rap_resamp[[2]]
herb <- rap_resamp[[1]]

#split ffp_list into a list of x coords and y coords
x_list <- list()
y_list <- list()
for (i in seq_along(ffp_list)) {
  x_list[[i]] <- ffp_list[[i]]$xr
  y_list[[i]] <- ffp_list[[i]]$yr
}

#run code for ffp_contours_to_mask function in Countours_to_Mask.R
#apply function to lists and loaded RAP data:
rap_ffp_list <- list()
for (i in seq_along(x_list)) {
  rap_ffp_list[[i]] <- ffp_contours_to_mask(x_list[[i]], y_list[[i]], canopy)
}

#convert to df with correct WD
deg_int <- seq(10, 360, by = 10)
skip <- 50
deg_int_real <- deg_int[!deg_int %in% skip]

rap_ffp_df <- data.frame(direction = deg_int_real,
                         veg_cover = unlist(rap_ffp_list)
                         )

#===============================================================================
#=====================calculate GPP per 10 degree window========================
#===============================================================================
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

#avg gpp by group
avg_gpp <- dir_dat %>%
  group_by(dir_group) %>%
  summarize(
  avg_gpp = mean(GPP_PI, na.rm = T)
  )
#===============================================================================
#=============================plot cover with avg gpp===========================
#===============================================================================
avg_gpp <- avg_gpp %>%
  rename(direction = dir_group) %>%
  mutate(direction = as.numeric(as.character(direction))) %>% 
  arrange(direction)
gpp_cover_df <- inner_join(rap_ffp_df, avg_gpp, by = "direction")

plot(gpp_cover_df$veg_cover, gpp_cover_df$avg_gpp, xlab = "Cover", ylab = "GPP")
par(mfrow = c(1, 1))
plot(gpp_cover_df$direction, gpp_cover_df$avg_gpp, xlab = "Wind Direction", ylab = "GPP")
plot(gpp_cover_df$direction, gpp_cover_df$veg_cover, xlab = "Wind Direction", ylab = "Canopy Cover")


gpp_cover_df <- gpp_cover_df%>%
  mutate(veg_cover = veg_cover*100)
ggplot(gpp_cover_df, aes(x = veg_cover, y = avg_gpp))+
  geom_point()+
  theme_minimal()+
  labs(x = "Weighted % Canopy Cover", y = "Weighted Average GPP (µmolCO2 m-2 s-1)")+
  scale_x_continuous(breaks = seq(60,80,2.5))
 

df_sub <- gpp_cover_df %>%
  filter(direction %in% 170:270)
plot(df_sub$veg_cover, df_sub$avg_gpp)

#===============================================================================
#=====================GPP ~ season, cover, diurnality (?)=======================
#===============================================================================
