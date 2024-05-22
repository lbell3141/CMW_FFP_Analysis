#fixing veg cover plot
#combining PFT classes to visualize proportion of woody vs herbaceous cover per footprint pixel
#data = RAP biomass product from 2001 - 2021

library(lubridate)
library(dplyr)
library(plantecophys)
library(ggplot2)
library(ggbreak)
library(terra)
library(viridis)
library(sf)

pathtoRAPrasts <- "./Data/RAP"
pathtoFFPoutline <- "./Data/RAP/2017_FFP_Outline/twi_ffp_sec.shp"

#load rasters (and store names) from downloaded from GEE (RAP biomass product)
rast_files <- list.files(pathtoRAPrasts, pattern = "RAP_VegCover_\\d{4}\\.tif$", full.names = TRUE)
rast_names <- list.files(pathtoRAPrasts, pattern = "RAP_VegCover_\\d{4}\\.tif$", full.names = FALSE)

#===============================================================================
#==================1. Veg cover plot: prop woody to herbaceous==================
#===============================================================================

#apply names to rasters
rast_list <- lapply(rast_files, rast)
names(rast_list) <- rast_names

#make vector to store raster layer averages (change length with # of bands/PFTs)
avg_rast_list <- vector("list", length = 4)

#calculate average values for each layer (PFT) across the current record (2001-2021)
#loop: 1) Extract the same layer from each raster, 2) then calculate the average
for (i in 1:4) {
  layer_stack <- sds(lapply(rast_list, function(x) x[[i]]))
  avg_rast_list[[i]] <- app(layer_stack, mean)
}

#add herbaceous bands (1 and 2) and woody bands (3 and 4) together to create a 2 band raster
avg_rast <- rast(avg_rast_list)
combd_rast <- c(avg_rast[[1]] + avg_rast[[2]], avg_rast[[3]] + avg_rast[[4]])
names(combd_rast) <- c("Herbaceous", "Woody")

#create raster comparing proportion of woody/herbaceous cover per pixel
#woody - herbaceous
dif_rast <- combd_rast[[2]] - combd_rast[[1]]
ffp <- vect(pathtoFFPoutline)

#format plot
extent <- c(-110.18, -110.1755, 31.6615, 31.666)
dif_rast <- crop(dif_rast, extent)

#function for radially segmented lines
#dividing the radians of a circle 
#set line length 
#set line style
plot_lines <- function(center_x, center_y, num_lines, line_length) {
  angles <- seq(0, 2*pi, length.out = num_lines + 1)[-1]
  end_x <- center_x + line_length * cos(angles)
  end_y <- center_y + line_length * sin(angles)
  for (i in 1:num_lines) {
    lines(c(center_x, end_x[i]), c(center_y, end_y[i]), col = "black", lty = "dashed")
  }
}

#plot
plot(dif_rast, col = viridis(15), main = "Woody and Herbaceous Cover per Pixel at US-CMW")


#add ffp 90th extent shapefile
lines(ffp, col = "black", lwd = 2)

#add lines
center_x <- mean(extent[1:2])
center_y <- mean(extent[3:4])
num_lines <- 8  
line_length <- 0.1  
plot_lines(center_x, center_y, num_lines, line_length)

#===============================================================================
#===================2. Veg cover with 10 degree footprints======================
#===============================================================================

#load in shapefiles; pull files from many folders using same naming convention
file_list <- list()
WD_folders <- list.dirs("./FFP_Outputs/multidirectional/", full.names = TRUE, recursive = TRUE)

sf_pattern <- "_90th.shp" 

for (i in WD_folders) {
  files <- list.files(i, pattern = sf_pattern, full.names = TRUE)
  file_list <- c(file_list, files)
}

#load in proportional veg cover rast made in code above (object = dif_rast)

#mask RAP by looping through each sf
avg_cover_df <- data.frame(shapefile = character(), mean_value = numeric())

# Loop through each shapefile
for (i in file_list) {
  #load .shp in the list as shapefiles
  sfs <- st_read(i)
  #then loop through and mask the RAP data with the sfs
  masked_rast <- mask(dif_rast, sfs)
  #find mean for each footprint
  avg_cover_val <- global(masked_rast, fun = mean, na.rm = TRUE)[[1]]
  #store values in df
  avg_cover_df <- rbind(avg_cover_df, data.frame(shapefile = basename(i), avg_cover_val = avg_cover_val, stringsAsFactors = FALSE))
}

#plotting GPP and avg cover=====================================================

#plot RAP vales against GPP_WD (predicted positive relationship)
avg_cover_df[,1] <- c("170-180","180-190","190-200", "200-210", "210-220", "220-230", "230-240", "240-250", "250-260", "260-270")

#below code adapted from multidir_comparison script
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

#use library plantecophys to calc VPD
dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)

#create dataframe
dat_ffp = dat_file %>%
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

wdsub_dat_ffp <- dat_ffp%>%
  filter(wind_dir %in% 170:270)
deg_int <- seq(170, 270, by = 10)
split_dat <- split(wdsub_dat_ffp, cut(wdsub_dat_ffp$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- c("170-180","180-190","190-200", "200-210", "210-220", "220-230", "230-240", "240-250", "250-260", "260-270") 
names(split_dat) <- dir_names

#calculate average gpp by wind direction (dataframe in list)
avg_gpp <- numeric(length(split_dat))
for (i in seq_along(split_dat)) {
  avg_gpp[i] <- mean(split_dat[[i]]$gpp, na.rm = TRUE)
}

#calculate standard error for each direction
se_gpp <- sapply(split_dat, function(x) {
  mean(x$gpp, na.rm = TRUE) / sqrt(length(x$gpp))
})

#make new df by combining both RAP and GPP values
plot_frame <- data.frame(avg_gpp, se_gpp)
plot_frame <- cbind(avg_cover_df, plot_frame)

#plot: WD, GPP, and % cover
plot <- ggplot(data = plot_frame, aes(x = shapefile, y = avg_gpp, size = avg_cover_val, ymin = avg_gpp - se_gpp, ymax = avg_gpp + se_gpp)) +
  geom_point() + 
  geom_errorbar(width = 0.2, size = 0.5) + 
  scale_size_continuous(name = "Average % Woody Cover") + 
  labs(x = "Wind Direction Window", y = "Average GPP", title = "Productivity per Wind Direction and Woody Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot

#plot: GPP and % cover
plot <- ggplot(data = plot_frame, aes(avg_cover_val, avg_gpp, ymin = avg_gpp - se_gpp, ymax = avg_gpp + se_gpp)) +
  geom_point() + 
  geom_errorbar(width = 0.2) + 
  labs(x = "Avgerage % Cover", y = "Average GPP (WD:170-270)", title = "Productivity and Woody Cover")

plot

#=====checking with WS==========================================================
#calculate average gpp by wind direction (dataframe in list)
avg_WS <- numeric(length(split_dat))
for (i in seq_along(split_dat)) {
  avg_WS[i] <- mean(split_dat[[i]]$wind_sp, na.rm = TRUE)
}

#calculate standard error for each direction
se_WS <- sapply(split_dat, function(x) {
  mean(x$wind_sp, na.rm = TRUE) / sqrt(length(x$wind_sp))
})

#make new df by combining both RAP and GPP values
plot_frame <- cbind(plot_frame, avg_WS, se_WS)

#plot: GPP and WS
plot <- ggplot(data = plot_frame, aes(avg_WS, avg_gpp)) +
  geom_point() + 
  labs(x = "Avgerage Wind Speed", y = "Average GPP (WD:170-270)", title = "GPP/Wind Speed")

plot

#=====and latent heat!==========================================================
#calculate average le by wind direction (dataframe in list)
avg_le <- numeric(length(split_dat))
for (i in seq_along(split_dat)) {
  avg_le[i] <- mean(split_dat[[i]]$le, na.rm = TRUE)
}
#make new df by combining both RAP and GPP values
plot_frame <- cbind(plot_frame, avg_le)

#plot: GPP and % cover
plot <- ggplot(data = plot_frame, aes(avg_gpp, avg_le)) +
  geom_point() + 
  labs(x = "Average GPP (WD:170-270)", y = "Average LE", title = "GPP/Latent Heat")

plot


#====================boxplot curve for WD, cover, and gpp=======================

deg_int <- seq(0, 360, by = 10)
split_dat <- split(dat_ffp, cut(dat_ffp$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

dir_names <- paste0(deg_int[-length(deg_int)], "-", deg_int[-1])
names(split_dat) <- dir_names

#convert list to a df
#give each a list ID
split_dat_ID <- lapply(seq_along(split_dat), function(i) {
  split_dat_ID <- split_dat[[i]]
  split_dat_ID$WD <- dir_names[i]
  return(split_dat_ID)
})
#merge lists into a dataframe 
combd_WD <- bind_rows(split_dat_ID)

avg_cover_df <- avg_cover_df %>% rename(WD = shapefile)
plot_frame <- merge(combd_WD, avg_cover_df, by = "WD")

plot_frame <- plot_frame %>%
  group_by(WD)%>%
  mutate(avg_gpp = mean(gpp, na.rm = T))
  
plot_frame <- plot_frame %>%
  group_by(WD)%>%
  mutate(se_gpp = sd(gpp, na.rm = T))

#boxplot
plot <- ggplot(data = plot_frame, aes(x = WD, y = gpp)) +
  geom_boxplot() +
  scale_y_break(c(16, 30)) +  
  stat_summary(fun = mean, aes(size = avg_cover_val), geom = "point", shape = 8, color = "red") + 
  scale_size_continuous(name = "Average % Woody Cover") +
  labs(x = "Wind Direction Window", y = "Average GPP", title = "Productivity per Wind Direction and Woody Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot
