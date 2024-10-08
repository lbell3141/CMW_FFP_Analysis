
library(dplyr)
library(lubridate)

#===============================================================================
#========================stitch processed files together========================
#===============================================================================
#define directory and location of year folders
PathToYearFolders <- "E:/CM10Hz_Processed"
yr_folders <- list.dirs(PathToYearFolders, recursive = F)

#make list for csvs
csv_list <- list()
#for each year folder, create csv path and read into list; part-0 is what all csvs happen to be named
for (i in yr_folders) {
  PathToCSV <- file.path(i, "part-0.csv")
  csv_dat <- read.csv(PathToCSV)
  csv_list[[i]] <- csv_dat
}

#stack csvs and save output
full_dat <- bind_rows(csv_list)
#write_csv(full_dat, "full_processed_CMdata.csv")

#===============================================================================
#========================add sigma_v to rest of site data=======================
#===============================================================================
#load data paths and read as csvs
PathToSiteData <- "./Data/AMF_US-CMW_BASE_HH_2-5.csv"
PathToProcessedData <- "./Data/full_processed_CMdata.csv"
site_csv <- read.csv(PathToSiteData, na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
proc_csv <- read.csv(PathToProcessedData, header = T, sep = ",", na.strings = "NA")

#rename
site_dat <- site_csv
proc_dat <- proc_csv

#format times as dates
site_dat$TIMESTAMP_END <- ymd_hm(site_dat$TIMESTAMP_END)
proc_dat$Time <- ymd_hms(proc_dat$Time)
#make sure all timestamps are in half hour format
round_to_HH <- function(x) {
  #extract min and sec values from dates
  min <- minute(x)
  sec <- second(x)
  #calc total seconds: convert min vals to seconds and add sec val
  sec_total <- min * 60 + sec
  #find nearest HH
  HH <- round(sec_total / (30 * 60)) * 30 * 60
  
  #get new vals to closest calculated HH
  clean_timestamp <- floor_date(x, "hour") + seconds_to_period(HH)
  
  return(clean_timestamp)
}

proc_dat <- proc_dat %>%
  mutate(Time = round_to_HH(Time),
         HH_group_ID = NULL) %>%
  rename(TIMESTAMP_END = Time) 
  
combd_dat <- merge(site_dat, proc_dat, by = "TIMESTAMP_END")
#write.csv(combd_dat, "./Data/combined_CMdata.csv")
#===============================================================================
#============================prep data for ffp calc=============================
#===============================================================================
combd_dat <- read.csv("./Data/combined_CMdata.csv")
#format df for ffp calc
meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

all_clim_dat <- combd_dat %>%
  summarize(
    yyyy = year(TIMESTAMP_END),
    mm = month(TIMESTAMP_END),
    day = day(TIMESTAMP_END),
    HH_UTC = hour(TIMESTAMP_END),
    MM = minute(TIMESTAMP_END),
    zm = meas_h - d,
    z0 = NaN,
    umean = mean(WS_1_1_1, na.rm = TRUE),
    h = bound_h,
    ol = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigmav = sigma_v,
    ustar = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/ol)
all_clim_dat <- all_clim_dat%>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)%>%
  filter(yyyy %in% 2015:2019)

#split into separate wind directions bins of 10 degrees
deg_int <- seq(0, 360, by = 10)
split_dat <- split(all_clim_dat, cut(all_clim_dat$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))

#filter by season/month for ffp input data
dec_split_dat <- lapply(split_dat, function(df) {
  df %>% filter(mm == 12)
})
