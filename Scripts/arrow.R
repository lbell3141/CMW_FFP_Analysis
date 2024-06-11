library(arrow)
library(tictoc)
library(dplyr)

tic()

hf_dataset <-
  arrow::open_tsv_dataset(
    sources = here::here(
      "Data/High_Freq_Data",
      "Jan2019.txt"))


collect()

toc()




#load relevant libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(plantecophys)
library(arrow)

#set data paths
pathtoHFdata <- "./Data/High_Freq_Data/Jan2019.txt"
pathtoHHdata <- "./Data/AMF_US-CMW_BASE_HH_2-5.csv"

#===============================================================================
#=====================Aggregate 10Hz to 3s observations=========================
#===============================================================================
#load high frequency data (10Hz obs)
hf_data <- read.table(pathtoHFdata, header = TRUE, sep = "\t")
#format timestamp
hf_data$TimeStamp <- ymd_hms(hf_data$TimeStamp)
#split into groups of 30 observations 
#add row number
hf_data <- hf_data %>%
  mutate(row_ID = row_number())
#adding dates by brute force bc something is wonky when I do it in the pipe w the rest of the values
#new column for dates in order to keep hm info in timestamp column
hf_data <- hf_data %>%
  mutate(Date = as.Date(TimeStamp))
group_date <- hf_data %>%
  # "%/% rounds down to the whole number in the quotient
  group_by(group = (row_ID - 1) %/% 30) %>%
  summarise(Date = first(Date))

#add date to real df
ag_3s_df <- hf_data %>%
  group_by(group = (row_ID - 1) %/% 30) %>%
  summarise(
    Date = first(Date),
    Time = max(TimeStamp, na.rm = TRUE),
    Ux = mean(Ux, na.rm = TRUE),
    Uy = mean(Uy, na.rm = TRUE),
    Uz = mean(uZ, na.rm = TRUE)
  )

#===============================================================================
#==================Calculate theta_i from vector components=====================
#===============================================================================
#use x and y components of wind direction and pythagorean thm to calc magnitude (wind speed)
ag_3s_df$u_i <- ((((ag_3s_df$Ux)^2) + ((ag_3s_df$Uy)^2)))^(1/2)
#atan2 returns quadrant-sensitive outputs (ie 0 to pi to -pi instead of just 0 to pi)
ag_3s_df$theta_stand <- atan2(ag_3s_df$Ux, ag_3s_df$Uy) 

#convert to compass degrees from standard plane radians
radians_to_compass <- function(angle_rad) {
  angle_deg <- angle_rad * (180 / pi)  
  compass_angle <- (90 - angle_deg) %% 360  
  return(compass_angle)
}

ag_3s_df$theta_cc <- radians_to_compass(ag_3s_df$theta_stand)

#===============================================================================
#====================Calculate half-hourly theta_v_bar==========================
#===============================================================================

bar_df <- ag_3s_df

#group by half hour (every 600 observations) and calc variables
bar_df_HH <- bar_df %>%
  group_by(group = group %/% 600) %>%
  summarise(
    u_i_bar = (1 / n()) * sum(u_i),
    u_x_bar = (1 / n()) * sum(u_i * sin(theta_stand - pi)),
    u_y_bar = (1 / n()) * sum(u_i * cos(theta_stand - pi)),
    theta_v_bar = atan2(u_x_bar, u_y_bar) + pi,
    sigma_v_sqd = (1 / (n() - 1)) * sum(u_i * sin(theta_v_bar - u_i)),
    sigma_v = sqrt(abs(sigma_v_sqd))
  )

#check relationship
plot(bar_df_HH$u_i_bar, bar_df_HH$sigma_v)
