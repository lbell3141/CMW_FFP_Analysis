library(lubridate)
library(dplyr)
library(ggplot2)

dat <- read.csv("./Data/combined_CMdata.csv", header = T)
dat$TIMESTAMP_END <- ymd_hms(as.character(dat$TIMESTAMP_END))

#===============================================================================
#===============================gpp climatology=================================
#===============================================================================

clim_dat <- dat %>%
  mutate(mm = month(TIMESTAMP_END),
         day = day(TIMESTAMP_END),
         HH_UTC = hour(TIMESTAMP_END)) %>%
  filter(HH_UTC %in% 8:15)%>%
  group_by(mm, day) %>%
  summarize(WD = median(WD_1_1_1, na.rm = T),
            WD_sd = sd(WD_1_1_1, na.rm = T),
            GPP = mean(GPP_PI, na.rm = T),
            GPP_sd = sd(GPP_PI, na.rm = TRUE)
            )%>%
  ungroup()%>%
  mutate(doy = seq(1, n(), by = 1))

ggplot(clim_dat, aes(x = doy, y = GPP)) +
  geom_line() +
  geom_ribbon(aes(ymin = GPP - GPP_sd, ymax = GPP + GPP_sd), alpha = 0.1) +
  theme_minimal() +
  labs(title = "GPP Climatology: US-CMW",
       x = "Day of Year",
       y = "GPP")

#===============================================================================
#================================WD climatology=================================
#===============================================================================

ggplot(clim_dat, aes(x = doy, y = WD)) +
  geom_point() +
  geom_ribbon(aes(ymin = WD - WD_sd, ymax = WD + WD_sd), alpha = 0.1) +
  theme_minimal() +
  labs(title = "Wind Direction Climatology: US-CMW",
       x = "Day of Year",
       y = "Wind Direction")
