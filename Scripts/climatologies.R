library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)

dat <- read.csv("./Data/combined_CMdata.csv", header = T)
dat$TIMESTAMP_END <- ymd_hms(as.character(dat$TIMESTAMP_END))

#===============================================================================
#===============================gpp climatology=================================
#===============================================================================

clim_dat <- dat %>%
  mutate(mm = month(TIMESTAMP_END),
         day = day(TIMESTAMP_END),
         HH_UTC = hour(TIMESTAMP_END)) %>%
  filter(HH_UTC %in% 8:17)%>%
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

#===============================================================================
#================================Diurnal WD Variation===========================
#===============================================================================
dat$TIMESTAMP_END <- ymd_hms(as.character(dat$TIMESTAMP_END))


deg_int <- seq(0, 360, by = 30)
labels <- labels <- deg_int[-1] 
#split_dat <- split(dat, cut(dat$WD_1_1_1, deg_int,labels = labels))
dat_grouped <- dat%>%
  mutate(mm = month(TIMESTAMP_END),
         day = day(TIMESTAMP_END),
         HH_UTC = hour(TIMESTAMP_END),
         doy = yday(TIMESTAMP_END))%>%
  select(mm, day, doy, HH_UTC, WD_1_1_1)

dat_grouped <- dat_grouped %>%
  filter(HH_UTC %in% 8:17)
dat_grouped <- dat_grouped%>%
  mutate(WD_Group = cut(dat_grouped$WD_1_1_1, deg_int, labels = labels))

#for bar plot
ggplot(dat_grouped, aes(x = HH_UTC)) +
  geom_bar(aes(fill = WD_Group), position = "dodge") +
  labs(title = "Diurnal Wind Direction Frequency",
       x = "Time of Day",
       y = "Number of Observations",
       fill = "WD Group") +
  theme_minimal()

#for line plot
summary_data <- dat_grouped %>%
  group_by(HH_UTC, WD_Group) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(summary_data, aes(x = HH_UTC, y = count, color = WD_Group, group = WD_Group)) +
  geom_line(size = 1.3) +
  labs(title = "Diurnal Wind Direction Frequency",
       x = "Time of Day",
       y = "Number of Observations",
       color = "WD Group") +
  theme_minimal()

#===============================================================================
#================================Annual WD Variation============================
#===============================================================================
dat$TIMESTAMP_END <- ymd_hms(as.character(dat$TIMESTAMP_END))


deg_int <- seq(0, 360, by = 45)
labels <- labels <- deg_int[-1] 
#split_dat <- split(dat, cut(dat$WD_1_1_1, deg_int,labels = labels))
dat_grouped <- dat%>%
  mutate(mm = month(TIMESTAMP_END),
         day = day(TIMESTAMP_END),
         HH_UTC = hour(TIMESTAMP_END),
         doy = yday(TIMESTAMP_END))%>%
  select(mm, day, doy, HH_UTC, WD_1_1_1)

dat_grouped <- dat_grouped %>%
  filter(HH_UTC %in% 8:17)
dat_grouped <- dat_grouped%>%
  mutate(WD_Group = cut(dat_grouped$WD_1_1_1, deg_int, labels = labels))



summary_data <- dat_grouped %>%
  group_by(doy, WD_Group) %>%
  summarise(count = n()) %>%
  ungroup()

# Create a bar plot of number of observations vs. day and month using WD_Group as color
ggplot(summary_data, aes(x = factor(doy), y = count, fill = WD_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Wind Direction Frequency Throughout the Year",
       x = "Day of Year",
       y = "Number of Observations",
       fill = "WD Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
summary_data <- dat_grouped %>%
  group_by(doy, WD_Group) %>%
  summarise(count = n()) %>%
  ungroup()
ggplot(summary_data, aes(x = doy, y = count, color = WD_Group, group = WD_Group)) +
  geom_line(size = 0.8) +  # Adjust line thickness as needed
  labs(title = "Wind Direction Frequency Throughout the Year",
       x = "Day of Year",
       y = "Number of Observations",
       color = "WD Group") +
  theme_minimal()


summary_data <- summary_data %>%
  group_by(WD_Group) %>%
  mutate(moving_avg = rollmean(count, k = 30, fill = NA, align = "center")) %>%
  ungroup()

# Create a line plot of moving average of number of observations vs. day of year (doy) using WD_Group as color
ggplot(summary_data, aes(x = doy, y = moving_avg, color = WD_Group, group = WD_Group)) +
  geom_line(size = 0.8) +  # Adjust line thickness as needed
  labs(title = "Annual Wind Direction Frequency",
       x = "Day of Year",
       y = "Moving Average of Observations",
       color = "WD Group") +
  theme_minimal()
