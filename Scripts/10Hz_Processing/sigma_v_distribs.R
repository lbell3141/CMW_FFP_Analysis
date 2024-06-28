#comparing new and old distributions of sigma_v


library(lubridate)
library(dplyr)
library(ggplot2)

#-------------------------------------------------------------------------------
#previous s_v

dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#create dataframe
dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    wind_sp = WS_1_1_1,
    zm = 14 - (2/3*14),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    wind_dir = WD_1_1_1,
    u_star = USTAR,
    test = zm/L,
    #adding associated fluxes
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.2)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, sigma_v, wind_sp, L, wind_dir)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

#has many, many outliers
old_sv <- data.frame(
  sigma_v = dat_voi$sigma_v[dat_voi$sigma_v < 6],
  group = rep("old_sigma_v", times = length(dat_voi$sigma_v[dat_voi$sigma_v < 6])))

ggplot(old_sv, aes(x = group, y = sigma_v)) +
  geom_jitter(color = "grey", width = 0.1, alpha = 0.8) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Distribution of Sigma_V from Ameriflux HH Data", x = "", y = "Sigma_V") +
  theme_minimal() +
  theme(legend.position = "none")
#-------------------------------------------------------------------------------

#new sv:
hz_dat <- read.csv("./Data/combined_CMdata.csv", header = T)
hz_dat$TIMESTAMP_END <- ymd_hms(as.character(hz_dat$TIMESTAMP_END))

all_clim_dat <- hz_dat %>%
  mutate(
    yyyy = year(TIMESTAMP_END),
    mm = month(TIMESTAMP_END),
    day = day(TIMESTAMP_END),
    HH_UTC = hour(TIMESTAMP_END),
    MM = minute(TIMESTAMP_END),
    zm = 14 - (2/3*14),
    z0 = NaN,
    umean = mean(WS_1_1_1, na.rm = TRUE),
    ol = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigmav = sigma_v,
    ustar = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/ol
  ) %>%
  filter(test >= -15.5)%>%
  filter(ustar > 0.2)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, z0, umean,  ol, sigmav, ustar, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)

#two outliers causing squished graph, filter for values less than 15
new_sv <- data.frame(
  sigma_v = all_clim_dat$sigmav[all_clim_dat$sigmav < 15],
  group = rep("new_sigma_v", times = length(all_clim_dat$sigmav[all_clim_dat$sigmav < 15])))
                     
ggplot(new_sv, aes(x = group, y = sigma_v)) +
  geom_boxplot() +
  labs(title = "Distribution of Sigma_V from 10Hz Data", x = "", y = "")+
  theme_minimal()

ggplot(new_sv, aes(x = group, y = sigma_v)) +
  geom_jitter(color = "grey", width = 0.1, alpha = 0.8) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Distribution of Sigma_V from 10Hz Data", x = "", y = "Sigma_V") +
  theme_minimal() +
  theme(legend.position = "none")

#-------------------------------------------------------------------------------
#plot distributions with boxplots
#make plotting df
#plot_dat <- data.frame(
#  sigma_v = c(new_sv, old_sv),
#  group = rep(c("new_sv", "old_sv"), times = c(length(new_sv), length(old_sv)))
#)

#make plot
#ggplot(plot_dat, aes(x = group, y = sigma_v, fill = group)) +
#  geom_boxplot(alpha = 0.7) +
#  labs(title = "Boxplot of new_sv and old_sv", x = "Group", y = "Value") +
#  theme_minimal()

par(mfrow = c(1,1))
ggplot(new_sv, aes(x = group, y = sigma_v)) +
  geom_jitter(color = "grey", width = 0.1, alpha = 0.8) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Distribution of Sigma_V from 10Hz Data", x = "", y = "Sigma_V") +
  theme_minimal() +
  theme(legend.position = "none")
ggplot(old_sv, aes(x = group, y = sigma_v)) +
  geom_jitter(color = "grey", width = 0.1, alpha = 0.8) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Distribution of Sigma_V from Ameriflux HH Data", x = "", y = "Sigma_V") +
  theme_minimal() +
  theme(legend.position = "none")
