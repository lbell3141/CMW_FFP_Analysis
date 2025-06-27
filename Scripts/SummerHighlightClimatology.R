#highlight summer on GPP climatology 

library(dplyr)
library(lubridate)
library(plantecophys)
library(ggplot2)
library(zoo)

# Load data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

# Create primary dataframe
dat_voi <- dat_file %>%
  transmute(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    nee = NEE_PI,
    reco = RECO_PI,
    gpp = GPP_PI,
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

plot_dat <- dat_voi %>%
  group_by(doy) %>%
  summarise(
    avg_gpp = mean(gpp, na.rm = TRUE),
    se_gpp  = sd(gpp, na.rm = TRUE)# / sqrt(sum(!is.na(gpp)))
  )
plot_dat <- plot_dat %>%
  arrange(doy) %>%
  mutate(gpp_roll = rollmean(avg_gpp, k = 7, fill = NA, align = "center"))

climatology_plot <- ggplot(plot_dat, aes(x = doy)) +
  
  geom_ribbon(aes(ymin = avg_gpp - se_gpp, ymax = avg_gpp + se_gpp), 
              fill = "grey", alpha = 0.4) +
  annotate("rect", xmin = 152, xmax = 273, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.3) +
  geom_line(aes(y = avg_gpp), color = "black", size = 0.8) +
  #geom_line(aes(y = gpp_roll), color = "black", size = 1) +
  labs(
    x = "",
    y = "",
    title = "")+
  theme_classic()+
  theme(axis.text = element_text(size = 15))

climatology_plot


