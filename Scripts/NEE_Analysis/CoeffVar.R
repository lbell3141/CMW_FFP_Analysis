# Load required libraries
library(dplyr)
library(lubridate)
library(plantecophys)
library(ggplot2)
library(patchwork)
library(tidyr)

#-------------------------
# Data Preprocessing
#-------------------------

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
    wind_sp = WS_1_1_1,
    temp_atmos = TA_1_1_1,
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    nee = NEE_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

# Create wind direction bins
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

#-------------------------
# Step 1: Calculate bin-level means per month
#-------------------------

vars_to_cv <- c("wind_sp", "temp_atmos", "nee", 
                "rel_h", "ppfd", "swc"
                #,"precip"
                )

bin_means <- dat_voi %>%
  group_by(mm, dir_group) %>%
  summarise(across(all_of(vars_to_cv), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"), .groups = "drop")

#-------------------------
# Step 2: Compute CV across bin means for each month
#-------------------------

cv_by_month <- bin_means %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value") %>%
  group_by(mm, variable) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    cv = sd_val / mean_val,
    .groups = "drop"
  ) %>%
  mutate(
    variable = gsub("mean_", "", variable),
    mm = factor(mm, levels = 1:12, labels = month.abb)
  )

#-------------------------
# Plotting CV by Month
#-------------------------
ggplot(cv_by_month, aes(x = mm, y = cv)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  facet_wrap(~ variable, scales = "fixed", 
             labeller = labeller(variable = c(
               "nee" = "NEE",
               "ppfd" = "PPFD",
               #"precip" = "Precipitation",
               "rel_h" = "Relative Humidity",
               "swc" = "Soil Moisture",
               "temp_atmos" = "Air Temperature",
               "wind_sp" = "Wind Speed"
             ))) +
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = "Coefficient of Variation"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12),  # Increase size of month labels
    axis.text.y = element_text(size = 16),  # Increase size of y-axis labels (numbers)
    axis.title.x = element_text(size = 16),  # Increase x-axis title size
    axis.title.y = element_text(size = 16),  # Increase y-axis title size
    plot.title = element_text(size = 16, face = "bold"),  # Increase plot title size and make it bold
    strip.text = element_text(size = 16)  # Increase facet labels size
  )



