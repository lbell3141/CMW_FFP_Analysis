# Load libraries
library(dplyr)
library(lubridate)
library(purrr)
library(plantecophys)
library(ggplot2)
library(viridis)
library(patchwork)
library(tidyr)
library(scales)


rf_pred <- readRDS("./Data/30minpredictionsRF.RDS")
rf_df <- bind_rows(rf_pred, .id = "mm")

# Load and process flux data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

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
    reco = RECO_PI,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

# Bin wind direction into 20° intervals
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

# Prepare flux data

gpp_dat <- dat_voi %>%
  select(yyyy, mm, gpp, dir_group) %>%
  rename(observed = gpp) %>%
  filter(!is.na(observed))

gpp_yr <- merge(rf_df, gpp_dat, by = c("mm", "observed", "dir_group")) %>%
  filter(#yyyy == 2021, 
         mm %in% 6:9) %>%
  mutate(residual = predicted - observed)



dat_flux <- dat_voi %>%
  select(yyyy,mm, dir_group, nee, gpp, reco) %>%
  filter(mm %in% 6:9) %>%
  filter(if_all(everything(), ~ !is.na(.)))

avg_flux <- dat_flux %>%
  group_by(mm, dir_group) %>%
  summarise(across(c(gpp, nee, reco), mean, na.rm = TRUE), .groups = "drop")

flux_var_cv <- avg_flux %>%
  group_by(mm) %>%
  summarise(
    GPP = (sd(gpp, na.rm = TRUE) / mean(gpp, na.rm = TRUE)) * 100,
    NEE = (sd(nee, na.rm = TRUE) / mean(nee, na.rm = TRUE)) * 100,
    Respiration = (sd(reco, na.rm = TRUE) / mean(reco, na.rm = TRUE)) * 100,
    .groups = "drop"
  )%>%
  mutate(mm = as.numeric(mm))

pred_cv <- gpp_yr %>%
  group_by(mm, dir_group) %>%
  summarise(
    predicted = mean(predicted, na.rm = TRUE),
    residual = mean(predicted - observed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(mm) %>%
  summarise(
    Predicted = (sd(predicted, na.rm = TRUE) / mean(predicted, na.rm = TRUE)) * 100,
    Residual = (sd(residual, na.rm = TRUE) / mean(residual, na.rm = TRUE)) * 100,
    .groups = "drop"
  )%>%
  mutate(mm = as.numeric(mm))


directional_cv <- flux_var_cv %>%
  left_join(pred_cv, by = "mm")

cv_long <- directional_cv %>%
  pivot_longer(cols = c(GPP, NEE, Respiration, Predicted, Residual),
               names_to = "variable", values_to = "cv") %>%
  mutate(variable = factor(variable, levels = c("NEE", "GPP", "Respiration", "Predicted", "Residual")))

ggplot(cv_long, aes(x = mm, y = abs(cv), color = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "%CV", color = "") +
  theme_classic(base_size = 14)


#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
# with z score doesnt work bc the center will be around 0, so dividing by 
#the mean for CV is pointless
# Load libraries
library(dplyr)
library(lubridate)
library(purrr)
library(plantecophys)
library(ggplot2)
library(viridis)
library(patchwork)
library(tidyr)
library(scales)

# ------------------------------------------------------------------------------
# Load random forest predicted GPP
rf_pred <- readRDS("./Data/30minpredictionsRF.RDS")
rf_df <- bind_rows(rf_pred, .id = "mm")

# ------------------------------------------------------------------------------
# Load and process flux data
dat_file <- read.csv("./Data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", skip = 2) %>%
  mutate(TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)))

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
    reco = RECO_PI,
    gpp = GPP_PI,
    precip = P,
    rel_h = RH_1_1_1,
    VPD = RHtoVPD(RH_1_1_1, TA_1_1_1, PA),
    ppfd = PPFD_IN_PI_F,
    le = LE,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)

# Bin wind direction into 20° intervals
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))

# ------------------------------------------------------------------------------
# Prepare GPP data for residuals and predictions
gpp_dat <- dat_voi %>%
  select(yyyy, mm, gpp, dir_group) %>%
  rename(observed = gpp) %>%
  filter(!is.na(observed))

gpp_yr <- merge(rf_df, gpp_dat, by = c("mm", "observed", "dir_group")) %>%
  filter(mm %in% 6:9) %>%
  mutate(residual = predicted - observed)

# ------------------------------------------------------------------------------
# Prepare flux data and compute z-scores
dat_flux <- dat_voi %>%
  select(yyyy, mm, dir_group, nee, gpp, reco) %>%
  filter(mm %in% 6:9) %>%
  filter(if_all(everything(), ~ !is.na(.)))

avg_flux <- dat_flux %>%
  group_by(mm, dir_group) %>%
  summarise(across(c(gpp, nee, reco), mean, na.rm = TRUE), .groups = "drop")

flux_z <- avg_flux %>%
  group_by(mm) %>%
  mutate(
    gpp_mean = mean(gpp, na.rm = TRUE),
    gpp_sd = sd(gpp, na.rm = TRUE),
    gpp_z = (gpp - gpp_mean) / gpp_sd,
    
    nee_mean = mean(nee, na.rm = TRUE),
    nee_sd = sd(nee, na.rm = TRUE),
    nee_z = (nee - nee_mean) / nee_sd,
    
    reco_mean = mean(reco, na.rm = TRUE),
    reco_sd = sd(reco, na.rm = TRUE),
    reco_z = (reco - reco_mean) / reco_sd
  ) %>%
  ungroup()


# Compute average z-scores by month and direction
avg_flux_z <- flux_z %>%
  group_by(mm) %>%
  summarise(across(c(gpp_z, nee_z, reco_z), mean, na.rm = TRUE), .groups = "drop")

# Compute %CV from z-scores
flux_var_cv_z <- avg_flux_z %>%
  group_by(mm) %>%
  summarise(
    GPP = (sd(gpp_z, na.rm = TRUE) / mean(gpp_z, na.rm = TRUE)) * 100,
    NEE = (sd(nee_z, na.rm = TRUE) / mean(nee_z, na.rm = TRUE)) * 100,
    Respiration = (sd(reco_z, na.rm = TRUE) / mean(reco_z, na.rm = TRUE)) * 100,
    .groups = "drop"
  ) %>%
  mutate(mm = as.numeric(mm))