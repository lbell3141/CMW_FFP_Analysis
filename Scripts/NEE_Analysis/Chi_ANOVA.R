#plot NEE by month
#Code to plot figure 1
library(dplyr)
library(lubridate)
library(plantecophys)
library(stringr)
library(purrr)
library(tidyverse)
library(patchwork)
library(broom) 
#===============================================================================
#Dataframe Prep
#===============================================================================

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

# Split into direction windows
deg_int <- seq(0, 360, by = 20)
deg_labels <- seq(20, 360, by = 20)
dat_voi <- dat_voi %>%
  mutate(dir_group = cut(wind_dir, breaks = deg_int, include.lowest = TRUE, labels = deg_labels))%>%
  select(mm, nee, wind_dir, dir_group)

dat_voi <- dat_voi %>%
  mutate(
    mm = factor(mm, levels = 1:12, labels = month.abb),
    dir_group = factor(dir_group)
  )

contingency_tbl <- table(dat_voi$mm, dat_voi$dir_group)

chi_result <- chisq.test(contingency_tbl)
print(chi_result)
#wind direction is dependent on month p<<0.05

#===============================================================================
#ANOVAs for significant difference by direction
#===============================================================================
# Make sure direction is a factor
dat_prep <- dat_voi %>%
  mutate(mm = factor(mm, levels = 1:12, labels = month.abb),
         direction = cut(wind_dir, breaks = seq(0, 360, by = 20), include.lowest = TRUE))
  # select(mm, direction, nee)

# Drop NAs
dat_anova <- dat_prep %>%
  filter(if_all(everything(), ~!is.na(.)))


# Run ANOVA per month with multiple observations per direction
anova_results <- dat_anova %>%
  group_by(mm) %>%
  do(tidy(aov(nee ~ direction, data = .))) %>%
  ungroup()

# Filter for direction effect
direction_anova <- anova_results %>%
  filter(term == "direction") %>%
  select(mm, df, statistic, p.value)

# View
print(direction_anova)

# Add significance stars
direction_anova <- direction_anova %>%
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Plot
ggplot(direction_anova, aes(x = mm, y = statistic)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = signif), vjust = -0.5, size = 5) +
  labs(
    title = "ANOVA F-statistics: NEE ~ Wind Direction (by Month)",
    x = "Month",
    y = "F-statistic",
    caption = "* p < 0.05, ** p < 0.01, *** p < 0.001"
  ) +
  theme_minimal()

